package com.colorcloud.trident;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import storm.trident.TridentState;
import storm.trident.TridentTopology;
import storm.trident.operation.Aggregator;
import storm.trident.operation.BaseAggregator;
import storm.trident.operation.Filter;
import storm.trident.operation.Function;
import storm.trident.operation.TridentCollector;
import storm.trident.operation.TridentOperationContext;
import storm.trident.state.State;
import storm.trident.state.StateFactory;
import storm.trident.tuple.TridentTuple;
import backtype.storm.Config;
import backtype.storm.LocalCluster;
import backtype.storm.LocalDRPC;
import backtype.storm.generated.StormTopology;
import backtype.storm.task.IMetricsContext;
import backtype.storm.tuple.Fields;
import backtype.storm.tuple.Values;

import com.colorcloud.trident.storage.JedisDB;

/**
 * Build a topology with drpc spout, and streaming lines of text(log) into drpc spout, and get back
 * the processing result in an added field in tuple. 
 * groupBy. GroupBy creates a "grouped stream" which means that subsequent aggregators can
 * only affect Tuples within the group. GroupBy must always be followed by an aggregator it knows how to aggregate group. 
 * Because we are aggregating groups, we don't need to produce a hashmap for the per-location counts (as opposed to {@link BatchAggregate} 
 * and we can use the simple Count() aggregator.
 */
public class DrpcStream {
	public static final String TOPNAME = "DRPC_STREAM";
	public static final Logger logger = LogManager.getLogger(DrpcStream.class.getName());
	public static final String TAG = "DRPC";
	
	public static void log(String...strings){
		System.out.println(Arrays.toString(strings));
	}
	
	/**
	 * first, the text tokenize and clean fn
	 */
	//public static class TextProcessor extends BaseFunction{
	public static class TextProcessor implements Function {
		private static final long serialVersionUID = 1L;
		public static final String TAG = "TextProc";
		public static final Logger logger = LogManager.getLogger(TextProcessor.class.getName());
		
		@Override
		public void prepare(Map conf, TridentOperationContext context) {
		}

		@Override
		public void execute(TridentTuple tuple, TridentCollector collector) {
			String v = (String) tuple.getValue(0);
			logger.info(TAG, v);
			String loc = extractLocation(v);
			System.out.println("location : " + loc);
			collector.emit(new Values(loc, v));  // emits location field first, then cleartext
		}

		@Override
		public void cleanup() {
			// TODO Auto-generated method stub
		}
		
		private String extractLocation(String text) {
			String loc = " XXX ";
			Pattern locpattern = Pattern.compile("(.*) (: \\w+ :)( \\d{4}-\\d{2}-\\d{2}) (.*)", Pattern.CASE_INSENSITIVE);
			try{
				Matcher m = locpattern.matcher(text);
				if(m.matches()){
					loc = m.group(2);   // group starts from 1
				}
			}catch(Exception e){
			}
			
			loc = loc.substring(1,loc.length()-2).trim();  // exclusive the end index
			return loc;
		}
	}
	
	/**
	 * state stores intermediate processing result.
	 */
	public static class LocationStateFactory implements StateFactory {		
		public LocationStateFactory() {
		}

		@Override
		public State makeState(Map conf, IMetricsContext metrics,
				int partitionIndex, int numPartitions) {
			return new LocationState(partitionIndex, numPartitions);
		}
	}

	
	/**
	 * batch processing treats every batch process as a transaction.
	 */
	public static class GroupTotal extends BaseAggregator<Map<String, Long>> {
		public static final Logger logger = LogManager.getLogger(GroupTotal.class.getName());
		private static final long serialVersionUID = 5747067380651287870L;
		private static final String TAG = "GroupTotal :";
		Map<String, Long> result = new HashMap<String, Long>();
		protected JedisDB jedis;
		int batchcnt = 0;
		
		/**
		 * prepare only called once when creating object. Put any initiation code here.
		 */
		@Override
	    public void prepare(Map conf, TridentOperationContext context) {
			logger.info(TAG, " prepare called, create jedis ");
			jedis = new JedisDB();
	    }
		
		/**
		 * init is called upon object instantiation, ret the state to store the aggregation.
		 */
		@Override
		public Map<String, Long> init(Object batchId, TridentCollector collector) {
			logger.info(TAG, "init : batchId : " + batchId); 
			batchcnt = 0;
			return result;
		}
	
		/**
		 * aggregate called upon every tuple inside the batch.
		 * @param Map<String, Long> aggregate fn was given the global state so it can update global for each tuple.
		 * @param tuple  the current tuple to be processed
		 * @param collector the collector to emit the processed tuple
		 */
		@Override
		public void aggregate(Map<String, Long> val, TridentTuple tuple, TridentCollector collector) {
			logger.info(TAG,  "aggregate :" + tuple);
			
			String loc = tuple.getString(3);
			//long cnt = tuple.getLong(1);
			batchcnt += 1;
			long cnt = batchcnt;
			long totcnt = 0;
			if( val.get(loc) != null ){
				totcnt = val.get(loc);
			}
			val.put(loc, cnt+totcnt);
			List<Object> v = tuple.getValues();
			//v.add(totcnt);
			collector.emit(new Values(cnt, totcnt));
		}
	
		/**
		 * complete called after done with every batch. store summary map to redis.
		 */
		@Override
		public void complete(Map<String, Long> val, TridentCollector collector) {
			System.out.println(TAG + "complete :" + val);
			jedis.storeMap("loc-cnt", val);
			//collector.emit(new Values(val));
		}	         
	}
	
	/**
	 * this function will be serialized and distributed to all nodes to run. 
	 * every member inside must be serializable in order to be distributed.
	 */
	static class DBWriteBolt implements Filter, Function {
		private static final long serialVersionUID = 1L;
		private static final String TAG = "DBWriteBolt :";
		protected JedisDB jedis;
		
		public DBWriteBolt() {
			System.out.println(TAG + " constructor: ");
		}
		
		/**
		 * prepare only called once for filter function at the time of init.
		 */
		@Override
		public void prepare(Map conf, TridentOperationContext context) {
			System.out.println(TAG + " prepare called, init db connection : ");
			jedis = new JedisDB();   // use default configuration.
		}

		@Override
		public boolean isKeep(TridentTuple tuple) {
			System.out.println(TAG + " iskeep : ");
			return true;
		}

		@Override
		public void cleanup() {
			// disconnect db connection
		}

		@Override
		public void execute(TridentTuple tuple, TridentCollector collector) {
			System.out.println(TAG + " execute : " + tuple.get(0));  // selected location as first ele in tuple
			Values v = new Values(tuple.get(0));
			jedis.rpush("tweetloc", (String)tuple.get(0));  // save location to redis
			collector.emit(v);
		}
	}

	
	public static StormTopology buildDRPCTopology(LocalDRPC drpc) throws IOException {
		FakeTweetsBatchSpout spout = new FakeTweetsBatchSpout(100);  // create spout as the source for the topology
		Function stateStore = new DBWriteBolt();
		Aggregator<Map<String, Long>> grpTotal = new GroupTotal();
		storm.trident.operation.builtin.Count counter = new storm.trident.operation.builtin.Count(); // default counter aggregator
		TridentTopology topology = new TridentTopology();
		
		// define text processing function
		Function textFn = new TextProcessor();
		TridentState locState = topology.newStaticState(new LocationStateFactory());
		LocationStateQuery stateQuery = new LocationStateQuery();
		
		
		// grouped stream, after aggregation, only contains grouping key and other fields emitted from aggregator. 
		
		topology.newDRPCStream(TOPNAME, drpc)	// spout src from drpc execute, data wrapped into args field.
			.each(new Fields("args"), textFn, new Fields("location", "cleartext"))	// got clear text out of text processing
			.broadcast()
			.stateQuery(locState, new Fields("location", "cleartext"), stateQuery, new Fields("locationx", "locationTotal"))
			.project(new Fields("locationx", "locationTotal"));  // ret two fields
		
		return topology.build();
	}

	public static void streamText(LocalDRPC drpc, FakeTweetsBatchSpout spout) {
		String tweet, result;
		while((tweet = spout.getNextTweetString()) != null){
			log("streamText src:" + tweet);
			// multiple fields packed into one ret string value.
			result = drpc.execute(TOPNAME, tweet);
			log("streamText reslt:" + result);
		}
	}
	
	public static void main(String[] args) throws Exception {
		Config conf = new Config();
		LocalDRPC drpc = new LocalDRPC();
		LocalCluster cluster = new LocalCluster();
		FakeTweetsBatchSpout spout = new FakeTweetsBatchSpout(100);  // create spout as the source for the topology
		spout.open(null, null);
		
		//cluster.submitTopology(TOPNAME, conf, buildTopology(drpc));
		cluster.submitTopology(TOPNAME, conf, buildDRPCTopology(drpc));
		
		logger.info(" >>>>>>>>>> done cluster submit topology");
		Thread.sleep(5000); // give it some time for top to setup.
		
		streamText(drpc, spout);
//		String result = drpc.execute(TOPNAME, "hello world");
//		System.out.println(">>>>>>>>>> drpc result :" + result);
//		result = drpc.execute(TOPNAME, "hello world from world");
//		System.out.println(">>>>>>>>>> drpc result :" + result);
	}
}
