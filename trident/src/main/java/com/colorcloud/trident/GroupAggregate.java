package com.colorcloud.trident;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import storm.trident.Stream;
import storm.trident.TridentTopology;
import storm.trident.operation.Aggregator;
import storm.trident.operation.BaseAggregator;
import storm.trident.operation.Filter;
import storm.trident.operation.Function;
import storm.trident.operation.TridentCollector;
import storm.trident.operation.TridentOperationContext;
import storm.trident.tuple.TridentTuple;
import backtype.storm.Config;
import backtype.storm.LocalCluster;
import backtype.storm.LocalDRPC;
import backtype.storm.generated.StormTopology;
import backtype.storm.tuple.Fields;
import backtype.storm.tuple.Values;

import com.colorcloud.trident.storage.JedisDB;

/**
 * This example illustrates the usage of groupBy. GroupBy creates a "grouped stream" which means that subsequent aggregators
 * will only affect Tuples within a group. GroupBy must always be followed by an aggregator. 
 * Because we are aggregating groups, we don't need to produce a hashmap for the per-location counts (as opposed to {@link BatchAggregate} 
 * and we can use the simple Count() aggregator.
 */
public class GroupAggregate {
	/**
	 * batch processing treats every batch process as a transaction.
	 */
	public static class GroupTotal extends BaseAggregator<Map<String, Long>> {		
		private static final long serialVersionUID = 5747067380651287870L;
		private static final String TAG = "GroupTotal :";
		Map<String, Long> result = new HashMap<String, Long>();
		protected JedisDB jedis;
		
		/**
		 * prepare only called once when creating object. Put any initiation code here.
		 */
		@Override
	    public void prepare(Map conf, TridentOperationContext context) {
			System.out.println(TAG + " prepare :");
			jedis = new JedisDB();
	    }
		
		/**
		 * init is called upon object instantiation.
		 */
		@Override
		public Map<String, Long> init(Object batchId, TridentCollector collector) {
			System.out.println(TAG + "init : batchId : " + batchId); 
			return result;
		}
	
		/**
		 * aggregate called upon every tuple inside the batch, update counter map.
		 */
		@Override
		public void aggregate(Map<String, Long> val, TridentTuple tuple, TridentCollector collector) {
			System.out.println(TAG + "aggregate :" + tuple);
			String loc = tuple.getString(0);
			long cnt = tuple.getLong(1);
			long totcnt = 0;
			if( val.get(loc) != null ){
				totcnt = val.get(loc);
			}
			val.put(loc, cnt+totcnt);
		}
	
		/**
		 * complete called after done with every batch.
		 */
		@Override
		public void complete(Map<String, Long> val, TridentCollector collector) {
			System.out.println(TAG + "complete :" + val);
			jedis.storeMap("loc-cnt", val);
			collector.emit(new Values(val));
		}	         
	}
	
	/**
	 * this function will be serialized and distributed to all nodes to run. 
	 * every member inside must be serializable in order to be distributed.
	 */
	static class DBWriteBolt implements Filter, Function {
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

	public static StormTopology buildTopology(LocalDRPC drpc) throws IOException {
		FakeTweetsBatchSpout spout = new FakeTweetsBatchSpout(100);  // create spout as the source for the topology
		Function stateStore = new DBWriteBolt();
		Aggregator<Map<String, Long>> grpTotal = new GroupTotal();
		storm.trident.operation.builtin.Count counter = new storm.trident.operation.builtin.Count();
		
		TridentTopology topology = new TridentTopology();
		topology.newStream("spout", spout) 		// topology src stream point to tweet spout
			.groupBy(new Fields("location"))    // for each location fields, a virtual stream is created
			.aggregate(new Fields("location"), counter, new Fields("count"))  // aggregation on each location stream
			.aggregate(new Fields("location", "count"), grpTotal, new Fields("location", "batch_count", "sum"))
			.each(new Fields("location"), new Utils.PrintFilter());  // after aggregation, emits aggregation result.
		
		// you can add more source spout stream as you like.
		FakeTweetsBatchSpout spout2 = new FakeTweetsBatchSpout(10);  // create spout as the source for the topology
		Stream stream = topology.newStream("spout2", spout2);  // topology src stream point to tweet spout 
		stream.each(new Fields("location"), stateStore, new Fields("duploc"))
			  .each(new Fields("id", "text", "actor", "duploc"), new Utils.PrintFilter());
		
		return topology.build();
	}

	public static void main(String[] args) throws Exception {
		Config conf = new Config();

		LocalDRPC drpc = new LocalDRPC();
		LocalCluster cluster = new LocalCluster();
		cluster.submitTopology("location_groupaggregate", conf, buildTopology(drpc));
	}
}
