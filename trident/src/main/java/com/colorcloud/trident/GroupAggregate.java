package com.colorcloud.trident;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import storm.trident.TridentTopology;
import storm.trident.operation.BaseAggregator;
import storm.trident.operation.TridentCollector;
import storm.trident.operation.builtin.Count;
import storm.trident.tuple.TridentTuple;
import backtype.storm.Config;
import backtype.storm.LocalCluster;
import backtype.storm.LocalDRPC;
import backtype.storm.generated.StormTopology;
import backtype.storm.tuple.Fields;
import backtype.storm.tuple.Values;

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
		private static final String TAG = "GroupTotal :";
		Map<String, Long> result = new HashMap<String, Long>();
		
		/**
		 * init is called upon every batch. each batch has a batch id
		 */
		@Override
		public Map<String, Long> init(Object batchId, TridentCollector collector) {
			System.out.println(TAG + "init :" + batchId); 
			return result;
		}
	
		/**
		 * aggregate called upon every tuple inside the batch
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
			collector.emit(new Values(val));
		}	         
	}

	public static StormTopology buildTopology(LocalDRPC drpc) throws IOException {
		FakeTweetsBatchSpout spout = new FakeTweetsBatchSpout(100);

		TridentTopology topology = new TridentTopology();
		topology.newStream("spout", spout) 		// topology src stream point to tweet spout
			.groupBy(new Fields("location"))    // for each location fields, a virtual stream is created
			.aggregate(new Fields("location"), new Count(), new Fields("count"))  // aggregation on each location stream
			.aggregate(new Fields("location", "count"), new GroupTotal(), new Fields("location", "batch_count", "sum"))
			.each(new Fields("location"), new Utils.PrintFilter());  // after aggregation, emits aggregation result.
		
		return topology.build();
	}

	public static void main(String[] args) throws Exception {
		Config conf = new Config();

		LocalDRPC drpc = new LocalDRPC();
		LocalCluster cluster = new LocalCluster();
		cluster.submitTopology("location_groupaggregate", conf, buildTopology(drpc));
	}
}
