package com.colorcloud.trident;

import java.util.ArrayList;
import java.util.List;

import storm.trident.operation.TridentCollector;
import storm.trident.state.BaseQueryFunction;
import storm.trident.tuple.TridentTuple;
import backtype.storm.tuple.Values;


public class LocationStateQuery extends BaseQueryFunction<LocationState, Long>{
	
	// for each tuple, get the location tag, update global location map, return cnt
	@Override
	public List<Long> batchRetrieve(LocationState state, List<TridentTuple> args) {
		List<Long> loccnts = new ArrayList<Long>();
		for(TridentTuple tuple : args){
			String loc = (String) tuple.getValue(0);   // first field is location
			long cnt = state.incrementAndGet(loc);
			loccnts.add(cnt);
		}
		
		return loccnts;
	}

	@Override
	public void execute(TridentTuple tuple, Long locCnt, TridentCollector collector) {
		//emit by tweet id
		String loc = (String) tuple.getValue(0);
		collector.emit(new Values(loc, locCnt));
	}


}