package com.colorcloud.trident;

import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicLong;

import storm.trident.state.State;

public class LocationState implements State, Serializable{

	ConcurrentMap<String, AtomicLong> locationMap;
	
	public LocationState(int partitionIdx, int numPartitions) {
		locationMap = new ConcurrentHashMap<String, AtomicLong>();
		System.out.println("LocationState : makeState : " + partitionIdx + " / " + numPartitions);
	}
	
	/**
	 * You got love clj! (def m (atom {})) (swap! m update-in [:c] (fnil inc 0)) 
	 */
	public long incrementAndGet(String loc){
		long cnt = 1;
		AtomicLong firstv = new AtomicLong(1);
		// put only when absent, otherwise get.
		AtomicLong cur = locationMap.putIfAbsent(loc, firstv); 
		if (cur != null){
			cnt = cur.incrementAndGet();
		}
		return cnt;
	}
	
	@Override
	public void beginCommit(Long txid) {
		// TODO Auto-generated method stub
	}

	@Override
	public void commit(Long txid) {
		// TODO Auto-generated method stub
	}

}
