package com.colorcloud.trident.storage;


import java.util.Map;

import redis.clients.jedis.Jedis;

public class JedisDB {
	//private static final long serialVersionUID = 7526472295622776147L;
	
	private String host;
	private int port;
	private Jedis jedis;  
	
	public JedisDB() {
		host = "localhost";
		port = 6379;
		jedis = new Jedis(host, port);
		System.out.println("JedisDB connecting to server");
	}
	
	public JedisDB(String h, int p){
		host = h;
		port = p;
		jedis = new Jedis(host, port);
	}
	
	/**
	 * append a vector of string values to the redis keyed list.
	 */
	public long rpush(final String key, final String... strings){
		return jedis.rpush(key, strings);
	}

	/**
	 * set hash field and value of the key.
	 */
	public Long hset(final String key, final String field, final String value) {
		return jedis.hset(key, field, value);
	}
	
	/**
	 * store a map into redis
	 */
	public void storeMap(final String key, final Map<?, ?> map){
		for (Map.Entry<?, ?> entry : map.entrySet()) {
		    String field = entry.getKey().toString();
		    Object value = entry.getValue();
		    	jedis.hset(key, field, value.toString()); 
		}
	}
}
