# clojure idioms

This repo contains examples of idiomatic clojure.

## dbconn

The project illustrates clojure way to connect to Mysql and Redis and read/write records from/to the underlying data store with ORM data model.

## msqqueue

The project illustrates clojure way of abstracting rabbitmq queue into lazy sequence and use clojure sequence library to process message queues. We use rabbitmq to distribute computations to a swarm of workers and use callback queue to retrieve the result.

## wordcount

A simple demo of data processing using storm. We create a topology to transform stream of logs into in memory state by filtering, grouping, and aggregation. Working in progress to provide state query API in real-time.

## 4clojure

My solutions to 4clojure puzzels. Currently ranked 162 out of 14099 users.

## Trident

This repo contains example of creating storm trident for processing logs with filter and aggregation. We illustrate with two implmenetations, one with java and one with clojure. 

## others

Other small modules contains examples of idiomatic clojure programming, e.g., graph algorithm, qsort in 5 lines, defmacro, etc.