# clojure idioms

This repo contains patterns and examples of idiomatic clojure.
This includes common clojure patterns for map reduce, best practice to connect to distributed components, utilize dependent libraries, DSL for other platforms, core.async examples, and some algorithms and puzzles in clojure.

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

## Trident-clj

Re-implement Trident example of log processing with grouping and aggregation using clojure. Trident currently does not have clojure DSL. Implementation is based on clojure gen-class.

## others

Other small modules contains examples of idiomatic clojure programming, e.g., core.async, graph algorithm, qsort in 5 lines, defmacro, etc.