# Storm Trident example written in clojure.

This is a simple example of using Storm Trident for data processing written in clojure

## Setup

In lein-2, all dependent libs are in maven repos at $HOME/.m2/repository/package
If you have local jar, use mvn deploy rather than mvn install.

  lein-2 new trident-clj
  lein-2 deps
  lein-2 with-profile dev compile
  lein-2 with-profile dev run


## redis data mapper

Start redis server, we will store intermediate state into redis server.

We use redis data mapper to store object model in redis.


## Trident Notes.

1. Each batch is a reliable transaction. 
2. GroupBy and aggregation functions are invoked per batch, init and complete upon batch start, and aggregate on each trident tuple in the batch.
