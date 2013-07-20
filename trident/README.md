# Storm Trident example

This is a simple example of using Storm Trident for data processing.

## Setup

This is no project scaffolding for storm project. We need to create new project manually and run storm cluster manually.

  1. create an eclipse java project add include storm jar files as dependency.
    mkdir -p src/main/java/com/colorcloud/trident

    config java build path add source folder to point to src/main/java.

  2. cp all dependent jar files in to lib folder, or just link it from jars in some existing projects without copying.

  3. start redis server, we will store intermediate state into redis server.

  4. run main class file by pointing classpath to libs and local bin and resource.
    java -cp ./bin:lib/*:lib/dev/*:src/main/resources com.colorcloud.trident.GroupAggregate

  5. To run topology, build in eclipse and execute the class main.

    java -cp ./bin:lib/*:lib/dev/*:src/main/resources com.colorcloud.trident.GroupAggregate



## Trident Notes.

1. Each batch is a reliable transaction. 
2. GroupBy and aggregation functions are invoked per batch, init and complete upon batch start, and aggregate on each trident tuple in the batch.


## Trident by Clojure.

Clojure DSL for Trident is still under development. 
For now, we can use gen-class to write trident topology in Clojure.
check whoahbot's github and blog.