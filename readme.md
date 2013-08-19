# UFdb a reliable performant BSON database with ACID written in Haskell.


UFdb started out as an experiment into working with binary structures as well as persitence in Haskell. The goal of the project is to be slim (the core server is less than 250 lines) as well as give decent performance. The server is built with threading in mind using Haskell's standard forkIO. Acidstate automatically handles thread access for us. Currently all lookups are naive.


# ACID 

Because UFdb is built on acidstate, it comes with the ACID properties you expect from a modern DB. Acidstate provides a brief summary of what ACID is and how it provides them.

```
Atomicity
    State changes are all-or-nothing. This is what you'd expect of any state variable in Haskell and AcidState doesn't change that.
Consistency
    No event or set of events will break your data invariants.
Isolation
    Transactions cannot interfere with each other even when issued in parallel.
Durability
    Successful transaction are guaranteed to survive unexpected system shutdowns (both those caused by hardware and software). 
```

For more information see the wikipedia page: http://en.wikipedia.org/wiki/ACID

# BSON

I chose BSON to hopefully capture some of the performance benefits that Haskell gives with working with ByteStrings in IO vs regular String (That I would use if I was using JSON as a document type). BSON also allows me to reuse mongoDB datastructures that nearly every language has bindings for. 

# Current State

So far the server accepts puts, gets by ObjectID and filters.

## Operations

The basic API structure of the server is laid out in a BSON document. Requests and Responses are laid out in structured BSON documents that should be easy to integrate with the BSON libraries of various languages. 

The Haskell datatype for our operations that the server uses is below:

```
data UFOperationType = UFPut | UFGet | UFFilter
  deriving (Generic, Show, Typeable, Eq)
data UFOperation = UFOperation { operationType :: UFOperationType, operationOptions :: B.Document }
  deriving (Generic, Show, Typeable, Eq)
```

Each operation holds what they want to perform on the server (Putting, Getting, or Filtering) and a BSON document which holds the parameters for operation. 

### An Example: Filtering

Currently about the only interesting operation we can perform on our database is filtering. To build a filter query for UF it would look something like this:

```
-- | Datastructure we want to filter for

[ test20: [test20 : [], test21 : 10], test21 11 ]

-- | Our constructed operation :

[ operation: [ operationType: [ _co: "UFFilter"], 
               operationOptions: [ parameters: [ $LT: 
                                                   [ label: "test20.test21", 
                                                     value: 100
                                                   ]
                                               ]
                                 ]
            ]
]    

```

Available filters are $LT, $GT, $EQ for comparing and $union and $intersection for set operations.  Below is what a union query would look like:

```
-- | Datastructure we want to filter for

[ test20: [test20 : [], test21 : 10], test21 11 ]

-- | Our constructed operation (This will filter for all documents where test21 is less than 10 and test20.test21 is more than 50)

[ operation: [ operationType: [ _co: "UFFilter"], 
               operationOptions: [ parameters: [ $union: 
                                                   [ arg1: ["$LT", [label : "test21", value : 10]
                                                     arg2: ["$GT", [label : "test20.test21", value : 50]
                                                   ]
                                               ]
                                 ]
            ]
]    

```

# Benchmarks

I included some basic benchmarks written in python and haskell comparing this database to MongoDB. So far insertion is comparable with MongoDB and is even 2x faster than Mongo when not in UnConfirmedWriteMode. Searching is very slow because it has to filter over all the documents in the database when it searches. I plan to fix this soon though.

Here are some results:

MongoDB UnConfirmedWriteMode

```
Puting 1000 small documents to server individually...
CPU time:   0.17s
Done.
Puting 1000 big documents to server individually...
CPU time:   9.87s
Done.
Now Searching over documents 100x...
CPU time:   0.03s
```

MongoDB ConfirmedWriteMode

```
Puting 1000 small documents to server individually...
CPU time:   0.27s
Done.
Puting 1000 big documents to server individually...
CPU time:   9.48s
Done.
Now Searching over documents 100x...
CPU time:   0.03s
Done.
```

UFDB:

```
Puting 1000 small documents to server individually...
CPU time:   0.17s
Done.
Puting 1000 big documents to server individually...
CPU time:   3.69s
Done Putting
Getting documents from server 100 times...
CPU time:   0.08s
Done!
```
# Plans for the future

Future plans for this project could include implementing a DSL for performing mapreduce and filter queries. 
