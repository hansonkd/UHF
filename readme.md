# UFdb a reliable performant BSON database with ACID written in Haskell.


UFdb started out as an experiment into working with binary structures as well as persitence in Haskell. The goal of the project is to be slim (the core server is less than 150 lines) as well as give decent performance. The server is built with threading in mind using Haskell's standard forkIO. Acidstate automatically handles thread access for us.


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

# Plans for the future

Future plans for this project could include implementing a DSL for performing mapreduce and filter queries. 