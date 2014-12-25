dissent
=======
 
Haskell implementation of the Dissent accountable group anonymity protocol

Motivation
==========

[Dissent](http://dedis.cs.yale.edu/dissent/papers/ccs10/) is a protocol for accountable anonymous group messaging. I am building a pure Haskell implementation of this protocol as a building block for [bitgloom](http://github.com/solatis/bitgloom).

Design
======
I base the code on the Dissent paper and proof of concept implementation. The code is designed a library, rather than a standalone application, in order to improve its usability.
