## Synopis ##

_What the project does, the problem it solves, etc._

> Encalmo is a Scala library designed to facilitate modeling and documenting of recurrent engineering calculations.
  * Goal: You are modeling calculations but Encalmo does it for you and document, step by step, in a pretty way (html + mathml, pdf, latex and so on).
  * Range of applications: real-life and educational structural engineering calculations

_Why it does it this way, benefits and drawbacks_

> Enclamo uses powers of the Scala language:

  * DSL capabilities (operator is a method, space instead of dot operator, implicit conversions, objects, default parameters) for easy-to-read and friendly (mathematical) appearance of user's code
  * flexible type system, pattern matching, implicits and collections for easy modeling of transformations and calculations under the hood

_Instructions that detail how the project is compiled, deployed and used._

> In the current phase Encalmo is a scala library, operated with unit tests :-), in the future combined with dedicated web gui.

## Main features ##

  * easy calculations using symbolic expressions
  * automatic units of values management and conversions
  * advanced MathML, HTML and PDF output with custom styling
  * FEA modelling with Z88

## Overview ##

|Module name|Description|
|:----------|:----------|
| **[graph](http://code.google.com/p/encalmo/source/browse/graph/src/main/scala/org/encalmo/graph/)** |graphs|
| **[expression](http://code.google.com/p/encalmo/source/browse/expression/src/main/scala/org/encalmo/expression/)** |symbolic expressions|
| **[calculation](http://code.google.com/p/encalmo/source/browse/calculation/src/main/scala/org/encalmo/calculation/)** |calculation context|
| **[document](http://code.google.com/p/encalmo/source/browse/document/src/main/scala/org/encalmo/document/)** |documentation of calculations|
| **[printer](http://code.google.com/p/encalmo/source/browse/printer/src/main/scala/org/encalmo/printer/)** |output of documents in different formats|
| **[fop](http://code.google.com/p/encalmo/source/browse/fop/src/main/java/org/encalmo/fop/)** |fop and pdf format support|
| **[structures](http://code.google.com/p/encalmo/source/browse/structures/src/main/scala/org/encalmo/structures/)** |examples of civil engineering calculations according to eurocode|
| **[chart](http://code.google.com/p/encalmo/source/browse/chart/src/main/scala/org/encalmo/chart/)** |JOGL charts drawing support|
| **[fea](http://code.google.com/p/encalmo/source/browse/fea/src/main/scala/org/encalmo/fea/)** |fea analysis support using Z88 |
