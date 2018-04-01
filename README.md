tools.api.alpha
========================================

A schema for defining api (ala swagger/openapi) with focus on ergonomics, complex use cases (generics ..) and java generators.

# Rationale

Api schema driven development is beneficial for developers so that they can have a clear idea of what and how they are 
going to structure an api before doing any kind of code development. 

There are a multitude of tools to describe apis but none of them is:

* expressive enough: json is too simple and easily become a mess.
* powerful enough: defining generics types is too challenging or down right impossible.
* simple enough: because of json limitation expressing something simple become too complex and usually achieved with 
too verbose nesting or code duplication.
* extensible enough: hooking into the code generation of schema interpretation is extremely difficult. 

tools.api.alpha aims to solve these problems by providing an easy to write first edn schema,
a set of validators to check the righteousness of the schema and the api you're describing and 
generators (retrofit2, jersey, java beans ...)

# Developer Information

* [GitHub project](https://github.com/ga2arch/tools.api.alpha)

# Copyright and License

Copyright © 2018 Gabriele Carrettoni and contributors

All rights reserved. The use and
distribution terms for this software are covered by the
[Eclipse Public License 1.0] which can be found in the file
epl-v10.html at the root of this distribution. By using this software
in any fashion, you are agreeing to be bound by the terms of this
license. You must not remove this notice, or any other, from this
software.

[Eclipse Public License 1.0]: http://opensource.org/licenses/eclipse-1.0.php
