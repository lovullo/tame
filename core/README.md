# TAME Core
Core library for TAME, providing generic abstractions for common
operations.

This library has accumulated a bit of cruft, is disorganized, and has
not been substantially refactored to take advantage of new language
features.  It is a work in progress.


## Features
- BDD abstraction;
- Classification match manipulation;
- Common operations on numbers;
- Conditional evaluation helpers;
- Core primitive declarations;
- Interpolation;
- Interval mapping;
- Matrix and vector manipulation;
- Query matrices as data tables;
- Value mappings; and
- Other miscellaneous stuff.


## What is TAME?
TAME is The Adaptive Metalanguage, a programming language and system of tools
designed to aid in the development, understanding, and maintenance of systems
performing numerous calculations on a complex graph of dependencies,
conditions, and a large number of inputs.

This system was developed at R-T Specialty Buffalo to handle the complexity of
comparative insurance rating systems. It is a domain-specific language (DSL)
that itself encourages, through the use of templates, the creation of sub-DSLs.
TAME itself is at heart a calculator—processing only numerical input and
output—driven by quantifiers as predicates. Calculations and quantifiers are
written declaratively without concern for order of execution.

The system has powerful dependency resolution and data flow capabilities.

TAME consists of a macro processor (implementing a metalanguage), numerous
compilers for various targets (JavaScript, HTML documentation and debugging
environment, LaTeX, and others), linkers, and supporting tools.  The input
grammar is XML, and the majority of the project (including the macro processor,
compilers, and linkers) are written in XSLT. There is a reason for that odd
choice; until an explanation is provided, know that someone is perverted enough
to implement a full compiler stack in XSLT.


## License
This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

