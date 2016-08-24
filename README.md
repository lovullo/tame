<!---
  Copyright (C) 2015, 2016 LoVullo Associates, Inc.

  Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License,
  Version 1.3 or any later version published by the Free Software
  Foundation; with no Invariant Sections, no Front-Cover Texts, and no
  Back-Cover Texts.  A copy of the license is included the file
  COPYING.FDL.
-->
# TAME
TAME is The Adaptive Metalanguage, a programming language and system of tools
designed to aid in the development, understanding, and maintenance of systems
performing numerous calculations on a complex graph of dependencies,
conditions, and a large number of inputs.

This system was developed at LoVullo Associates to handle the complexity of
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
to implement a full compiler in XSLT.

More information will become available as various portions are liberated
during refactoring. [tame-core](https://github.com/lovullo/tame-core) is
TAME's core library, and [hoxsl](https://github.com/lovullo/hoxsl) was
developed as a supporting library.


## "Current"
The current state of the project as used in production is found in
`src/current/`.  The environment surrounding the development of this
project resulted in a bit of a mess, which is being refactored into
`src/` as it is touched.  Documentation is virtually non-existent.


## License
This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

[nova-ho]: http://conferences.idealliance.org/extreme/html/2006/Novatchev01/EML2006Novatchev01.html
[xslt-30-ho]: http://www.w3.org/TR/xslt-30/#dt-higher-order-operand
