<!---
  Copyright (C) 2015-2021 Ryan Specialty Group, Inc.

  Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License,
  Version 1.3 or any later version published by the Free Software
  Foundation; with no Invariant Sections, no Front-Cover Texts, and no
  Back-Cover Texts.  A copy of the license is included the file
  COPYING.FDL.
-->
# TAME
TAME is The Algebraic Metalanguage, a programming language and system of tools
designed to aid in the development, understanding, and maintenance of systems
performing numerous calculations on a complex graph of dependencies,
conditions, and a large number of inputs.

This system was developed at Ryan Specialty Group (formerly LoVullo Associates) to
handle the complexity of comparative insurance rating systems. It is a
domain-specific language (DSL) that itself encourages, through the use of
templates, the creation of sub-DSLs.  TAME itself is at heart a
calculator—processing only numerical input and output—driven by quantifiers
as predicates. Calculations and quantifiers are written declaratively
without concern for order of execution.

The system has powerful dependency resolution and data flow capabilities.

TAME consists of a macro processor (implementing a metalanguage), numerous
compilers for various targets (JavaScript, HTML documentation and debugging
environment, LaTeX, and others), linkers, and supporting tools.  The input
grammar is XML, and the majority of the project (including the macro processor,
compilers, and linkers) is written in a combination of XSLT and Rust.


## TAMER
Due to performance requirements, this project is currently being
reimplemented in Rust.  That project can be found in the [tamer/](./tamer/)
directory.


## Documentation
Compiled documentation for the latest release is available via our GitLab
mirror, which uses the same build pipeline as we do on our internal GitLab
instance.  Available formats are:

- [Multi-page HTML][doc-html]
- [PDF][doc-pdf]
- [Info][doc-info]


## Getting Started
To get started, make sure Saxon version 9 or later is available and its path
set as `SAXON_CP`; that the path to hoxsl is set via `HOXSL`; and then run
the `bootstrap` script:

```bash
$ export SAXON_CP=/path/to/saxon9he.jar
$ export HOXSL=/path/to/hoxsl/root

$ ./boostrap
```

## Running Test Cases
To run the test cases, invoke `make check` (or its alias, `make test`).

##### Testing Core Features
In order to run tests located at `core/test/core/**`, a supporting environment
is required. (e.g. mega rater). Inside a supporting rater, either check out a
submodule containing the core tests, or temporarily add them into the
submodule.

Build the core test suite summary page using:

```bash
$ make rater/core/test/core/suite.html
```

Visit the summary page in a web browser and click the __Calculate Premium__
button. If all test cases pass, it will yield a value of __$1__.


## Hacking
Information for TAME developers can be found in the file `HACKING`.


## License
This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

[doc-html]: https://lovullo.gitlab.io/tame/
[doc-pdf]: https://lovullo.gitlab.io/tame/tame.pdf
[doc-info]: https://lovullo.gitlab.io/tame/tame.info

