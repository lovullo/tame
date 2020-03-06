# Program Testing
<!--
  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

  This file is part of TAME.

  Copying and distribution of this file, with or without modification, are
  permitted in any medium without royalty provided the copyright notice and
  this notice are preserved.  This file is offered as-is, without warranty
  of any kind.
-->

A /program/ is a top-level package (either marked as with `@program="true"`,
or with a root `rater` node).  This system provides a means of writing and
running test cases.


## Test Case Format
Test cases must be provided in a YAML file.  Each test case has a
description (`description`), input data (`data`), and expected results
(`expect`).

```yaml
- description: >-
    Example test case
  data:
    state:              [ STATE_AK ]
    prem_total:         [ 9000 ]
    effective_date:     12/05/2017
  expect:
    premTaxStamping: [ 1234 ]
    premTaxSurplus:  [ 1010 ]
```

A file may contain any number of test cases.  All identifiers in `data` and
`expect` must be valid inputs (params) and outputs (classifications and
calculations) respectively.  Inputs are passed as-is to the program, and
outputs are recursively (deeply) compared.
