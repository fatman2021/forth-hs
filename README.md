# Forth

An interpreter for a bespoke dialect of Forth that has built in list primatives.

```haskell 

import Language.Forth\

result = forth "1 2 + DUP DUP" -- [3, 3]


```
