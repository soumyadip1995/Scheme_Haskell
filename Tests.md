### Parsing
#### FIXED :heavy_check_mark:
- Whitespace support
- ParseNumber without LiftM
- ParseNumber changed.
- Support added for Floating point
- Support added for recursive parsing

#### Return Values

#### Running test cases:-


- Whitespace ./ghc main.hs && main " $"
- Return Value ./ghc main.hs && main " \ "this is shreya \ "
- Return Value ./ghc main.hs && main 25
- float value = 23.3334

#### Recursive parser test cases:-

- ./ghc main.hs && main "(a test)"
- ./ghc main.hs && main "(a (nested)test)"
- ./ghc main.hs && main "((a (dotted . list) test)"

Te 1:- 
-- Rewrite parseNumber, without liftM, using
-- [do-notation](http://www.haskell.org/haskellwiki/all_about_monads#Do_notation)
-- explicit sequencing with the >>= operator 

Te 2:- Change parseNumber to support the [Scheme standard for different bases](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4). Import numeric, use readOct, readHex

Te 3:- Add a Float constructor to LispVal.Use readFloat


### Evaluator

#### Tests run

- ./ghc eval.hs && eval "(1 2 2)"
- ./ghc eval.hs && eval "'(1 3 (\"this\" \"one\"))"