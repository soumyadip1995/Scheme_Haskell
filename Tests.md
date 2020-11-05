### Parsing- main.hs

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
- Flaoting point :- 0.811223

#### Recursive parser test cases:-

- ./ghc main.hs && main "(a test)"
- ./ghc main.hs && main "(a (nested)test)"
- ./ghc main.hs && main "((a (dotted . list) test)"

Te 1:- 
- Rewrite parseNumber, without liftM, using
  - [do-notation](http://www.haskell.org/haskellwiki/all_about_monads#Do_notation)
  - explicit sequencing with the >>= operator 

Te 2:- Change parseNumber to support the [Scheme standard for different bases](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4). Import numeric, use readOct, readHex

Te 3:- Add a Float constructor to LispVal.Use readFloat


### Evaluator - eval.hs

#### Primitives

In Lisp, the data types for both code and data are the same, so our evaluator will return a LispVal. Evaluating numbers, strings, booleans and quoted lists.

#### Adding basic primitives - primitives.hs

- Improving our scheme, a little bit, so that we can use it for basic calculations.
- FIXED:- Support added for primitives to perform the various [type-testing](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3), symbol, string, number ,etc.
- Additional primitives - including compareBinop fixed. No errors
- Conditional pattern matching.

#### Test runs for eval.hs, primitives.hs and repl.hs

- Initial Evaluator - ./ghc eval.hs && eval "(1 2 2)"
- ./ghc eval.hs && eval "'(1 3 (\ "this \ "  \ " one \ "))"
- Primitives:- ./ghc eval.hs && eval "'atom"
- Adding basic Primitives./ghc eval.hs && eval "(+ 2 (-4 1))"
- Adding basic primitives./ghc eval.hs && eval "(- (+ 4 6 3) 3 5 2)"
- ./ghc primitives.hs && primitives "(< 2 3)"
- BUG FIX:- Lispval returns #f. Use Import Control.Monad.Except. This is the way I suppose. Treat it as a garbage value.
- pattern Matching 2:- BUG FIX:- #f by adding an if clause or use an error throw.
- ./ghc primitives.hs && primitives "(if (> 2 3) \"no\" \"yes\")"
- repl all cleaned up and float support added.
- repl:- ghc repl.hs
- Tests:- (+ 2 3), ( * 2 3), (> 4 2)


#### Tests for Env and Bindings - env.hs and bind.hs

Env- monads and IOref
- ./ghc eval.hs > (define x 3) > (+ x 2)

Primitive Bindings
- ./ghc bind.hs > (f 1 2 3)


#### TO DO:

- Change func from LispVal to string. for string matching
- That means changing the eval atom. also unpacknum (String s) = s
- Open questions:- Can more expressions be added to the body. (List). How to use Records ?. Are monads only used for finite states or can it provide support for non- determinism. If so, then how can those computations be performed. What changes do we need to make
