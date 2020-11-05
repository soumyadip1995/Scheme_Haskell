### Scheme in Haskell

Following the [Write Yourself a Haskell Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) tutorial.

#### Why Haskell 

Languages such as C/C++ are imperative in the sense, they provide a sequence of actions. Functional programming languages such as haskell evaluate expressions instead of evaluating sequential instructions. Simply put, haskell can reach a stage of functional abstraction(In functional abstraction, details of the algorithms to accomplish the function are not visible to the consumer of the function. The consumer of the function need to only know the correct calling convention and have trust in the accuracy of the functional results) that imperative languages cannot without adding more keywords and also not being able to get rid of sequencing.

#### Downloading Haskell

Install haskell and check if `ghci` is working or not in the command prompt. You might have to add the path. [Learn You a Haskell](http://learnyouahaskell.com/chapters) is a great reference.

[Download haskell](https://www.haskell.org/platform/windows.html). Follow the steps:-

Step 1:- [Configure chocolatey](https://chocolatey.org/install) on your machine.

Step 2:- open up powershell on your machine, Run -  Get-ExecutionPolicy, to check whether or not it is restricted.

Step 3:- Run the command from the Configure Chocolatey link provided above.

Step 4:- Run ` choco install haskell-dev `.

#### Run

1. main.hs is the parser
2. eval.hs is the evaluator
3. primitives.hs is to add basic primitives, to perform basic calculations and pattern matching.
4. repl.hs - Building a read, eval, print, loop.
5. env.hs - managing states
6. bind.hs - primitive Bindings.

To run the tests , look into tests.md.

Corresponding .o , .h  and exec files have been saved.

