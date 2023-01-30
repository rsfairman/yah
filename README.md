
This is a series of tests concerning how to speed up Haskell programs. How does one speed up Haskell programs? How does the speed compare with C? Just how effective is Haskell at parallelizing a program compared to C?

The test case is a solution a game similar to Yahtzee, but much simplified. The program determines the optimal way to play (highest expected average score) and reports the resulting expected value. In the game analyzed here, there is no bonus based on the top six rows, and bonus yahtzees can be used by the player to fill any row, without limitation. This reduces the size of the calculation by a factor of roughly 192, and generally simplifies things without changing the overall character of the game. For details, see `YahRules.hs`.

The folders contain the code for successive attempts at improving the speed of the program. The essential algorithm remains the same throughout; only the details of the implementation change.

Each version consists of these files.
* `YahRules.hs` encodes the rules of the game.
* `YahDice.hs` calculates various probability tables.
* `Yahtzee.hs` solves the problem.
* `Main.hs` runs it all.


| Version     | User(s)  | CPU(s)    | User(t) | CPU(t)    |
| -----       |  -----:  | -----:    | -----:  | -----:    |
|`Initial`    | 3486.--- | 3402.---  | 332.--- |  5122.--- |
|`Cleaner`    | 3282.--- | 3249.---  | 834.--- | 12900.--- |
|`BetterAlg`  |  280.905 |  284.875  |  37.303 |   569.188 |
|`RollIndex`  |  228.292 |  233.844  |  24.141 |   370.812 |
|`RollVector` |  182.413 |  186.797  |  20.281 |   310.297 |
|`RowIndex`   |   86.371 |   90.078  |  10.139 |   151.297 |
|`RowVectorST`|   66.856 |   77.359  |  --.--- |   ---.--- |
|`RowVectorIO`|   61.444 |   70.766  |   8.543 |   110.641 |
|`WhyNot`     |    4.683 |    4.656  |   4.279 |    54.031 |


These times, in seconds, are for an i5-12600K (3.69 GHz and 16 cores) with 16 GB of RAM, and it may be relevant that the "disk" is solid state. The '(s)' is for single-threaded, and '(t)' is for threaded. The programs were run with `O2` optimization, although it didn't seem to make much difference. Everything was done with GHC 9.2.5 on Windows 10. I was careful about timing, but not obsessive, and the numbers vary by a few percent around the values given above. There are profiler output files for every version except `Initial` and `Cleaner`.

*Whoops*: I see now that many of the single-threaded tests above were run with the `-threaded` option, even though there was no explicit threading happening. If the CPU time is greater than the user time, that's what happned. 

The `Initial` version is *absolutely loaded* with inefficiencies, but that's OK. It's easy to understand, which is one of Haskell's strengths. It can be run in parallel or single-threaded by changing one line in Yahtzee.hs; search for `parList`.

The `Cleaner` version uses a single large table for the calculation rather than four related tables; otherwise, it's the same as `Initial`. This expression of the algorithm also seems easier to understand than what appears in `Initial`. Bringing all parts of the calculation "under the same roof," will make certain future changes easier. And with less data being shuffled, it's marginally faster. A mystery about `Cleaner` is that it doesn't parallelize well, and it's not clear why.

The `BetterAlg` version uses a better algorithm, while continuing to make poor choices about the implementation. Tables of probabilities are crucial, and this version is more careful about tracking the use of these tables so that certain calculations are done only once. The basic idea used in `BetterAlg` is reminiscent of pointers in a language like C. This version *does* parallelize well, even though there's no significant difference in how the program is structured.

Indiscriminate use of lists makes the program slow. Performance dies by a thousand cuts, and the worst offenders are indexing into a list with `!!`, using `find` and comparing lists with `==`. As a first step toward eliminating these shortcomings, the `RollIndex` version replaces the use of a list to represent a dice roll with an integer index. For example, there are 252 ways that 5d6 can come up, and these rolls are represented by an integer in `[0..251]`. The full benefit hasn't been gained since `RollIndex` is still being used with `!!` and `find`, but there was a significant savings; `Int` is simply less data to manage than `[Int]`.

`RollVector` uses Haskell's `Vector` module for rolls in place of lists when doing score lookups in `YahRule.hs`. This very small change produced a large savings.

`RowIndex` makes a change similar to the one made in `RollIndex`. Instead of using a `[Bool]` to track which rows of the scorecard are open, a single `Int` is used. Everything else about the algorithm remains the same, including use of `find` and `!!` for indexing. The time taken went down by a factor of more than two, merely by replacing a list everywhere with a single `Int` value. Considering that this replacement is in the outermost loop of of the algorithm shows just how costly lists are.

The `Int` used in `RowIndex` to track the selection of open rows of the scorecard is held in a way that's redundant. The `RowVectorST` version eliminates that redundant information by using a mutable `Vector`, which requires that the calculation be done in a monad. The `ST` monad is "safe" (in the Haskell sense), but it's not possible to parallelize the calculation. Even so, the single-threaded savings is significant.

The `RowVectorIO` version is essentially the same as the `RowVectorST` version, except that the calculation is done in the `IO` monad. This allows it to be done "unsafely" so that it can be threaded: search `Yahtzee.hs` for `forConcurrently_`. Note that for threading to work, `-XStrict` was added to `ghc-options`. Using this extension might speed up some of the previous versions too, but I didn't check.

At this point, a look at the profiler output shows that most of the effort is devoted to the infrastructure *around* the central calculation, and there are still a number of places where `!!` and the like are used.

I'm starting to lose interest in this exercise, but the `WhyNot` version makes some further optimizations. Vectors are used more widely so that `!!` can be avoided. It's clear from the profiler that more could be done; it wouldn't surprise me if the time could be brought down to the range of one or two seconds. It's also clear that, as the calculation is made more efficient, a different parallelization strategy is needed. 


Conclusions
-----------

Vanilla Haskell, where everything is based on lists, is *dog slow*. I haven't worked up the energy to implement this algorithm in C for a direct comparison, but the fact that it's possible to speed up the original Haskell program so dramatically indicates that there's *plenty* of room left for a C implementation to be much faster than one in Haskell.

On the other hand, there is no question that working in Haskell is orders of magnitude more pleasant than working in C. And the resulting code aligns more naturally to what the code does, making it easier to understand. Part of the reason there is no C implementation above is that C makes the problem painful to implement. Then again, after going to this much effort to make Haskell faster, the code starts to look more and more like C, thereby losing clarity.

If you want to learn Haskell, my best advice is to ignore category theory. If any instructional material even mentions the topic, move on to something else. I have a background in mathematics, and knew a bit of category theory long before I'd heard of Haskell. The idea of a language where category-theoretic ideas are somehow central was attractive, and I got sucked into wasting time trying to unravel how category theory and Haskell link up. It's true that certain ideas from category theory have parallels in Haskell, but don't get distracted by trying to understand these parallels; most of it seems like vague nattering. Someday these parallels may be put on a firmer basis, and even then I doubt that category theory will help with practical programming. Of course, if you're a language theorist or know how to play the shakuhachi (or get that joke), then maybe you'll see things I don't. So don't worry about the theory; just jump in and use the language.

#### What I wish for Haskell 

The biggest thing that would improve Haskell's usability is top-level mutable state (global variables). This would be contrary to the spirit of the language, and make any number of people howl, but it's already possible. The "unsafePerformIO hack" is one way to access global variables now, and it's possible to read/write to an external file and treat that as mutable state. So global variables are already part of the language in some sense; they should be easier to use.

Haskell needs more strictness and the ability to unambiguously sequence the order of actions. Certain tasks are easier to express and reason about using an imperative famework. Maybe `DO` (instead of `do`) could be used for blocks of imperative code. Inside a `DO` block, there would be no "thunking" whatsoever and no reordering of operations. Ideally, `DO` should be recursive so that when a function is called from a `DO` block, it is treated imperatively; if the same function is called from lazy code, then the function is also allowed to be lazy.

One of the reasons Haskell is slow is all of the boxing (and thunking). Things like `Data.Vector.Unboxed` help, but they are syntactically awkward. The language needs a way to express the idea (expressed in C-like terms) of an array of structs. 



