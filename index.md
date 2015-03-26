---
title       : Authoring R Packages
subtitle    : IAMCS Machine Learning and Applied Statistics Workshop Series
talkdate    : February 18, 2014
author      : Mathew McLean
job1        : Research Assistant Professor
job2        : Texas A & M University
job3        : http://stat.tamu.edu/~mmclean
logoUni     : Tamulogo.png
logoDep     : IAMCSlogo.svg
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      #
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
--- .smallerSpacing

## Functions

- There are two types of functions
  1. closures
  2. primitives
- Primitives are implemented in `C` and cannot be created by the user
- Closures can be created using the primitive function `function`

```r
c(mode(`function`), typeof(`function`), mode("function"))
```

```
## [1] "function"  "special"   "character"
```
- A closure has three parts
  1. formal arguments: variables passed to the function for it to use
  2. body: an R expression, or several expressions enclosed in `{ }`
  3. environment - current environment when function is created (more on this later)


---

## Extracting the Parts of a Closure


```r
FunParts <- function(fun.name){
  return(list(formals(fun.name), body(fun.name), environment(fun.name)))
}
FunParts(ncol)
```

```
## [[1]]
## [[1]]$x
## 
## 
## 
## [[2]]
## dim(x)[2L]
## 
## [[3]]
## <environment: namespace:base>
```

---

## Dot-dot-dot

- `...` is a special object type in `R` used to passed extra arguments to a function
- Arguments that don't match any formal arguments of a function are matched with `...`
- Arguments matched to `...` can be extracted using `list(...)`
  - can also use `c(...)` or `as.list(...)`

---

## Partial matching of Argument Names

- Full names of arguments need not always be specified when calling a function
  * True for extraction (`$`), and attributes (`attr`) as well
- Note: partial argument matching occurs before `...`, but not after it

```r
DemoPartialMatch <- function(first.arg, ..., second.arg){c(...)}
DemoPartialMatch(fir = 1, sec = 2)
```

```
## sec 
##   2
```
- **Bad practise**, leads to bugs, ***don't* use partial matching**
- R has global options which can be set to warn about this

---

## Useful Functions for Working with Arguments

- Use `missing(x)` to check if the argument `x` is specified in the call to a function
- Use `hasArg(x)` to check for argument `x` is in `...` or in formal arguments

```r
TestingMissing <- function(x, ...){
  print(c(hasArg(x), hasArg(y), missing(x)))
  missing(y)
  invisible(x)
}
TestingMissing(y = 1)
```

```
## [1] FALSE  TRUE  TRUE
```

```
## Error in missing(y): 'missing' can only be used for arguments
```
- Alternative: `function(x = NULL){ is.null(x) ...`
- Note: the use of `invisible` prevents `x` from being printed if no assignment occurs

---

## Checking Arguments

- We need to make sure the arguments are in the form we expect
- errors, warnings, and messages should be issued as necessary
  - messages should be *meaningful* for the user

Some useful functions include
* `is.na`, `is.null`, `is.numeric`, etc. for checking type
* `all`, `any`, e.g. `any(is.na(x))`
* use `inherits` to check the class of an object
* `identical` **is the proper way to check equality** in `if` and `while` statements
  * consider `x == y` when `x` or `y` does not have length one or are `NA` or `NULL`
  * all.equal is for *near* equality; though `isTRUE(all.equal(...))` is fine too

---

## Handling Errors

* `try` - wrapper to safely run an expression that may fail

```r
result <- try(FooFun(x), silent = TRUE)  # silent = TRUE means error message suppressed
if (inherits(result, "try-error"))
  result <- NA
```
* Equivalent alternative:

```r
result <- tryCatch(FooFun(x), error = function(e) return(NA))
```
* Try `demo(error.catching)` in `R` and see [?conditions](http://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html)
* if messages can be ignored when function is called by your code, use `suppressMessages`
  * `suppressWarnings` for suppressing warnings

--- .smallerSpacing

## Working with Calls

- a `call` is one of three language objects in `R` (the others are `expression`s and `name`s)
- We may use `match.call()` inside a function to capture the original call
- This is useful for example:
  - to modify the contents of a call and pass them on to another function ; see e.g. `lm`
  - to save the call for later use (e.g. returning it as part of a list)

```r
GetCallParts <- function(x, ...){
  .call <- match.call(expand.dots = TRUE)
  return(list(.call[[1]], .call$z))
}
GetCallParts(x, y = 1, z = 2)
```

```
## [[1]]
## GetCallParts
## 
## [[2]]
## [1] 2
```

---

## Working with Calls

* We can use `do.call` to call a function by name and argument list (example coming later)
* We can evaluate calls using `eval`

```r
StupidSum <- function(x, ...){
  .call <- match.call(expand.dots = TRUE)
  .call$x <- NULL
  .call$na.rm <- TRUE
  .call[[1L]] <- as.name("sum")
  return(eval(.call))
}
StupidSum(x = "hi", y = 1, z = 2, z2 = NA)
```

```
## [1] 3
```

--- .smallerSpacing

## Functions are First Class Objects

Functions may be
  - assigned to variables and stored in data structures
  - used as arguments to other functions
  - returned by other functions
  - anonymous (not bound to a variable)

Example: Change error distribution used to simulate complicated model

```r
GetErrorDist <- function(err.fun.name, ...){
  err.fun.name <- match.fun(err.fun.name)
  return(function(n) do.call(err.fun.name, list(n, ...)))
}
ErrorDist <- GetErrorDist("rnorm", sd = 3, mean = 10)
## data <- SimulateModel(n = 100, ErrorDist, other.args)
ErrorDist <- GetErrorDist("rt", df = 2); ErrorDist(3)
```

```
## [1] -0.01098557  0.58717775  3.81613738
```

--- .biggerSpacing

## Scope, Environments, and Frames

- Assignment (e.g. `x <- 1`) and function calls (e. g. f(x = 1)) *bind* values to variables
- A *frame* is a set of bindings (collection of objects)
- A variable is allowed only one binding per frame
  - Can have different bindings in different frames
- A free variable in a particular frame is one that is unbound (has no assigned value)
- Scoping rules determine where `R` looks for values for free variables
- An environment is made up of a frame and a pointer to an enclosing environment
- The scope of a binding is the set of environments in which it is visible

--- .biggerSpacing

## Environments and Function Calls

- Recall, a function has three parts: arguments, a body, and an enclosing environment
  - The function's enclosing environment is the one it was **created** in
- When a function is called, the following occurs
   1. Args. in the call are matched to the formal args of the function definition
   2. A **new** environment is created and assignments are made to it for each formal arg.
      - The function's environment becomes the enclosing environment for this one
   3. The body of the function is evaluated in this new env. and result is returned

--- .biggerSpacing

## Environments and Frames

- User's workspace is `R_GlobalEnv`, which is bound to `.GlobalEnv`
- `search()` gives a list of all attached packages - "the search path"
  - i.e. all env. where `R` will look for bindings (including `.GlobalEnv`)
- When a package is attached, its environment becomes the enclosing env. of R_GlobalEnv
- `x <<- 1` will bind value `1` to first instance of a variable `x` in the search path
  - or create a binding for `x` in `R_GlobalEnv` if no existing binding is found
- Craziness: Functions `parent.frame` and `parent.env` are *very* different
  - `parent.frame(1)` gives the environment of the calling function - used frequently
  - `parent.env` gives the enclosing environment of its argument - rarely of *direct* use
- Also see `?with` and `?local`
- Two nice references on frames, environments and scope in `R` [here](http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff) and [here](http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-scope.pdf)


--- .smallerSpacing

## Lexical Scoping Example

* Every closure is associated with a enclosing environment (the one it was _defined_ in)

```r
Fun1 <- function(x){
  y <- 1
  H(x)
}
Fun2 <- function(x){
  y <- 1
  G <- function(z) z + y
  G(x)
}
H <- function(z) z + y
y <- 0; x <- 100
c(Fun1(1), Fun2(1))  # Because of local bindings for x in both Fun.'s, x in .GlobalEnv is ignored
```

```
## [1] 1 2
```

* Since `H` is **defined** in `.GlobalEnv`, `H` uses `y` in `.GlobalEnv` \-\- this is lexical scoping

---

## Working with Environments


```r
env <- new.env(); env  # create and print a new environment
```

```
## <environment: 0x00000000183b5ea0>
```

```r
env$x <- 1; as.list(env)  # add object x to env and view its contents
```

```
## $x
## [1] 1
```

```r
env$f <- function(x, env){
  assign("x", x, envir = env);
  return(environment(NULL)) # return the current evaluation environment
}
ls(envir = env)  # list objects in env
```

```
## [1] "f" "x"
```

---

## Working with Environments


```r
env$f(100, env)  # assign 100 to x in env
```

```
## <environment: 0x000000000e1f20c0>
```

```r
get("x", env)  # retrieve value of x in env              ## 100
```

```r
env$f(10, parent.env(env))  # assign 10 to x in env's parent environment: R_GlobalEnv
```

```
## <environment: 0x000000000db730c0>
```

```r
x                                                        ## 10
```

```r
f <- function(n) parent.frame(n);
identical(f(1), eval(f(2), envir = new.env()))           ## TRUE  # R_GlobalEnv
```

--- .smallerSpacing

## Package Environments

- A loaded package, `pkgname`, has three environments associated with it
  1. "package:pkgname" - all exported objects (really pointers to the objects);
     - returned by `search()`
  1. "namespace:pkgname" - all objects (pointers), including internal ones
  1. "imports:pkgname" - objects from other packages required by `pkgname`
- These are locked: trying to add or remove bindings in them causes `error`; see [`?bindenv`](http://astrostatistics.psu.edu/su07/R/library/base/html/bindenv.html)

```r
# Function for viewing the environments associated with an attached package
GetPkgEnvirons <- function(pkgname){
 ns <- getNamespace(pkgname)
 return(list(package = as.environment(paste0("package:", pkgname)),
   namespace = ns, imports = parent.env(ns)))
}
sapply(GetPkgEnvirons("stats"), environmentIsLocked)
```

```
##   package namespace   imports 
##      TRUE      TRUE      TRUE
```

--- .smallerSpacing

## Viewing Source code

- source code can be viewed by typing the function name at the console
  - only works if function is exported by namespace on the search path
  - sometimes backticks (`) are necessary

  ```
  `::`
  ```
  
  ```
  ## function (pkg, name) 
  ## {
  ##     pkg <- as.character(substitute(pkg))
  ##     name <- as.character(substitute(name))
  ##     getExportedValue(pkg, name)
  ## }
  ## <bytecode: 0x000000000b607278>
  ## <environment: namespace:base>
  ```
- `::` - function for viewing *exported* objects from a namespace, e.g. `pkgname::FunName`
- `:::` - function for viewing *internal* objects in a namespace

--- .largerSpacing

## Viewing the sources

- My favourite R function: `getAnywhere()`
  - access any internal or external function on the search path
  - no need to specify package
  - who has time to read the help files? Learn by example\!
- Download source package (the .tar.gz one) view files in src (for C code)
- Note: `:::` should not be used in production code;
  - function is not exported for a reason, package author could change it without notice
- Source code for [R-devel  ](https://svn.r-project.org/R/trunk/)
- U. Ligges on accessing source code: [Rnews 6.4, pp. 43-45](http://cran.r-project.org/doc/Rnews/Rnews_2006-4.pdf)

--- .largerSpacing

## Classes and Methods

- *Classes* provide a template for the creation of objects
- *Methods* are the special functions of a class that work on the class's objects
- Classes in `R` can inherit features from multiple other classes
- Classes simplify code, making it easier to use, and reuse
- `S3` is `R`'s oldest, simplest, and most used way to implement OOP
  - Uses generic functions, special functions that usually only call `UseMethod`
    - Create a new generic: `MyGeneric <- function(x, ...) UseMethod("MyGeneric")`
- `S4` is newer, more elegant, and has many more features

---

## Defining `S3` Classes and Methods

* `S3` classes do not require a formal definition
  * simply change the `class` attribute of an object
    * e.g. `class(x) <- "MyClass"` or `class(x) <- c("MyClass", "data.frame")
* We can define a new method for `MyGeneric` as follows

```r
MyGeneric.MyClass <- function(x, ...){  # note the matching signature
  ...  # some expressions
}
y <- structure(list(a = 1, b = 2), class = "MyClass")  # create instance of MyClass
MyGeneric(y)  # no need to say MyGeneric.MyClass(y)
```
* `UseMethod` decides which method to dispatch based on the first argument of the generic
* Use `methods(print)` to list all the methods defined for the generic `print`
  * or `methods(class = "Date")` to list all methods defined for class `Date`

---

## S4 Classes

```r
library(fortunes); fortune(121)
```

```
## 
## Sean Davis: It got me going quickly with S4 methods, which it seems to me
## are the way to go in most cases.
## Rolf Turner: If you want to simultaneously handcuff yourself, strap
## yourself into a strait jacket, and tie yourself in knots, and moreover
## write code which is incomprehensible to the human mind, then S4 methods
## are indeed the way to go.
##    -- Sean Davis and Rolf Turner (expressing different views about the
##       benefits of S4 classes)
##       R-help (May 2005)
```
- H. Wickham on object oriented programming in `R`: [intro](http://adv-r.had.co.nz/OO-essentials.html) and [more advanced](http://vita.had.co.nz/papers/mutatr.pdf)
- [`?setRefClass`](https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html) for creating references classes - methods belong to obj., obj. are mutable

---

## Debugging

* My second favourite `R` function: `browser`
   * stops execution and allows for inspection of current environment when called
* `debug(Foo)`: calls `browser` as soon as `Foo` is entered
   * can then step through function line by line
* `recover` allows for browsing in any active function call
* `trace`: allows for insertion of arbitrary debugging code at chosen points in a function
  * `trace(Foo, edit = TRUE)` opens editor with copy of `Foo`'s code when `Foo` is called
     * changes made in the editor are then used when executed `Foo`
  * e.g. `trace(Foo, browser, exit = browser)` calls browser on entry `and` exit of `Foo`
  * to insert code at certain steps, see the `at` argument and the examples at `?trace`
  * Use `untrace(Foo)` to stop tracing `Foo`

---

## Debugging

* When developing package, you may find the following options useful:

  * `options(error = utils::recover)`
     * post\-mordem debugging of errors when they occur
  * `options(error = function() traceback(2))`
     * print call stack automatically on error
  * `options(showWarnCalls = TRUE, showErrorCalls = TRUE)`
     * Call stack is printed when a warning or error occurs
  * `options(warn = 2)`
     * all warnings are treated as errors
  * `options(prompt = "Howdy! ")`


---

## Profiling

* For determining which parts of code are slowest and use the most memory

```r
library(mgcv, quietly = TRUE)
X <- matrix(rnorm(5e5), 1e3, 5e2); y  <- rowSums(X) + rt(1e3, df = 1)
Rprof("mcgvEx.out")
invisible(gam(y ~ te(X[, 1], X[, 2]) + X[, c(-1, -2)]))
Rprof(NULL)  # turn off profiling
summaryRprof("mcgvEx.out")
```

```
## $by.self
##                      self.time self.pct total.time total.pct
## ".C"                     14.00    96.95      14.00     96.95
## "%*%"                     0.14     0.97       0.14      0.97
## "eigen"                   0.08     0.55       0.08      0.55
## "crossprod"               0.04     0.28       0.04      0.28
## "tcrossprod"              0.04     0.28       0.04      0.28
## "gam.fit"                 0.02     0.14      14.26     98.75
## "FUN"                     0.02     0.14       0.06      0.42
## "na.omit.data.frame"      0.02     0.14       0.04      0.28
## "sort.int"                0.02     0.14       0.04      0.28
## "[.data.frame"            0.02     0.14       0.02      0.14
## "double"                  0.02     0.14       0.02      0.14
## "is.na"                   0.02     0.14       0.02      0.14
## 
## $by.total
##                       total.time total.pct self.time self.pct
## "block_exec"               14.44    100.00      0.00     0.00
## "call_block"               14.44    100.00      0.00     0.00
## "doTryCatch"               14.44    100.00      0.00     0.00
## "eval"                     14.44    100.00      0.00     0.00
## "evaluate"                 14.44    100.00      0.00     0.00
## "evaluate_call"            14.44    100.00      0.00     0.00
## "force"                    14.44    100.00      0.00     0.00
## "gam"                      14.44    100.00      0.00     0.00
## "handle"                   14.44    100.00      0.00     0.00
## "ifelse"                   14.44    100.00      0.00     0.00
## "in_dir"                   14.44    100.00      0.00     0.00
## "knit"                     14.44    100.00      0.00     0.00
## "parse_page"               14.44    100.00      0.00     0.00
## "process_file"             14.44    100.00      0.00     0.00
## "process_group"            14.44    100.00      0.00     0.00
## "process_group.block"      14.44    100.00      0.00     0.00
## "slidify"                  14.44    100.00      0.00     0.00
## "try"                      14.44    100.00      0.00     0.00
## "tryCatch"                 14.44    100.00      0.00     0.00
## "tryCatchList"             14.44    100.00      0.00     0.00
## "tryCatchOne"              14.44    100.00      0.00     0.00
## "withCallingHandlers"      14.44    100.00      0.00     0.00
## "withVisible"              14.44    100.00      0.00     0.00
## "estimate.gam"             14.34     99.31      0.00     0.00
## "gam.fit"                  14.26     98.75      0.02     0.14
## "magic"                    14.02     97.09      0.00     0.00
## ".C"                       14.00     96.95     14.00    96.95
## "magic.post.proc"           0.22      1.52      0.00     0.00
## "%*%"                       0.14      0.97      0.14     0.97
## "eigen"                     0.08      0.55      0.08     0.55
## "totalPenaltySpace"         0.08      0.55      0.00     0.00
## "FUN"                       0.06      0.42      0.02     0.14
## "apply"                     0.06      0.42      0.00     0.00
## "matrix"                    0.06      0.42      0.00     0.00
## "quantile.default"          0.06      0.42      0.00     0.00
## "variable.summary"          0.06      0.42      0.00     0.00
## "crossprod"                 0.04      0.28      0.04     0.28
## "tcrossprod"                0.04      0.28      0.04     0.28
## "na.omit.data.frame"        0.04      0.28      0.02     0.14
## "sort.int"                  0.04      0.28      0.02     0.14
## ".External2"                0.04      0.28      0.00     0.00
## "model.frame"               0.04      0.28      0.00     0.00
## "model.frame.default"       0.04      0.28      0.00     0.00
## "na.omit"                   0.04      0.28      0.00     0.00
## "sort"                      0.04      0.28      0.00     0.00
## "sort.default"              0.04      0.28      0.00     0.00
## "[.data.frame"              0.02      0.14      0.02     0.14
## "double"                    0.02      0.14      0.02     0.14
## "is.na"                     0.02      0.14      0.02     0.14
## "["                         0.02      0.14      0.00     0.00
## "format_perc"               0.02      0.14      0.00     0.00
## "formatC"                   0.02      0.14      0.00     0.00
## "paste0"                    0.02      0.14      0.00     0.00
## "pmax"                      0.02      0.14      0.00     0.00
## "vapply"                    0.02      0.14      0.00     0.00
## 
## $sample.interval
## [1] 0.02
## 
## $sampling.time
## [1] 14.44
```

--- .smallerSpacing

## Timing functions

- For shorter runs, Rprof can be misleading and the `microbenchmark` package can be useful

```r
library(microbenchmark)
x <- numeric(10000)
microbenchmark(unlist(lapply(x, function(y) y + 1)), for (i in seq_along(x)) x[i] <- 1)
```

```
## Unit: milliseconds
##                                  expr      min       lq     mean   median       uq       max neval
##  unlist(lapply(x, function(y) y + 1)) 4.511981 4.829220 5.218576 4.964749 5.241842  7.643321   100
##     for (i in seq_along(x)) x[i] <- 1 7.600157 7.687542 8.199546 7.955580 8.073601 13.383509   100
```
- Need to consider garbage collection; see Radford Neal on `microbenchmark` [issues](http://radfordneal.wordpress.com/2014/02/02/inaccurate-results-from-microbenchmark/)
- See `tracemem`, `Rprofmem`, or `Rprof` with `memory.profiling` on for tracking memory usage
- Contributed packages `profr` and `proftools` provide additional anaylsis of profiling results
- See Chapter 3 of [Writing R Extensions manual](http://cran.r-project.org/doc/manuals/R-exts.html)

--- .mediumSpacing

## Creating a Package

* To start a package use function `package.skeleton`
  * specify package name, directory to create pkg in and specify objects to include in pkg
* What to put in the package is specified using one of the following arguments
  * `list` \- character vector specifying object names
  * `environment` \- an environment containing objects to add to the package
  * `code_files` \-  character vector of path names to `.R` files (sourced to create pkg)
* The result is a directory named after your package containing:
  * Directory `R` containing source code (one file per object or a copy of `code_files`)
  * Directory `man` containing outlines of documentation files for each object
  * Directory `data` for data in .rda files
  * NAMESPACE file \- objects to be imported and exported
  * DESCRIPTION file \- basic info about the package
  * Read-and-delete-me file \- do as the name suggests

--- .mediumSpacing

## The DESCRIPTION File - Example


```text
x <- read.dcf(file = system.file("DESCRIPTION", package = "splines"))
writeLines(formatDL(names(as.data.frame(x)), as.character(x), style = "list"))
```

```
## Package: splines
## Version: 3.2.0
## Priority: base
## Imports: graphics, stats
## Title: Regression Spline Functions and Classes
## Author: Douglas M. Bates <bates@stat.wisc.edu> and William N. Venables <Bill.Venables@csiro.au>
## Maintainer: R Core Team <R-core@r-project.org>
## Description: Regression spline functions and classes.
## License: Part of R 3.2.0
## Built: R 3.2.0; x86_64-w64-mingw32; 2015-02-27 03:19:04 UTC; windows
```

These are *all* the *required* fields, plus `Built` which should not be specified by author

--- .mediumSpacing

## The DESCRIPTION File \- Other fields to consider

* `Collate`: used to specify the order the `R` code files should be processed in
  * If used, all files must be given; can be OS\-specific, e.g. Collate.unix
* `Authors@R`: for machine\-readable author specification with the [`person class`](http://www.inside-r.org/r-doc/utils/person) class
* `BugReports`: A URL to send bug reports to
* `Imports`: List of packages whose namespaces you use (comma-separated, no quotes)
* `Suggests`: Packages not necessarily needed, e.g. only used in tests or vignettes
* `LinkingTo`: Package names, to make use of their `C` header files
* `Enhances`: Packages your package enhances
* `Depends`: Packages that must be _attached_ to load your package
   * `Depends` is often incorrectly used instead of `Imports`

* Can also version number requirements, including for `R` itself
  - e.g. `Depends: R (>= 3.0.0)`
* Many others. Only fields that should not be used are `Built` and `Packaged`

--- .mediumSpacing

## Package Contents: NAMESPACE File


```r
# lists possible functions for NAMESPACE file
names(parseNamespaceFile("utils", R.home("library")))
```

```
##  [1] "imports"             "exports"             "exportPatterns"      "importClasses"      
##  [5] "importMethods"       "exportClasses"       "exportMethods"       "exportClassPatterns"
##  [9] "dynlibs"             "nativeRoutines"      "S3methods"
```

* What should be exported? (user can use with `::`)
  * `export(FnctnName)`
  * `exportPattern(reg.ex)` - includes all files matching `reg.ex`
* What needs to be imported from other packages?
  * `import(pkg.name)`, `importFrom(pkg.name, fnctn.name)`
  * importing an entire package (using `imports`) should be avoided, if possible
* What compiled code needs to be included: `useDynLib(foo)`

---

## Documentation

- `LaTeX`-like help files that get converted to `LaTeX`, `HTML`, and text when pkg built
- use function `prompt` to generate an outline (skeleton) for a new function for the pkg
- Should contains examples that are checked when package is built
- Chapter 2 of Writing R Extensions Manual

--- .mediumSpacing

## Package `roxygen2` - Documentation Made Easier

- With this package, you write documentation above function definition in source file
- Function `roxygenise()` parses this code into a documentation file
- Can also create NAMESPACE and DESCRIPTION files
- Each line starts \#', then just replace LaTeX-like markup with `@`

For example:

```text
#' This is the title of the documentation file
#'
#' This is the description of the function
#' @param x a character vector
#' @return string; x - processed some way
#' @details These are the details
#' @export
#' @importFrom tools toRd
#' @seealso \code{\link{table}}
#' @examples MyFun(c("hi", "bye"))
MyFun <- function(x = "hi"){
```

---

## Additional Subdirectories of the Package Directory

* **tests** - additional code for testing the package
* demo - Code for user can see executed with `demo` function
* inst - additional data, that is not `.rda` format
* src - for compiled code

---

## Additional Helpful tools

* [`RStudio`](http://www.rstudio.com/ide/docs/packages/overview)
   * Can add a special pane for checking, building, and installing packages to its GUI
* package [`testthat`](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)
   * tools for making testing easier  (**important**)
* package [`devtools`](https://github.com/hadley/devtools)
   * Many useful functions when writing code for packages

---

## Additional Information and Getting Help

1. [Writing R Extensions manual](http://cran.r-project.org/doc/manuals/R-exts.html)
  - `system2("open", file.path(R.home("doc"), "manual", "R-exts.pdf"))`
  - Other manuals: `list.files(R.home("doc/manual"))`
3. [Tutorial](http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf)  on creating packages by R Core Team member F. Leisch
99. [Lecture notes](http://portal.stats.ox.ac.uk/userdata/ruth/APTS2012/Rcourse10.pdf) on how to create R packages by R Core member B. Ripley
8. [CRAN policies](http://cran.r-project.org/web/packages/policies.html)
10. John Chambers - [Newest book](http://www.amazon.com/Software-Data-Analysis-Programming-Statistics/dp/0387759352/), [the "Green Book" (S4)](http://www.amazon.com/Programming-Data-Language-John-Chambers/dp/0387985034/), [the "White Book" (S3)](http://www.amazon.com/Statistical-Models-John-M-Chambers/dp/0534167659/)
2. Hadley Wickham - [Advanced R Programming](http://adv-r.had.co.nz/) and the companion package [`pryr`](https://github.com/hadley/pryr)
4. Search and ask questions on [Stack Overflow](http://stackoverflow.com/) and the [R mailing lists](https://stat.ethz.ch/mailman/listinfo/r-help)
349834. `R` functions `??` or `help.search`; `RSiteSearch` to use the R-project search engine
6. [CRANberries](http://dirk.eddelbuettel.com/cranberries/cran/new/)  for new packages and updates
