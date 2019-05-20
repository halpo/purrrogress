
<!-- README.md is generated from README.Rmd. Please edit that file -->

# purrrogress

<!-- badges: start -->

<!-- badges: end -->

The goal of purrrogress is to add as simply as possible progress bars to
purrr mapping functions.

## Installation

You can install the released version of purrrogress from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("purrrogress")
```

## Using

Progress bars can be added to any map function with the
`with_progress()` function wrapped around the function to be mapped.

``` r
library(purrrogress)
## basic example code

fun <- function(...){
    # this does nothing but take time.
    Sys.sleep(0.1)
    invisible(NULL)
}
invisible(map(1:100, with_progress(fun)))
```

This example doesn’t do much but illustrates the simplicity of
purrrogress bars. The length of the progress bar is imputed from the
variable passed to the map function as well as the title and label for
the progress bar. No changes are needed to the function and all
arguments are passed on as is.

### Caveat

The `with_progress()` function can only impute the length if it is
actually part of the map call. The following will not work.

``` r
# This will not work.
not_so_fun <- with_progress(fun)
invisible(map(1:100, not_so_fun))
```

This could be made to work by specifying the length if known a priori.

``` r
# The fix
just_less_fun <- with_progress(fun, 100)
invisible(map(1:100, just_less_fun))
```

### Directly

The progress bars used by `purrrogress` are defined in a class system
described in a later section. Progress bars can be created and
manipulated directly through the `progress_bar()` function.

``` r
pb <- progress_bar(100, "A Title", "An informative label", type="none")
```

The type argument will determine what type of progress bar is created,
windows, Tk, or none(used for testing and demonstration).

# Customization

The progress bar windows can be customized to display relevent
information such as the number of elements completed, or the estimated
time remaining. This can be accomplished through inserting
[glue](https://glue.tidyverse.org/) style keywords. These keywords are
added by default:

  - `total` - the total number of elements.
  - `current` - the current number of elements completed.
  - `frac` - an alias for `"{current}/{total}"`, giving the nicely
    formatted fraction of completed elements.
  - `percent` - the percent completed as a whole number percent.
  - `elapsed.time` - The total time elapsed from the start to the
    completion of the last `step()` or `update()` call, typically the
    last element completion.
  - `average.time` - The average time to complete each step.
  - `estimated.total.time` - a naive estimate of the total time
    remaining. Taken as the `average.time * total`.
  - `estimated.time.remaining` - Just what is says,
    `estimated.total.time - elapsed.time`.
  - `etr` - alias for `estimated.time.remaining`

These keywords can be used in either the title or the label of progress
bars to obtain more informative messages.

``` r
pb <- progress_bar( 100
                  , title = "Test progress bar ({etr})"
                  , label = "{frac}({percent}) completed."
                  , initial = 50
                  )
pb$init()
Sys.sleep(2)
pb$title
#> Test progress bar (00:00:02)
pb$label
#> 50/100(50%) completed.
pb$term()
```

## Adding Bindings

In addition to those pre-specified additional bindings can be added to
show even more information.

``` r
words <- stringi::stri_rand_lipsum(1, FALSE) %>%
         stringi::stri_split(fixed = ' ') %>%
         unlist()
pb <- R6_progress$new( length(words)
                     , title = "Test Progress {current}/{total}"
                     , label = "Working on item {current}, {word}"
                     , bindings = list(word = ~words[pb$current+1])
                     )
pb$init()
pb$label
#> Working on item 0, Maecenas
pb$step()
pb$label
#> Working on item 1, ac
pb$step()
pb$label
#> Working on item 2, eget
pb$term()
```

# Class System

Additional progress bars may be defined to work within the `purrrogress`
framework, however each must inherit from the base progress class, “R6
Progress Base Class” which handles the creation and management of the
active bindings for titles and labels.

#### Public Methods to Implement.

The following are public methods for which a derived class **must**
implement.

  - **`init()`** - This is called at the begining of a loop or apply
    function. It shouldcontain the code to actually create and show the
    progress bar window. Variables that are used to monitor and control
    the progress windows through other steps should be initialized here.
    It should additionally call `super$init()` to start timers. The
    return value should be `invisible(self)`.
  - **`term()`** - This is called to close any open windows, close
    connections and free resources. After `term()` is called the
    progress object should be unusable. There is no need to call
    `super$term()` at this time but is it provided for good practice.
    The return value is expected to be `invisible(NULL)`.
  - **`update(...)`** - This takes any number of arguments which may be
    used to update internal variables or displays. This should handle
    updating of the progress window, titles, lables, etc. The return
    value is expected to be `invisible(self)`.

#### Other Public Methods

The following are functions that are provided in the public interface
for R6 progress bars but do not need to be implemented in child classes,
or when implemented care should be taken to carry forward the default
behavior.

  - **`step(n=1L, ..., keep.open=FALSE)`** - This is called to increment
    the internal progress counter, it in turn calls `update(...)`
    function the update any windows, titles and labels. The `n` argument
    is provided to allow for taking uneven steps. The `keep.open`
    argument is provided to allow for windows that stay open to show
    relevent information, such as total time or average time per step.
    If `keep.open` is false (default) when the current counter reaches
    the total `term()` will be called.
  - **`initialize(...)`** - populates the initial values of `title`,
    `label`, and `initial` count, which cannot be changed once
    specified.
  - **`add_binding()`** - adds an active binding which may be used in
    the title or label.
  - **`add_bindings(...)`** - a more convenient way to specify multiple
    bindings at once in the form of `add_bindings(name=funtion(){...})`.
  - **`expose(sym)`** - The most convenient way of binding variable to
    use in the title or label. When called the variable from the current
    scope is added to the available bindings so that it may be
    referenced in the title or label.

# Acknowledgements

This project was insipred by a post to
[R-Bloggers](https://www.r-bloggers.com/purrring-progress-bars-adding-a-progress-bar-to-purrrmap/)
by Adi Sarid. Credit goes to him for the original idea on which
`purrrogress` is built.
