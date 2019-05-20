#' @import pkgcond
#' @importFrom assertthat is.flag is.string is.count is.number
#' @importFrom hms as.hms round_hms
#' @importFrom glue glue
#' @importFrom testextra are
NULL


# R6 Virtual Progress Bar Base Class ===================================

#' Base Progress bar Class
#'
#' This is the base class for all R6 progress bars.
#' It also doubles as a null progress bar that displays no progress bar,
#' but allows for checking values.
#'
#' @export
R6_progress <- R6::R6Class("R6 Progress Base Class",
    public = {list(
        initialize = function( total
                             , title = "Progress"
                             , label = "{frac}"
                             , initial = 0L
                             , ...
                             , bindings = list()
                             , expose = character(0)
                             ){
            assert_that( is.count(total)
                       , is.string(title)
                       , is.string(label)
                       , is.count(initial) || identical(initial, 0L)
                       )
            private$.total. <- as.integer(total)
            private$.title. <- title
            private$.label. <- label
            private$.current. <- as.integer(initial)

            private$bindings <- new.env(hash = TRUE, parent = as.environment(self))
            if (!missing(bindings) && length(bindings)){
                assert_that( is.list(bindings)
                           , rlang::is_named(bindings)
                           , all(are(bindings, c('function', 'formula')))
                           )

                private$.add_bindings(bindings, overwrite=NA)
            }
            if (!missing(expose) && length(expose)){
                assert_that(is.character(expose)
                           , all(nchar(expose) > 0)
                           )
                purrr::map2(rlang::syms(expose), expose, private$.expose, parent.frame())
            }
        },
        init = function(){
            if (is.null(private$.start.time.))
                private$.start.time. <- proc.time()
            invisible(self)
        },
        term = function(){invisible(NULL)},
        update = function(...){},
        step = function(n=1L, ..., keep.open = FALSE){
            self$init()
            private$.current. <- private$.current. + n
            if (keep.open || private$.current. < private$.total.)
                self$update(...)
            else
                self$term()
            return(invisible(self))
        },
        add_binding = function(fun, name, overwrite=NA){
            if (is(fun, 'formula')) fun <- rlang::as_function(fun)
            assert_that( is.function(fun)
                       , rlang::is_string(name)
                       , is.flag(overwrite)
                       )
            if ( name %in% ls(self, all=TRUE)
               | name %in% ls(private$bindings, all=TRUE)
               ) {
                if (isFALSE(overwrite))
                    pkg_error(sQuote(name) %<<% "already exists.")
                if (is.na(overwrite))
                    pkg_warning(sQuote(name) %<<% "already exists." %<<%
                                "Overwriting previous value.")
                if (name %in% ls(private$bindings, all=TRUE))
                    rm(list=name, envir = private$bindings)
            }
            makeActiveBinding(name, fun, private$bindings)
            return(invisible(self))
        },
        add_bindings = function(..., overwrite=NA){
            private$.add_bindings(list(...), overwrite=overwrite)
        },
        expose = function(..., env=parent.frame(), .overwrite=NA){
            c <- rlang::ensyms(...)
            assert_that(all(map_lgl(c, is.symbol)))
            names(c) <- ifelse(nchar(names(c)) > 0, names(c), as.character(c))
            imap(c, env=env, private$.expose, overwrite=overwrite)
            return(invisible(self))
        }
    )},
    active = {list(
        title = function()glue::glue(private$.title., .envir = private$bindings),
        label = function()glue::glue(private$.label., .envir = private$bindings),
        total = function(){private$.total.},
        current = function(){private$.current.},
        frac = function(){paste0(private$.current., '/', private$.total.)},
        elapsed.time=function(){
            (proc.time() - private$.start.time.)['elapsed'] %>% hms::as.hms() %>%
                hms::round_hms(1)
        },
        average.time = function(){
            ((proc.time() - private$.start.time.)['elapsed']/private$.current.) %>%
                hms::as.hms() %>% hms::round_hms(1)
        },
        estimated.total.time = function(){
            round_hms(as.hms((proc.time() - private$.start.time.)['elapsed']/private$.current.*private$.total.), 1)
        },
        estimated.time.remaining = function(){
            round_hms(as.hms(self$estimated.total.time - self$elapsed.time), 1)
        },
        etr = function(){self$estimated.time.remaining},
        percent = function(){sprintf("%d%%", round(private$.current./private$.total.*100))}
    )},
    private = {list(
        .title. = "Progress",
        .label. = '{frac}',
        .total. = 0L,
        .current. = 0L,
        .start.time. = NULL,
        .show.after. = 1L,
        .min.time.needed.to.show. = 5L,
        bindings = NULL,
        .add_bindings = function(.list, overwrite=NA){
            purrr::imap(.list, self$add_binding, overwrite=overwrite)
            return(invisible(self))
        },
        .expose = function(var, name, env, overwrite=NA){
            fun <- eval(substitute(function()var, list(var=var)), env)
            self$.add_binding(fun, name, .overwrite=.overwrite)
        }
    )}
)
if(FALSE){
    test <- R6_progress$new(100)

    expect_equal(test$title, "Progress")
    expect_equal(test$label, "0/100")

    expect_true(is.na(test$etr))
    expect_identical(test$init(), test)
    Sys.sleep(0.1)
    expect_false(is.na(test$etr))
    expect_equal(test$current, 0L)

    expect_identical(test$step(), test)
    expect_equal(test$current, 1L)

    words <- stringi::stri_rand_lipsum(1, FALSE) %>%
             stringi::stri_split(fixed = ' ') %>%
             unlist()
    pb <-
        R6_progress$new( length(words)
                       , "Test Progress {current}/{total} ({estimated.time.remaining} remaining.)"
                       , "{elapsed.time}/{estimated.total.time} estimated.\n {word}"
                       , bindings = list(word = ~words[i])
                       )
    i <- 1

    expect_identical(pb$total, length(words))
    expect_identical(pb$current, 0L)
    expect_equal(pb$title, "Test Progress 0/" %<<<% length(words) %<<<% " (NA remaining.)")
    expect_equal(pb$label, "NA/NA estimated.\n" %<<<% words[[1]])
    expect_equal(pb$frac, "0/" %<<<% length(words))

    pb$step()
    Sys.sleep(1)
    et <- pb$elapsed.time
    expect_is(et, 'hms')
    expect_true(et==1)

}


# R6 Windows Progress Bar ==============================================
R6_win_progress <- R6::R6Class("R6 Windows Progress Bar",
    inherit = R6_progress,
    public  = list(
        initialize = function( total
                             , title = "Progress"
                             , label = ""
                             , label.final = "Finalizing"
                             , initial = 0L
                             , width = 500L
                             , show.after = 1 # Number of seconds to pass before showing progress bar.
                             , min.time = show.after * 5 # Show only if expected time (in seconds) is greater than max.time.
                             , ...
                             ){
            assert_that( is.string(label.final)
                       , is.count(width)
                       , is.number(show.after)
                       , is.number(min.time)
                       )
            super$initialize(total, title, label, initial, ...)
            private$.final. <- label.final
            private$.width. <- as.integer(width)
            private$.show.after. <- show.after
            private$.min.time.needed.to.show. <- min.time

            if (private$.show.after. <= 0) self$init()
        },
        update = function(label, ...){
            if (!missing(label)){
                assert_that(is.string(label))
                self$.label. <<- label
            }
            private$init()
            if (!is.null(private$.pb.))
                if (private$.current. < private$.total.) {
                    utils::setWinProgressBar( pb = private$.pb.
                                            , value = private$.current.
                                            , title = glue::glue(self$.title., .envir = self)
                                            , label = glue::glue(self$.label., .envir = self)
                                            )
                } else {
                    utils::setWinProgressBar( pb = private$.pb.
                                            , value = private$.total.
                                            , title = glue::glue(private$.title., .envir = self)
                                            , label = glue::glue(private$.final., .envir = self)
                                            )
                }
            return(invisible(self))
        },
        init = function(){
            super$init()
            if(is.null(private$.pb.) && elapsed.time > min.time)
                private$.pb. <-
                    utils::winProgressBar( title = self$title
                                         , label = self$label
                                         , min = 0
                                         , max = private$.total.
                                         , initial = private$.current.
                                         , width = private$.width.
                                         )
            return(invisible(self))
        },
        term = function(){
            if(!is.null(private$.pb.)) close(private$.pb.)
            private$.pb. <- NULL
            invisible(NULL)
        }
    ),
    active = list(
        final = function()glue::glue(private$.final., .envir = self)
    ),
    private = list(
        .pb. = NULL,
        .width. = 500,
        .final.= "Finalizing"
    )
)

# Testing --------------------------------------------------------------
if(FALSE){
    words <- stringi::stri_rand_lipsum(1, FALSE) %>%
             stringi::stri_split(fixed = ' ') %>%
             unlist()

    pb <-
        R6_win_progress$new( length(words)
                           , "Test Progress {current}/{total} ({estimated.time.remaining} remaining.)"
                           , "{elapsed.time}/{estimated.total.time} estimated.\n {word}"
                           , width = 600
                           , bindings = list(word = ~words[i])
                           )
    i <- 1

    expect_identical(pb$total, length(words))
    expect_identical(pb$current, 0L)
    expect_equal(pb$title, "Test Progress 0/" %<<<% length(words) %<<<% " (NA remaining.)")
    expect_equal(pb$label, "NA/NA estimated.\n" %<<<% words[[1]])
    expect_equal(pb$frac, "0/" %<<<% length(words))

    pb$init()
    pb$elapsed.time


}