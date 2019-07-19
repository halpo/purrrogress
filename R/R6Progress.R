#' @import pkgcond
#' @importFrom assertthat is.flag is.string is.count is.number
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
                             , label = "{frac} items completed"
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
            if(!private$.initialized.){
                private$.start.time. <- proc.time()
                private$.initialized. <- TRUE
            }
            invisible(self)
        },
        term = function(){invisible(NULL)},
        update = function(...){},
        step = function(n=1L, ..., keep.open = FALSE){
            if (!private$.initialized.) self$init()
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
            if ( name %in% ls(self, all.names = TRUE)
               | name %in% ls(private$bindings, all.names = TRUE)
               ) {
                if (isFALSE(overwrite))
                    pkg_error( sQuote(name) %<<% "already exists."
                             , type = "already-exists")
                if (is.na(overwrite))
                    pkg_warning(sQuote(name) %<<% "already exists." %<<%
                                "Overwriting previous value."
                               , type = "already-exists")
                if (name %in% ls(private$bindings, all.names = TRUE))
                    rm(list=name, envir = private$bindings)
            }
            makeActiveBinding(name, fun, private$bindings)
            return(invisible(self))
        },
        add_bindings = function(..., overwrite=NA){
            private$.add_bindings(list(...), overwrite=overwrite)
        },
        expose = function(..., env=parent.frame(), overwrite=NA){
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
            (proc.time() - private$.start.time.)['elapsed'] %>%
                as.numeric() %>%
                hms::as_hms() %>%
                hms::round_hms(1)
        },
        average.time = function(){
            if (private$.current. == 0) return(hms::as_hms(NA_integer_))
            as.numeric((proc.time() - private$.start.time.)['elapsed']/private$.current.) %>%
                hms::as_hms() %>% hms::round_hms(1)
        },
        estimated.total.time = function(){
            if (private$.current. == 0) return(hms::as_hms(NA_integer_))
            hms::round_hms(hms::as_hms(as.numeric((proc.time() - private$.start.time.)['elapsed']/private$.current.*private$.total.)), 1)
        },
        estimated.time.remaining = function(){
            if (private$.current. == 0) return(hms::as_hms(NA_integer_))
            hms::round_hms(hms::as_hms(as.numeric(self$estimated.total.time - self$elapsed.time)), 1)
        },
        etr = function(){self$estimated.time.remaining},
        percent = function(){sprintf("%d%%", floor(private$.current./private$.total.*100))}
    )},
    private = {list(
        .title. = "Progress",
        .label. = '{frac}',
        .total. = 0L,
        .current. = 0L,
        .start.time. = NULL,
        .initialized. = FALSE,
        bindings = NULL,
        .add_bindings = function(.list, overwrite=NA){
            purrr::imap(.list, self$add_binding, overwrite=overwrite)
            return(invisible(self))
        },
        .expose = function(var, name, env, overwrite=NA){
            fun <- eval(substitute(function()var, list(var=var)), env)
            self$add_binding(fun, name, overwrite=overwrite)
        }
    )}
)
if(FALSE){#@testing
    test <- R6_progress$new(100)

    expect_equal(test$title, "Progress")
    expect_equal(test$label, "0/100 items completed")

    expect_true(is.na(test$etr))
    expect_identical(test$init(), test)
    Sys.sleep(0.1)
    expect_true(is.na(test$etr))
    expect_equal(test$current, 0L)

    expect_identical(test$step(), test)
    expect_equal(test$current, 1L)
    expect_false(is.na(test$etr))


    expect_equal(test$percent, "1%")
    expect_null(test$term())

    words <- stringi::stri_rand_lipsum(1, FALSE) %>%
             stringi::stri_split(fixed = ' ') %>%
             unlist()
    i <- 1
    pb <-
        R6_progress$new( length(words)
                       , "Test Progress {current}/{total} ({estimated.time.remaining} remaining.)"
                       , "{elapsed.time}/{estimated.total.time} estimated.\n {word}"
                       , bindings = list(word = ~words[i])
                       , expose = 'i'
                       )

    expect_identical(pb$total, length(words))
    expect_identical(pb$current, 0L)
    expect_equal(pb$title, "Test Progress 0/" %<<<% length(words) %<<<% " (NA remaining.)")
    expect_equal(pb$label, "NA/NA estimated.\n" %<<<% words[[1]])
    expect_equal(pb$frac, "0/" %<<<% length(words))

    pb$step()
    Sys.sleep(1)
    et <- pb$elapsed.time
    expect_is(et, 'hms')
    expect_true(et >= 1)

    at <- pb$average.time
    expect_is(at, 'hms')
    expect_true(at >= 1)

    expect_error(pb$expose(i, overwrite = FALSE)
                , class = "purrrogress-error-already-exists")
    expect_warning(pb$expose(i, overwrite = NA)
                  , class = "purrrogress-warning-already-exists")
    expect_silent(pb$expose(i, overwrite = TRUE))

    pb$add_bindings(next_word = function()words[[pb$current+2]])


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
                             , show.after = 0 # Number of seconds to pass before showing progress bar.
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
            if (private$.show.after. <= 0){
                private$.min.time.needed.to.show. <- 0L
                self$init()
            } else {
                private$.min.time.needed.to.show. <- min.time
            }
        },
        # nocov start
        update = function(label, ...){
            if (!missing(label)){
                assert_that(is.string(label))
                self$.label. <<- label
            }
            self$show()
            if (!is.null(private$.pb.))
                if (private$.current. < private$.total.) {
                    utils::setWinProgressBar( pb = private$.pb.
                                            , value = private$.current.
                                            , title = self$title
                                            , label = self$label
                                            )
                } else {
                    utils::setWinProgressBar( pb = private$.pb.
                                            , value = private$.total.
                                            , title = self$title
                                            , label = self$final
                                            )
                }
            return(invisible(self))
        },
        init = function(){
            super$init()
            self$show()
            return(invisible(self))
        },
        term = function(){
            if(!is.null(private$.pb.)) close(private$.pb.)
            private$.pb. <- NULL
            invisible(NULL)
        },
        show = function(){
            if ( is.null(private$.pb.)
               & self$elapsed.time >= private$.min.time.needed.to.show.
               & ( is.na(self$estimated.total.time)
                 | self$estimated.total.time >= private$.min.time.needed.to.show.
                 )
               )
                private$.pb. <-
                    utils::winProgressBar( title = self$title
                                         , label = self$label
                                         , min = 0
                                         , max = private$.total.
                                         , initial = private$.current.
                                         , width = private$.width.
                                         )
        }
        # nocov end
    ),
    active = list(
        final = function()glue::glue(private$.final., .envir = self)
    ),
    private = list(
        .pb. = NULL,
        .width. = 500,
        .final.= "Finalizing",
        .show.after. = 1L,
        .min.time.needed.to.show. = 5L
    )
)

# Testing --------------------------------------------------------------
if(FALSE){#@testing
    words <- stringi::stri_rand_lipsum(1, FALSE) %>%
             stringi::stri_split(fixed = ' ') %>%
             unlist()

    i <- 1
    pb <-
        R6_win_progress$new( length(words)
                           , "Test Progress {current}/{total} ({estimated.time.remaining} remaining.)"
                           , "{elapsed.time}/{estimated.total.time} estimated.\n {word}"
                           , width = 600
                           , bindings = list(word = ~words[i])
                           , show.after=2
                           )

    expect_identical(pb$total, length(words))
    expect_identical(pb$current, 0L)
    expect_equal(pb$title, "Test Progress 0/" %<<<% length(words) %<<<% " (NA remaining.)")
    expect_equal(pb$label, "NA/NA estimated.\n" %<<<% words[[1]])
    expect_equal(pb$frac, "0/" %<<<% length(words))

    pb$init()
    pb$elapsed.time
}
if(FALSE){# Manual testing
    debug(pb$init)
    debug(pb$update)

    pb$init()


    pb <- R6_win_progress$new(100, show.after = 0)
    pb$term()
}
