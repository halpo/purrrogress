#' @import purrr
NULL

get_root_call_symbol <- function(call){
    assert_that(is.call(call))
    while (is.call(call)) {
        if ( call[[1]] == '::'){
            call <- call[[3]]
        } else
            call <- call[[1]]
    }
    return(call)
}
if(FALSE){
    call <- substitute(purrr::pmap(list(letters, Letters), ~paste(.x, "->", .y)))
    expect_identical(get_root_call_symbol(call), rlang::sym('pmap'))

    call <- substitute(pmap(list(letters, Letters), ~paste(.x, "->", .y)))
    expect_identical(get_root_call_symbol(call), rlang::sym('pmap'))
}

all_calls <- function(x){
    if (is.function(x)) x<- body(x)
    if(!any(map_lgl(as.list(x), is.call))) return(character(0))

    calls <- keep(as.list(x), is.call)
    c( as.character(map(calls, getElement, 1L))
     , unlist(map(calls, all_calls))
     )
}
if(FALSE){#@testing
    fun <- purrr::imap
    expect_equal( sort(all_calls(fun))
                , c("`<-`", 'as_mapper', 'map2', 'vec_index'))
}

dot_calls <- function(fun, cfun){
    calls <- keep(as.list(body(fun)), is.call)
    if(!any(. <- as.character(map(calls, get_root_call_symbol)) == '.Call'))
        return(FALSE)
    any(as.character(map(calls[.], getElement, 2L)) %in% cfun)
}
if(FALSE){
    fun <- purrr::pmap
    cfun <- "pmap_impl"
    expect_true(dot_calls(purrr::pmap, "pmap_impl"))
    expect_false(dot_calls(purrr::pmap, "map_impl"))
    expect_true(dot_calls(purrr::map, "map_impl"))
}

#' Check if a function is a map2 derived function
#'
#' Besides the obvious [map2] and `map2_*` variants,
#' this also covers functions based off `map2`:
#' * [imap] and `imap_*` variants.
#' * [invoke_map] and `[invoke_map_*]` variants.
#'
#' @param fun function to test.
is_purrr_map2_fun <- function(fun) dot_calls(fun, 'map2_impl')
is_purrr_pmap_fun <- function(fun) dot_calls(fun, 'pmap_impl')
is_purrr_map_fun  <- function(fun) dot_calls(fun, 'map_impl')

is_any_purrr_map_fun <-
function(fun){
    bod <- as.list(body(fun))
    if('map' %in% all.names(body(fun), TRUE)) return(TRUE)
    any(purrr::map_lgl( bod, ~is.call(.) && .[[1]] == ".Call" && grepl(".*map.*_impl", deparse(.[[2]]))))
}

is_purrr_map_call <- function(call){
    f <- call[[1]]
    if (is.call(f)) {
        if ( f[[1]] == '::'
          && f[[2]] == 'purrr'
           ){
            f <- f[[3]]
        } else return(FALSE)
    }
    if (!is.symbol(f)) return(FALSE)
    grepl('map', f)
}
if(FALSE){#@testing
    vals <- purrr::map_lgl(1:2, function(x)
        is_purrr_map_call(sys.call(sys.parent(1)))
    )
    expect_true(all(vals))

    v2 <- sapply(1:2, function(x)
        is_purrr_map_call(sys.call(sys.parent(1)))
    )
    expect_false(any(v2))
}

is_purrr_frame <- function(frame){
    if (is.list(frame))
        return(purrr::map_lgl(frame, is_purrr_frame))
    getPackageName(topenv(frame)) == 'purrr'
}
if(FALSE){#@testing
    vals <- purrr::map_lgl(1:2, function(x)
        is_purrr_frame(sys.frame(sys.parent(1)))
    )
    expect_true(all(vals))

    v2 <- sapply(1:2, function(x)
        is_purrr_frame(sys.frame(sys.parent(1)))
    )
    expect_false(any(v2))
}

in_purrr_map <-
function( which = seq.int(sys.nframe())
        , calls = sys.calls()[which]
        , frames = sys.frames()[which]
        ){
    assert_that( length(which) == length(calls)
               , length(which) == length(frames)
               )
    i <- which[base::which(is_purrr_frame(frames))]
    if (any(i)) i <- i[map_lgl(calls[i], is_purrr_map_call)]
    if (any(i)) i <- i[map_lgl(map(i, sys.function), is_any_purrr_map_fun)]
    if (length(i)) max(i) else FALSE
}
if(FALSE){#@testing
    vals <- purrr::map_lgl(1:1, function(x){
        # which <- sys.parent(1):sys.nframe()
        # calls <- sys.calls()[which]
        # frames <- sys.frames()[which]
        # in_purrr_map(which, calls, frames) == sys.parent(1)
        in_purrr_map() == sys.parent(1)
    })
    expect_true(all(vals))

    v2 <- sapply(1:2, function(x)
        in_purrr_map( sys.parent(1):sys.nframe()) == sys.parent()
    )
    expect_false(any(v2))
}
if(FALSE){#@testing in_purrr_map nested.
    purrr::map(1:1, function(x){
        parent <- sys.parent()
        inner.vals <- purrr::map_int(1:2, function(...)
            in_purrr_map()
        )
        expect_true(all(inner.vals > parent))

        me.val <- in_purrr_map()
        expect_equal(me.val, parent)
    })
}

with_purrr_progress <-
function( i = in_purrr_map()
        , title = NULL
        , ...
        , fun){
    purrr.frame <- sys.frame(i)
    sys.call(i)[[1]]

    if (exists('.x', envir = purrr.frame)) {
        if (exists('.y', envir = purrr.frame))
            total <- max( length(get('.x', envir = purrr.frame))
                        , length(get('.y', envir = purrr.frame))
                        )
        else
            total <- length(get('.x', envir = purrr.frame))
    } else
    if (exists('.l', envir = purrr.frame)) {
        total <- max(map_int(get('.l', envir=purrr.frame), length))
    } else pkg_error("could not determine length.")

    if(is.null(title)){
        call <- sys.call(i)
        if (!is.name(call[[2]])) {
            title <- deparse(as.call(c(as.list(call[1]), alist(...))))
        } else if (!is.name(call[[3]])){
            title <- deparse(as.call(c(as.list(call[1:2]), as.name('...'))))
        } else {
            title <- paste(sQuote(call[[1]]), "progress")
        }
    }
    pb <- progress_bar(total = total, title=title, ...)
    push_progress(pb, "with_progress")
    eval(quote(on.exit(quote(.GlobalEnv$pop_progress("with_progress")))), sys.frame(i))
    pb$init()
    function(...){
        pb$update()
        on.exit(pb$step())
        fun(...)
    }
}
if(FALSE){#@testing
    purrr::map_lgl(1:5, with_progress(test_progress_status, type='none')
                  , 5
                  , "purrr::map(...)"
                  , "\\d+/\\d+ items completed"
                  , class = "R6 Progress Base Class"
                  )

    purrr::map_lgl(1:5, with_progress( test_progress_status, type='none'
                                     , total = 10
                                     , title = "Mapping progress"
                                     , label = "{elapsed.time}/{estimated.total.time} this will take forever"
                                     )
                  , total = 10
                  , "Mapping progress"
                  , "this will take forever"
                  , class = "R6 Progress Base Class"
                  )

    purrr::pmap_lgl(list(1:5), with_progress( test_progress_status, type='none'
                                            , title = "pmap progress"
                                            )
                   , total = 5
                   )
}
