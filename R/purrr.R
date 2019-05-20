is_purrr_map_fun <-
function( fun){
    bod <- as.list(body(fun))
    if('map' %in% all.names(body(fun), TRUE)) return(TRUE)
    map_lgl( bod, ~is.call(.) && .[[1]] == ".Call" && grepl(".*map.*_impl", deparse(.[[2]]))
           ) %>% any()
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
function( calls = sys.calls()
        , frames = sys.frames()
        ){
    i <- which(is_purrr_frame(frames))
    if (any(i)) i <- i[purrr::map_lgl(calls[i], is_purrr_map_call)]
    if (any(i)) i <- i[purrr::map(i, sys.function) %>% map_lgl(is_purrr_map_fun)]
    if(length(i)) max(i) else FALSE
}
if(FALSE){#@testing
    vals <- purrr::map_lgl(1:2, function(x){
        in_purrr_map( sys.calls()[sys.parent(1):sys.nframe()]
                    , sys.frames()[sys.parent(1):sys.nframe()]
                    ) == sys.parent()
    })
    expect_true(all(vals))

    v2 <- sapply(1:2, function(x)
        in_purrr_map( sys.calls()[sys.parent(1):sys.nframe()]
                    , sys.frames()[sys.parent(1):sys.nframe()]
                    ) == sys.parent()
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
function( i = find_purrr_frame()
        , title = NULL
        , ...
        , fun){
    purrr.frame <- sys.frame(i)
    stopifnot(exists('.x', envir = purrr.frame))
    total <- length(get('.x', envir = purrr.frame))

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
    function(...){
        pb$update()
        on.exit(pb$step())
        fun(...)
    }
}
if(FALSE){#@testing
    purrr::map_lgl(1:5, with_progress(test_progress_status, type='none')
                  , 5
                  , "purrr::map\\(\\.\\.\\.\\)"
                  , "\\d+/\\d+ items completed\\t(.+) remaining"
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
}
