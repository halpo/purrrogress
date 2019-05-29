in_call <-
function( consider
        , calls = sys.calls()
        ){
    call.syms <- get_call_symbols(calls)
    j <- which(call.syms %in% consider)
    if (length(j)) max(j) else FALSE
}


if(FALSE){
    call.syms <- map(calls, `[[`, 1) %>% map(deparse) %>% map_chr(head, 1)

    i <- which(call.syms == 'map2')
    j <- which(call.syms == 'mapply')

    sys.function(i-1)
    sys.function(i)
    sys.function(i+1)
    topenv(frames[[11]])

    frame <- frames[[j]]
    ls(frame, all=TRUE)

    eval(quote(length(..1)), envir=frame)
}
get_call_symbol <- function(call){
    if(is.symbol(call)) return(deparse(call))
    if(is.call(call)){
        if ( length(call) == 3
          && is.name(call[[1]])
          && call[[1]] == '::')
            return(get_call_symbol(call[[3]]))
        else return(get_call_symbol(call[[1]]))
    }
    return(NA_character_)
}

get_call_symbols <- function(calls = sys.calls()){
    purrr::map_chr(calls, get_call_symbol)
}

base.apply.calls <- c('mapply', 'apply', 'sapply', 'tapply', 'lapply')
in_apply_call <- function(calls = sys.calls(), ...){
    in_call(base.apply.calls)
}

get_apply_length <-
function( i = in_apply_call()
        , calls = sys.calls()
        , frames = sys.frames()
        ){
    switch( get_call_symbols(calls)[[i]]
          , mapply = eval(quote(length(..1)), envir = frames[[i]])
          , lapply = eval(quote(length(X)), envir = frames[[i]])
          , sapply = eval(quote(length(X)), envir = frames[[i]])
          , tapply = eval(quote(length(X)), envir = frames[[i]])
          ,  apply = eval(quote(sum(dim(X)[MARGIN])), envir = frames[[i]])
          )

}

with_apply_progress <-
function(i = in_apply_call(), title=NULL, ..., fun){
    total <- get_apply_length(i)
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
    push_progress(pb, 'with_progress')
    pb$init()
    function(...){
        pb$update()
        on.exit(pb$step())
        fun(...)
    }
}
if(FALSE){#@testing
    val <- sapply( 1:5, with_progress( test_progress_status
                                     , label="{frac} items completed"
                                     , type="none")
                 , total=5
                 , title = "sapply"
                 , label = "\\d/5 items completed")
    expect_true(all(val))
}

