
progress_bar <- function( total
                        , title = "Progress"
                        , label = ""
                        , label.final = "Finalizing"
                        , initial = 0L
                        , show = FALSE
                        , min.time = 1 # Minimum number of seconds to show
                        , ...){
    .self <- environment()
    start.time <- proc.time()
    current <- initial
    makeActiveBinding("frac", function(){paste(current, '/', total)}, .self)
    makeActiveBinding("elapsed.time", function(){
        (proc.time() - start.time)['elapsed'] %>% hms::as.hms() %>%
            hms::round_hms(1)
    }, .self)
    makeActiveBinding("average.time", function(){
        ((proc.time() - start.time)['elapsed']/current) %>%
            hms::as.hms() %>% hms::round_hms(1)
    }, .self)
    makeActiveBinding("estimated.total.time", function(){
        (hms::round_hms(hms::as.hms((proc.time() - start.time)['elapsed']/current*total), 1))
    }, .self)
    makeActiveBinding("estimated.time.remaining", function(){
        (estimated.total.time - elapsed.time) %>%
            hms::as.hms() %>% hms::round_hms(1)
    }, .self)
    makeActiveBinding("etr", function()estimated.time.remaining, .self)
    makeActiveBinding("percent", function()sprintf("%d%%", round(current/total*100)), .self)

    pb <- NULL
    init = function(){
        if(is.null(pb) && elapsed.time > min.time)
            pb <<- utils::winProgressBar(title=title, label = label, 0, total, current, ...)
        return(invisible(.self))
    }
    step = function(n=1L, keep.open = FALSE){
        init()
        current <<- current + n
        if (keep.open || current < total)
            update()
        else
            term()
        return(invisible(.self))
    }
    update = function(label=NULL){
        if (!missing(label))
            .self$label <<- label
        init()
        if (!is.null(pb))
            if (.self$current < .self$total) {
                utils::setWinProgressBar( pb, .self$current
                                        , title = glue::glue(.self$title, .envir = .self)
                                        , label = glue::glue(.self$label, .envir = .self)
                                        )
            } else {
                utils::setWinProgressBar( pb, .self$total
                        , title = glue::glue(.self$title, .envir = .self)
                        , label = glue::glue(.self$label.final, .envir = .self)
                        )
            }
        return(invisible(.self))
    }
    term = function(){
        if(!is.null(pb)) close(pb)
        pb <<- NULL
        invisible(NULL)
    }
    .add_binding <- function(fun, name, .overwrite=FALSE){
        assert_that( is.function(fun)
                   , rlang::is_string(name)
                   , .overwrite || !(name %in% ls(.self))
                   )
        if (.overwrite && (name %in% ls(.self)))
            rm(list=name, envir=.self)
        makeActiveBinding(name, fun, .self)
        return(invisible(.self))
    }
    .add_bindings <- function(bindings, .overwrite=FALSE){
        purrr::imap(bindings, .add_binding, .overwrite=.overwrite)
        return(invisible(.self))
    }
    add_bindings <- function(..., .overwrite=FALSE){
        .add_bindings(list(...), .overwrite=.overwrite)
    }
    .expose <- function(var, name = as.character(var), env = parent.frame(), .overwrite=FALSE){
        assert_that(is.symbol(var))
        fun <- eval(substitute(function()var, list(var=var)), env)
        .add_binding(fun, name, .overwrite=.overwrite)
    }
    expose <- function(..., env = parent.frame(), .overwrite=FALSE){
        c <- ensyms(...)
        names(c) <- ifelse(nchar(names(c)) > 0, names(c), as.character(c))
        imap(c, env=env, .expose, .overwrite=.overwrite)
        return(invisible(.self))
    }
    if (show) init()
    return(.self)
}
if(F){
    words <- stringi::stri_rand_lipsum(1, FALSE) %>%
             stringi::stri_split(fixed = ' ') %>%
             unlist()
    pb <- progress_bar( length(words)
                      , "Test Progress {current}/{total} ({estimated.time.remaining} remaining.)"
                      , "{elapsed.time}/{estimated.total.time} estimated.\n {word}"
                      , width = 600)
    pb$add_bindings(word = function()words[i+1])

    debug(pb$.expose)
    debug(pb$expose)

    pb$expose(j=i, i)

    for(i in seq_along(words)) {
        Sys.sleep(0.1)
        pb$step(keep.open = TRUE)
    }



    pb$term()
}


tk_progress_bar <- function( total
                           , title = "Progress"
                           , label = ""
                           , initial = 1L
                           , min = initial
                           , ...){
    .self <- environment()
    start.time <- proc.time()
    current <- initial
    close.if.done <- TRUE
    makeActiveBinding("elapsed.time", function(){
        (proc.time() - start.time)['elapsed'] %>% hms::as.hms() %>%
            hms::round_hms(1)
    }, .self)
    makeActiveBinding("average.time", function(){
        ((proc.time() - start.time)['elapsed']/current) %>%
            hms::as.hms() %>% hms::round_hms(1)
    }, .self)
    makeActiveBinding("estimated.total.time", function(){
        (hms::round_hms(hms::as.hms((proc.time() - start.time)['elapsed']/current*total), 1))
    }, .self)
    makeActiveBinding("estimated.time.remaining", function(){
        (estimated.total.time - elapsed.time) %>%
            hms::as.hms() %>% hms::round_hms(1)
    }, .self)
    makeActiveBinding("percent", function()sprintf("%d%%", round(current/total*100)), .self)

    pb <- utils::tkProgressBar( title=title
                              , label = label
                              , min=min, max=total, initial=current, ...)
    step = function(n=1L, close.if.done = FALSE){
        current <<- current + n
        if(close.if.done && current == total)
            utils::setTkProgressBar( pb, current
                                   , title = glue::glue(title, .envir = .self)
                                   , label = glue::glue(label, .envir = .self)
                                   )
        else
            term()
    }
    term = function()invisible(close(pb))
    .add_binding <- function(fun, name){
        assert_that( is.function(fun)
                   , rlang::is_string(name)
                   , !(name %in% ls(.self))
                   )
        makeActiveBinding(name, fun, .self)
    }
    .add_bindings <- function(bindings) purrr::imap(bindings, .add_binding)
    add_bindings <- function(...) .add_bindings(list(...))
    return(.self)
}

NFUN <- function(...)invisible(NULL)
progress_none <- function(...){
    list( init = NFUN
        , step = NFUN
        , update = NFUN
        , .add_binding = NFUN
        , add_bindings = NFUN
        , expose = NFUN
        )
}

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
is_purrr_frame <- function(frames){
    map_lgl(frames, . %>% topenv %>% getPackageName %>% `==`('purrr'))
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
get_call_symbols <- function(calls = sys.calls()){
    purrr::map_chr( calls, ~head(deparse(.[[1L]]), 1L))
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

.pb.stacks <- new.env(hash = TRUE, parent = emptyenv())
get_progress_stack <- function(stack, mode=c('any', 'list', 'environment')){
    mode <- match.arg(mode)
    if (exists(stack, envir=.pb.stacks, mode=mode, inherits = FALSE))
        get(stack, .pb.stacks)
    else switch( mode
               , list=list()
               , environment = {
                   env <- new.env(TRUE, emptyenv())
                   set_progress_stack(stack, env)
               }
               , NULL
               )
}
set_progress_stack <- function(stack, value){
    assign(stack, value, .pb.stacks)
}

push_progress <- function(pb, stack="progress_bars"){
    set_progress_stack(stack, c(get_progress_stack(stack), pb))
}
pop_progress <- function(stack = "progress_bars"){
    bars <- get_progress_stack(stack)
    if (length(bars) == 0) return()
    if (length(bars)) {
        pb <- tail(bars, 1)[[1]]
        if (is(pb, "winProgressBar")){
            close(pb)
        } else if(is.environment(pb) && exists('term', envir = pb, inherits = FALSE)){
            pb$term()
        }
        set_progress_stack(stack, head(bars, -1))
    }
}
if(FALSE){
    pb1 <- progress_bar(3, show=TRUE)
    push_progress(pb1)

    pb1$step()

    pb2 <- progress_bar(5, title = "sub-progress")
    push_progress(pb2)
    pb2$step(4)
    pop_progress()

    pb1$step()
    pb1$step()

    pop_progress()

}
with_purrr_progress <-
function(i = find_purrr_frame()
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

with_apply_progress <-
function(i = in_apply_call(), ..., fun){
    total <- get_apply_length(i)
    stack <- get_progress_stack("with_progress", "environment")
    set_progress_stack("with_progress", stack)
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
    pb <- progress_bar(total = total, ...)
    push_progress(pb, 'with_progress')
    function(...){
        pb$update()
        on.exit(pb$step())
        fun(...)
    }
}


with_progress_group_map <-
function( fun
        , i = in_call('group_map')
        , title = NULL
        , ...
        , frame = sys.frame(i)
        ){
    assert_that(!missing(fun))
    total <- eval( quote(pull(count(ungroup(count(.tbl)), name="TOTAL_ROWS"), "TOTAL_ROWS"))
                 , frame)
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
    push_progress(pb)
    # eval( quote(on.exit(.GlobalEnv$pop_progress(), add=TRUE))
    #     , frame
    #     )
    function(...){
        pb$update()
        on.exit(pb$step(keep.open = FALSE))
        fun(...)
    }
}

with_progress <-
function( fun
        , total
        , label = "{frac} items completed\t {etr} remaining"
        , ...
        ){
    if (missing(total)) {
        calls <- sys.calls()
        frames <- sys.frames()
        i <- max( in_purrr_map(calls, frames)
                , in_apply_call(calls)
                , in_call(c('group_map'))
                )
        if (length(i) && is.finite(i)) {
            if (getPackageName(frames[[i]]) == 'purrr')
                return(with_purrr_progress(i, label=label, ..., fun=fun))
            call.symbols <- get_call_symbols(calls)
            if (call.symbols[[i]] %in% base.apply.calls)
                return(with_apply_progress(i, label=label, ..., fun=fun))
            if (call.symbols[[i]] == 'group_map')
                return(with_progress_group_map(i, label=label, ..., fun=fun))
        } else {
            stop("total is missing and could not find an appropriate" %<<%
                 "call to associate with progress bar.")
        }
    } else {
        pb <- progress_bar(total = total, title=title, label=label, ...)
        function(...){
            pb$update()
            on.exit(pb$step())
            fun(...)
        }
    }
}
if(FALSE){#@development

    f <- function(x, y){
        Sys.sleep(.5)
        x^y
    }

    purrr::map_dbl(1:100, with_progress(f), 2)

}
