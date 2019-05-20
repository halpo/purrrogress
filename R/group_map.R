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
