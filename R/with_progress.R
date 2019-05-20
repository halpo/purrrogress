#' @import methods
NULL

#' Apply a function with progress bars.
#'
#' @param fun   The function to be apply
#' @inheritParams progress_bar
#'
#' @export
with_progress <-
function( fun
        , total
        , label = "{frac} items completed\t {etr} remaining"
        , ...
        , type = infer_type(.Platform$OS.type)
        ){
    if (missing(total)) {
        calls <- sys.calls()
        frames <- sys.frames()
        which <- seq.int(sys.nframe())
        i <- max( in_purrr_map(which, calls=calls, frames=frames)
                , in_apply_call(calls)
                , in_call(c('group_map'))
                )
        if (length(i) == 1 && is.finite(i) && i > 0) {
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
        pb <- progress_bar(total = total, label=label, ...)
        push_progress(pb, "with_progress")
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

infer_type <- function(sysname){
    switch( sysname
          , Windows = 'win'
          , 'tk'
          )

}
