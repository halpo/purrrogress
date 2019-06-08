#' @import methods
NULL

#' Apply a function with progress bars.
#'
#' @param fun   The function to be apply
#' @param total The total number of elements to be mapped.
#'              If omitted an attempt will be made to infer the
#'              correct number.
#' @inheritDotParams progress_bar
#'
#' @export
#' @examples
#'
#' # with purrr functions
#' long_function <- function(x, how.long=0.05){
#'     Sys.sleep(how.long)
#'     x
#' }
#' \donttest{
#' purrr::walk(1:100, with_progress(long_function))
#' purrr::walk2(1:100, 0.01, with_progress(long_function))
#' }
#'
#' # with dplyr::group_map
#' \donttest{
#' if(require(dplyr)){
#' group_function <- function(x, y, how.long=0.05){
#'     Sys.sleep(how.long)
#'     x
#' }
#' group_map( group_by(mtcars, cyl, gear)
#'          , with_progress(group_function, type='line')
#'          , how.long=1/3)
#' group_walk( group_by_all(mtcars)
#'           , with_progress(group_function, type='box')
#'           , how.long=1)
#' }
#' }
#' # with standard apply functions
#' sapply(1:100, with_progress(long_function, type='txt'), 0.001)
#'
#'
with_progress <-
function( fun
        , total
        , ...
        ){
    if(!rlang::is_function(fun)) fun <- rlang::as_function(fun, parent.frame())
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
                return(with_purrr_progress(i, ..., fun=fun))
            call.symbols <- get_call_symbols(calls)
            if (call.symbols[[i]] %in% base.apply.calls)
                return(with_apply_progress(i, ..., fun=fun))
            if (call.symbols[[i]] == 'group_map')
                return(with_progress_group_map(i, ..., fun=fun))
        } else {
            stop("total is missing and could not find an appropriate" %<<%
                 "call to associate with progress bar.")
        }
    } else {
        pb <- progress_bar(total = total, ...)
        push_progress(pb, "with_progress")
        pb$init()
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
