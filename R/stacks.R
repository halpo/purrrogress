#' @importFrom utils head tail
NULL


.pb.stacks <- new.env(hash = TRUE, parent = emptyenv())
get_progress_stack <- function(stack){
    if (exists(stack, envir=.pb.stacks, mode='list', inherits = FALSE))
        get(stack, .pb.stacks)
    else list=list()
}
set_progress_stack <- function(stack, value){
    assign(stack, value, .pb.stacks)
}

push_progress <- function(pb, stack="progress_bars"){
    if(is(pb, c('winProgressBar', 'tkProgressBar', 'txtProgressBar')))
        pb <- list(pb)
    set_progress_stack(stack, c(get_progress_stack(stack), pb))
}
pop_progress <- function(stack = "progress_bars"){
    bars <- get_progress_stack(stack)
    if (length(bars) == 0)
        pkg_error("Stack" %<<% sQuote(stack) %<<% "is empty."
                 , type = "empty progress stack" )
    if (length(bars)) {
        pb <- utils::tail(bars, 1)[[1]]
        if (is(pb, "winProgressBar") | is(pb, 'tkProgressBar') | is(pb, 'txtProgressBar')){
            close(pb)
        } else if(is.environment(pb) && exists('term', envir = pb, inherits = FALSE)){
            pb$term()
        }
        set_progress_stack(stack, head(bars, -1))
    }
}
peek_progress <- function(stack = 'progress_bars'){
    bars <- get_progress_stack(stack)
    if (length(bars) == 0)
        pkg_error("No current progress bars registered"
                 , type = "empty progress stack" )
    return(bars[[length(bars)]])
}

if(FALSE){#@testing
    expect_identical(get_progress_stack('test stack'), list())

    pb1 <- progress_bar(3, show=TRUE, type='none')
    push_progress(pb1, 'test stack')

    expect_identical(get_progress_stack('test stack'), list(pb1))
    expect_identical(peek_progress('test stack'), pb1)

    pb1$step()
    expect_equal(peek_progress('test stack')$current, 1L)

    pb2 <- progress_bar(5, title = "sub-progress", type='none')
    push_progress(pb2, 'test stack')

    expect_identical(get_progress_stack('test stack'), list(pb1, pb2))
    expect_identical(peek_progress('test stack'), pb2)

    pop_progress('test stack')

    expect_identical(get_progress_stack('test stack'), list(pb1))
    expect_identical(peek_progress('test stack'), pb1)

    pb1$step()

    pop_progress('test stack')

    expect_identical(get_progress_stack('test stack'), list())
    expect_error( peek_progress('test stack')
                , class = "purrrogress-error-empty progress stack" )

    expect_error( pop_progress('test stack')
                , class = "purrrogress-error-empty progress stack" )


    txt <- txtProgressBar()
    push_progress(txt, 'test stack')

    expect_identical(peek_progress('test stack'), txt)

}



