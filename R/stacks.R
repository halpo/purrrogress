
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
peek_progress <- function(stack = 'progress_bars'){
    bars <- get_progress_stack(stack)
    if (length(bars) == 0)
        pkg_error("No current progress bars registered")
    return(bars[[length(bars)]])
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



