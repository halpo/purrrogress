test_progress_status <-
function( step
        , total = NULL
        , title = NULL
        , label = NULL
        , ...
        , stack = 'with_progress'
        , class = "R6 Progress Base Class"
        ){
    pb <- peek_progress('with_progress')
    expect_equal(pb$current, step-1)

    if(!is.null(class)) expect_is(pb, class)
    if(!is.null(total)) expect_equal(pb$total, total)
    if(!is.null(title)) expect_match(pb$title, title)
    if(!is.null(label)) expect_match(pb$label, label)
    invisible(TRUE)
}
