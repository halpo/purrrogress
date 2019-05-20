test_progress_status <-
function( step
        , total = NULL
        , title = NULL
        , label = NULL
        , ...
        , stack = 'with_progress'
        , class = "R6 Progress Base Class"
        ){
    assert_that(requireNamespace('testthat'))
    pb <- peek_progress('with_progress')
    testthat::expect_equal(pb$current, step-1)

    if(!is.null(class)) testthat::expect_is(pb, class)
    if(!is.null(total)) testthat::expect_equal(pb$total, total)
    if(!is.null(title)) testthat::expect_match(pb$title, title)
    if(!is.null(label)) testthat::expect_match(pb$label, label)
    invisible(TRUE)
}
