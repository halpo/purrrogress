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
        if (is.name(call[[2]])) {
            if ( is.call(call[[3]])
              && call[[3]][[1]] == "with_progress"
              && is.symbol(call[[3]][[2]])
               )
                title <- deparse(as.call(c( as.list(call[1:2])
                                          , list(call[[3]][[2]])
                                          , if (length(call)>3L) alist(...)
                                          )))
            else if(is.symbol(call[[3]]))
                title <- deparse(as.call(c( as.list(call[1:3])
                                          , if (length(call)>3L) alist(...)
                                          )))
            else
                title <- deparse(as.call(c(as.list(call[1:2]), alist(...))))
        } else {
            title <- paste(sQuote(deparse(call[[1]])), "progress")
        }
    }
    pb <- progress_bar(total = total, title=title, ...)
    push_progress(pb, "with_progress")
    # eval( quote(on.exit(.GlobalEnv$pop_progress(), add=TRUE))
    #     , frame
    #     )
    pb$init()
    function(...){
        pb$update()
        on.exit(pb$step())
        fun(...)
    }
}
if(FALSE){#@testing
    if( requireNamespace('dplyr')
      & requireNamespace('tibble')
      & requireNamespace('datasets')
      ){

    x <- dplyr::group_by(datasets::iris, Species)
    test_group_map_progress <- function(df, key, ...){
        val <- test_progress_status( step = match(key$Species, unique(iris$Species)), ...)
        tibble::tibble(val)
    }

    val <- dplyr::group_map( dplyr::group_by(iris, Species)
                           , with_progress(test_group_map_progress, type="none")
                           , total=3
                           , title = ".dplyr::group_map. progress"
                           , label = "\\d/3 items completed")
    expect_true(all(val$val))

    val <- dplyr::group_map(x, with_progress(test_group_map_progress, type="none")
                           , total=3
                           , title = "group_map\\(x, test_group_map_progress, ...)"
                           , label = "\\d/3 items completed")
    expect_true(all(val$val))

    val <- dplyr::group_map(x, with_progress(function(...){
                                    test_group_map_progress(...)
                                }, type="none")
                           , total=3
                           , title = "group_map\\(x, ...)"
                           , label = "\\d/3 items completed")
    expect_true(all(val$val))

    delayedAssign('f', with_progress(test_group_map_progress, type="none"))
    val <- dplyr::group_map(x, f
                           , total=3
                           , title = "group_map\\(x, f, \\.\\.\\.\\)"
                           , label = "\\d/3 items completed")
    expect_true(all(val$val))
    }
}
if(FALSE){# Manual Testing
    x <- dplyr::group_by(datasets::iris, Species)
    group_function <- function(df, keys, how.long=0.05){
        Sys.sleep(how.long)
        x
    }
    dplyr::group_walk( dplyr::group_by_all(iris)
                     , with_progress(group_function, type="win", show.after=0)
                     , how.long = 0.05
                     )

}


