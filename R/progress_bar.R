

#' Create a R6 progress bar directly
#'
#' @param total the total number of elements
#' @param title the title of the progress bar
#' @param label the label to display
#' @param ... passed on to the specific constructor determined by type.
#' @param type the type of progress bar to create as a string, or an
#'             `R6ClassGenerator` object for a class that inherits from
#'             the "R6 Progress Base Class".
#'
#' @export
progress_bar <-
function( total
        , title = "Progress"
        , label = ""
        , ...
        , type = c('none', 'win', 'tk')
        ){
    if (is(type, "R6ClassGenerator")){
        assert_that( identical(type, R6_progress)
                   | identical(type$get_inherit(), R6_progress)
                   )
    } else {
        type <- match.arg(type)
        type <- switch( type
                      , win = R6_win_progress
                      , tk  = pkg_error('R6_tk_progress not implimented.')
                      , R6_progress
                      )

    }
    type$new( total=total
            , title=title
            , label=label
            , ...)
}
