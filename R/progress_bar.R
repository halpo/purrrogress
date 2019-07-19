

#' Create a R6 progress bar directly
#'
#' @param total the total number of elements
#' @param title the title of the progress bar
#' @param ... passed on to the specific constructor determined by type.
#' @param type the type of progress bar to create as a string, or an
#'             `R6ClassGenerator` object for a class that inherits from
#'             the "R6 Progress Base Class".
#'
#' @export
#' @examples
#'
#' \donttest{
#' pb_win <- progress_bar(100, "Windows Progress", type = 'win')
#' }
#' pb_txt <- progress_bar(100, "Text Progress", type = 'txt')
#' pb_txt$init() # starts the timer and shows the bar.
#' pb_txt$step() # take 1 step update progress bar.
#' pb_txt$step(25) # take 24 steps at one time
#' pb_txt$term() # do finishing tasks for progress bar.
#'
#' # The following use Unicode characters and may not work with all fonts.
#' # DejaVu Sans Mono is one font which supports all the characters used
#' pb_bar   <- progress_bar(100, "Bar Progress", type = 'bar')
#' pb_line  <- progress_bar(100, "Line Progress", type = 'line')
#' pb_box   <- progress_bar(100, "Box Progress", type = 'box')
#' pb_block <- progress_bar(100, "Block Progress", type = 'block')
#'
progress_bar <-
function( total
        , title = "Progress"
        , ...
        , type = getOption('progress.type', infer_type())
        ){
    type <- resolve_type(type)
    type$new( total=total
            , title=title
            , ...)
}

infer_type <-
function( sysname=Sys.info()['sysname']
        , is.interactive = interactive()
        ){
    if (!is.interactive) return('none')
    switch( sysname
          , Windows = 'win'
          , 'txt'
          )

}
if(FALSE){#@testing
    expect_equal(infer_type('Windows', TRUE), 'win')
    expect_equal(infer_type('Windows', FALSE), 'none')
    expect_equal(infer_type('Linux', TRUE), 'txt')
    expect_equal(infer_type('FooBar', TRUE), 'txt')
    expect_equal(infer_type('FooBar', FALSE), 'none')
}


resolve_type <- function(type = infer_type()){
    if (is(type, "R6ClassGenerator")){
        assert_that( identical(type, R6_progress)
                   | identical(type$get_inherit(), R6_progress)
                   )
        return(type)
    } else {
        switch( type
              , win = R6_win_progress
              , tk  = pkg_error('R6_tk_progress not implimented.')
              , txt = R6_txt_progress
              , bar =
              , line= R6_line_progress
              , block=
              , box = R6_box_progress
              , none = R6_progress
              , {
                    pkg_warning("Invalid progress type", type="invalid progress type")
                    R6_progress
                })
    }
}
if(FALSE){#@testing
    expect_identical(resolve_type('win'), R6_win_progress)
    expect_error(resolve_type('tk'))
    expect_identical(resolve_type('txt'), R6_txt_progress)
    expect_identical(resolve_type('bar'), R6_line_progress)
    expect_identical(resolve_type('line'), R6_line_progress)
    expect_identical(resolve_type('box'), R6_box_progress)
    expect_identical(resolve_type('none'), R6_progress)
    expect_warning( resolve_type('foobar')
                  , class = "purrrogress-warning-invalid progress type"
                  )
    expect_identical(suppress_warnings(resolve_type('none')
                                      , class = "purrrogress-warning-invalid progress type")
                    , R6_progress)

}
