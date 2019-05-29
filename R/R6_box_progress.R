#' @importFrom assertthat see_if

get_char <- function(pct, pctmap, default, env = parent.frame()){
    delayedAssign('val', default)
    for(f in pctmap) {
        lhs <- rlang::f_lhs(f)
        test <- rlang::eval_tidy(lhs, list(. = pct), env = parent.frame())
        if (!test) break
        val <- rlang::f_rhs(f)
    }
    return(val)
}
ensure_nl <- function(s){
    if (nchar(s)>0 && substring(s, first = nchar(s), last=nchar(s)) != '\n')
        return(paste0(s, '\n'))
    else return(s)
}


make_txt_progress_bar =
function( width
        , pct
        , charset=basic.charset
        ){
    no.start <- is.null(charset$start)
    no.end   <- is.null(charset$end)
    fixed.start <- no.start || is.character(charset$start)
    fixed.end   <- no.end || is.character(charset$end)

    usable.width <- width - (fixed.start&!no.start) - (fixed.end&!no.end)
    nc <- usable.width * pct

    char.start <- if (fixed.start) {
        if (no.start) '' else charset$start
    } else if (is.list(charset$start)) {
        get_char(nc, charset$start)
    } else pkg_error("Invalid value for charset$start"
                    , type="invalid value")

    char.fill <- if (is.character(charset$fill)){
        charset$fill
    } else if ( !is.null(charset$position)
             && is.list(charset$position)
              ){
        get_char(1, charset$position)
    } else '='

    char.blank <- if (is.character(charset$blank)){
        charset$blank
    } else if ( !is.null(charset$position)
             && is.list(charset$position)
              ){
        get_char(0, charset$position)
    } else ' '

    char.position <- if ( is.null(charset$position)
                       || (!fixed.start && nc <= 1)
                       || (!fixed.end && width - nc < 1)
                       || nc %% 1 == 0
                        ){
        ''
    } else if (is.character(charset$position)){
        charset$position
    } else if (is.list(charset$position)){
        get_char(nc %% 1L, charset$position, char.blank)
    } else pkg_error( "Invalid value for charset$position"
                    , type="invalid value")

    char.end <- if (fixed.end) {
        if (no.end) '' else charset$end
    } else if (is.list(charset$end)) {
        get_char( nc - usable.width + 1, charset$end)
    } else pkg_error("Invalid value for charset$end"
                    , type="invalid value")

    nf <- max(min( floor(usable.width * pct - (nchar(char.start)*(!fixed.start)))
                 , usable.width - (nchar(char.end)*(!fixed.end)) - (nchar(char.end)*(!fixed.end))
                 ), 0)
    nb <- max(0, width - nf
               - nchar(char.start)
               - nchar(char.position)
               - nchar(char.end)
               )

    paste0( char.start
          , strrep(char.fill, nf)
          , char.position
          , strrep(char.blank, nb)
          , char.end
          )
}
if(FALSE){#@testing
    val <- make_txt_progress_bar(20, 1/3, basic.charset)
    expect_equal(val, "|======            |")

    val <- make_txt_progress_bar(20, 1/2, basic.charset)
    expect_equal(val, "|=========         |")

    val <- make_txt_progress_bar(20, 0, basic.charset)
    expect_equal(val, "|                  |")

    val <- make_txt_progress_bar(20, 1, basic.charset)
    expect_equal(val, "|==================|")
}


basic.charset <- list( start = "|"
                     , fill  = '='
                     , blank = ' '
                     , end   = '|'
                     )
is_valid_charmap <- function(charmap){
    see_if( is.list(charmap)
          , all(are(charmap, 'formula'))
          , all(map_int(charmap, length)== 3)
          , all(map_lgl(map(charmap, rlang::f_rhs), is.string))
          , is.string(get_char(0, charmap))
          , is.string(get_char(1, charmap))
          )
}
is_valid_charset <- function(charset){
    are.strings <- purrr::map_lgl(charset, is.string)
    are.lists <- purrr::map_lgl(charset, is.list)
    are.valid.charmaps <-purrr::map_lgl(charset, is_valid_charmap)
    see_if( all(are.strings | are.valid.charmaps)
          , all(c("fill", "blank") %in% names(charset))
          | ('position' %in% names(charset)
            & is_valid_charmap(charset$position)
            )
          , all(names(charset) %in% c("start", "fill", "position", "blank", "end"))
          , 'fill' %!in% names(charset) | is.string(charset$fill)
          , 'blank' %!in% names(charset) | is.string(charset$blank)
          )
}


R6_txt_progress <- R6::R6Class("R6 Text Progress Bar",
    inherit = R6_progress,
    public = {list(
        initialize = function( total
                             , title = ""
                             , label = "{fwfrac}{bar}({percent}) {etr} remaining"
                             , ...
                             , width = getOption('width')
                             , charset=basic.charset
                             ){
            super$initialize(total=total, title=title, label=label, ...)
            assert_that(is_valid_charset(charset))
            private$charset <- charset

            if (private$.title. != self$test_title){
                pkg_warning("Dynamic titles are not supported in text progress bars"
                           , type="feature not supported" )
            }

            if (!missing(width)){
                if (is.number(width) && 0 < width && width < 1){
                    width <- as.integer(round(console.width * width))
                } else {
                    assert_that( is.count(width)
                               , width <= getOption('width')
                               )
                }
            }
            private$total.width <- width
            private$infer_bar_width()
        },
        init = function(){
            {
                super$init()
                title <- self$title
                if(is.character(title) && nchar(title) > 0)
                    ensure_nl(title)
                self$print_label()
                flush.console()
            }
            invisible(self)
        },
        update = function(...){
            cat('\r')
            flush.console()
            if (...length()>0)
                cat(ensure_nl(paste0(...)))
            self$print_label()
            flush.console()
            return(invisible(self))
        },
        term = function(){
            self$update()
            cat('\n')
            flush.console()
            invisible(NULL)
        },
        print_label = function(){
            label <- self$label
            console.width <- getOption('width')
            if (nchar(label) > console.width)
                label <- substring(label, 0, console.width)
            cat(label)
        }
    )},
    active = {list(
        fwfrac = function(){
            format <- sprintf("%% %dd/%%d", ceiling(log10(self$total+1L)))
            sprintf(format, self$current, self$total)
        },
        test_label = function()private$get_test_label(bar="==="),
        test_title = function(){
            glue::glue_data(private$get_test_params(), private$.title., envir=private$bindings)
        },
        bar = function(){
            make_txt_progress_bar( width = private$bar.width
                                 , pct = self$current/self$total
                                 , charset = private$charset)
        }
    )},
    private = {list(
        total.width = NULL,
        bar.width = NULL,
        charset = basic.charset,
        get_test_params = function(...){list( current = private$.total.
                                            , frac = paste0(private$.total., '/', private$.total.)
                                            , elapsed.time = "12:34:56"
                                            , average.time = "00:12:34"
                                            , estimated.total.time = "12:34:56"
                                            , estimated.time.remaining = "12:34:56"
                                            , etr = "12:34:56"
                                            , percent = "100%"
                                            , ...)},
        get_test_env = function(...){
            list2env(private$get_test_params(...), parent=private$bindings)
        },
        get_test_label = function(...){
            glue::glue_data(private$get_test_env(...), private$.label.)
        },
        check_bar_width = function(min.bar.width = 10L){
            console.width <- getOption('width')
            if (private$bar.width < min.bar.width)
                pkg_error( "Console does not have enough room for progress bar and label."
                         , type = "label too wide")
        },
        infer_bar_width = function() {
            label.width <- nchar(private$get_test_label(bar=""))
            console.width <- getOption('width')
            private$bar.width <- private$total.width - label.width
        }
    )}
)
if(FALSE){#@testing
    pb <- R6_txt_progress$new( 1000, title = "Test text progress"
                             , label = "{fwfrac} {bar}({percent}) {etr} remaining"
                             , width = 50
                             )

    expect_equal(pb$fwfrac, "   0/1000")
    expect_match(pb$bar, "\\| +\\|")
    expect_equal(pb$title, "Test text progress")
    expect_equal( pb$label
                , "   0/1000 |             |(0%) NA remaining"
                )
    expect_output( pb$step(999)
                 , " 999/1000 |=+|\\(99%\\) 00:00:00 remaining"
                 )
    expect_output( pb$step()
                 , "1000/1000 |=+|\\(100%\\) 00:00:00 remaining\n"
                 )
}

line.charset <-
    list( start = list( TRUE ~ '\u250A' #< dashed vertical
                      , . >= 0.25 ~ '\u2502' #< light vertical
                      , . >= 0.50 ~ '\u2503' #< heavy vertical
                      , . >= 0.75 ~ '\u2520' #< heavy vertical light right
                      , . >= 1.00 ~ '\u2523'
                      )
        , fill = '\u2501' #< heavy horizontal
        , position = list( TRUE ~ ' '
                         , . >= 0.25 ~ '\u2574' #< light left
                         , . >= 0.50 ~ '\u2578' #< heavy left
                         , . >= 0.75 ~ '\u257E' #< heavy left light right
                         , . == 1   ~ '\u2501' #< heavy horizontal
                         )
        , blank = ' ' #< blank space
        , end = list( TRUE ~ '\u2502' #< light vertical
                    , . >= 0.25 ~ '\u2524' #< light left light vertical
                    , . >= 0.50 ~ '\u2525' #< heavy left light vertical
                    , . >= 0.75 ~ '\u252B' #< heavy/heavy
                    )
        )
if(FALSE){#@testing
    val <- make_txt_progress_bar(23, 1/3, line.charset)
    expect_equal(nchar(val), 23)

    get_char(8.3, line.charset$start)

    expected <- paste0('\u2523'
                      , strrep('\u2501', floor(23/3-1))
                      , '\u2578' #< heavy left i.e. >= 50% of block
                      , strrep(' ', 23-3-6)
                      , '\u2502'
                      )
    expect_equal(val, expected)

    val <- make_txt_progress_bar(23, 1, line.charset)
    expect_equal(nchar(val), 23)
    expected <- paste0('\u2523'
                      , strrep('\u2501', 21)
                      , '\u252B'
                      )
    expect_equal(val, expected)
}
R6_line_progress <- R6::R6Class("R6 Line Drawing Progress",
    inherit = R6_txt_progress,
    public = list(
        initialize = function(..., charset=line.charset)
                        super$initialize(..., charset=charset)
    )
)
if(FALSE){#@testing
    pb <- R6_line_progress$new( 80, title = "Test line text progress"
                              , label = "{fwfrac}{bar}({percent}) {etr} remaining"
                              , width = 50
                              )

    expect_equal(pb$fwfrac, " 0/80")
    expect_equal(pb$title, "Test line text progress")
    expect_true(pb$bar=="\u250A                  \u2502")
    expect_output(pb$init(), " 0/80(.*)\\(0%\\) NA remaining")

    expect_output( pb$step(), regexp = ".* 1/80(.*)\\(1%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2502                  \u2502")

    expect_output( pb$step(), regexp = ".* 2/80(.*)\\(2%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2503                  \u2502")

    expect_output( pb$step(), regexp = ".* 3/80(.*)\\(3%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2520                  \u2502")

    expect_output( pb$step(), regexp = ".* 4/80(.*)\\(5%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523                  \u2502")

    expect_output( pb$step(), regexp = ".* 5/80(.*)\\(6%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523\u2574                 \u2502")

    expect_output( pb$step(), regexp = ".* 6/80(.*)\\(7%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523\u2578                 \u2502")

    expect_output( pb$step(), regexp = ".* 7/80(.*)\\(8%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523\u257E                 \u2502")

    expect_output( pb$step(), regexp = ".* 8/80(.*)\\(10%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523\u2501                 \u2502")

    expect_output( pb$step(), regexp = ".* 9/80(.*)\\(11%\\) ([0-9:]{8}) remaining")
    expect_true(pb$bar=="\u2523\u2501\u2574                \u2502")

    expect_output(pb$step(67))
    expect_true(pb$bar==paste0("\u2523", strrep("\u2501", 18), "\u2502"))

    expect_output(pb$step())
    expect_true(pb$bar==paste0("\u2523", strrep("\u2501", 18), "\u2524"))

    expect_output(pb$step())
    expect_true(pb$bar==paste0("\u2523", strrep("\u2501", 18), "\u2525"))

    expect_output(pb$step())
    expect_true(pb$bar==paste0("\u2523", strrep("\u2501", 18), "\u252B"))
}

block.charset <-
    list(position = list( TRUE ~ ' '
                        , . >= 1/8  ~ '\u258F'
                        , . >= 2/8  ~ '\u258E'
                        , . >= 3/8  ~ '\u258D'
                        , . >= 4/8  ~ '\u258C'
                        , . >= 5/8  ~ '\u258B'
                        , . >= 6/8  ~ '\u258A'
                        , . >= 7/8  ~ '\u2589'
                        , . == 1    ~ '\u2588'
                        ))
if(FALSE){#@testing
    val <- make_txt_progress_bar(10, 9/80, block.charset)
    expect_equal(nchar(val), 10)
    expect_equal( val, "\u2588\u258F        ")
    expect_equal( make_txt_progress_bar(10, 12/80, block.charset)
                , "\u2588\u258C        ")
}
R6_box_progress <- R6::R6Class("R6 Block Drawing Progress",
    inherit = R6_txt_progress,
    public = list(
        initialize = function(..., charset=block.charset)
                        super$initialize(..., charset=charset)
    )
)
if(FALSE){#@testing
    pb <- R6_box_progress$new( 160, title = "Test block box progress"
                               , label = "{fwfrac}{bar}({percent}) {etr} remaining"
                               , width = 52
                               )

    expect_equal(pb$fwfrac, "  0/160")
    expect_match(pb$bar, " {20}")
    expect_equal(pb$title, "Test block box progress")
    expect_match( pb$label, "  0/160 {20}\\(0%\\) NA remaining")
    expect_output(pb$init(), "  0/160 {20}\\(0%\\) NA remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  1/160(\u258F) {19}\\(0%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  2/160(\u258E) {19}\\(1%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  3/160(\u258D) {19}\\(1%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  4/160(\u258C) {19}\\(2%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  5/160(\u258B) {19}\\(3%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  6/160(\u258A) {19}\\(3%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  7/160(\u2589) {19}\\(4%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  8/160(\u2588) {19}\\(5%\\) \\d\\d:\\d\\d:\\d\\d remaining")

    expect_output( pb$step())
    expect_match(pb$label, regexp = "  9/160(\u2588\u258F) {18}\\(5%\\) \\d\\d:\\d\\d:\\d\\d remaining")
}
