#' Parse FB file
#'
#' @param file file to parse
#' @param parser which parser to use. This function normally should choose it
#'   automatically. Possible values are: `"table"`, `"ul"`, `"thread"`.
#'
#' @export
#'
#' @examples
#' msgs <- fb_parse_file("data/html/messages.htm)
#' msgs <- fb_parse_file("data/html/messages.htm, parser = "thread")
#'
fb_parse_file <- function(file, parser = NULL){
    obj <- fb_contents(file)
    if (!is.null(parser)) parser_function <- switch(
        parser,
        "table"  = fb_table,
        "ul"     = fb_ul,
        "thread" = fb_thread
    ) else parser_function <- switch(
        obj$ind,          # obj$what
        "1"  = fb_table,  # profile
        "2"  = fb_table,  # contact info
        "3"  = NULL,      # timeline
        "4"  = NULL,      # photos
        "5"  = NULL,      # synced photos
        "6"  = NULL,      # videos
        "7"  = fb_ul,     # friends
        "8"  = fb_thread, # messages
        "9"  = fb_ul,     # pokes
        "10" = NULL,      # events
        "11" = fb_ul,     # security
        "12" = fb_ul,     # ads
        "13" = NULL,      # mobile devices
        "14" = fb_ul,     # places created
        "15" = NULL       # survey responses
    )
    parser_function(obj$contents)
}
