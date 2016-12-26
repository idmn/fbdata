#' Parse FB file
#'
#' @param file file to parse
#' @param parser which parser to use. This function normally should detect by itself.
#' But if any problems occur, you can set it by hands. Possible values are:
#'  "table", "ul", "thread".
#'
#' @export
#'
#' @examples
#' msgs <- fb_parse_file("data/html/messages.htm)
#' msgs <- fb_parse_file("data/html/messages.htm, parser = "thread")
fb_parse_file <- function(file, parser = NULL){
    obj <- fb_contents(file)
    if (!is.null(parser)) parser_function <- switch(
            parser,
            "table"  = fb_table,
            "ul"     = fb_ul,
            "thread" = fb_thread
    ) else parser_function <- switch(
            obj$what,
            "profile"          = fb_table,
            "ads"              = fb_ul,
            "contact info"     = fb_table,
            "events"           = fb_events,
            "friends"          = NULL,
            "messages"         = fb_thread,
            "mobile devices"   = NULL,
            "photos"           = NULL,
            "places created"   = fb_ul,
            "pokes"            = fb_ul,
            "security"         = fb_ul,
            "survey responses" = NULL,
            "synced photos"    = NULL,
            "timeline"         = NULL,
            "videos"           = NULL
    )
    parser_function(obj$contents)
}
