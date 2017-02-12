#' Parse datetime in FB data
#'
#' @param x character vector of datetimes
#' @param lang language in which time is written
#'
#' @export
#'
#' @examples
#' # no examples currently
fb_parse_dt <- function(x, lang){
    # try to guess locale ???
    #locale = "en_GB"
    readr::parse_datetime(
        x,
        fb_dt_formats[[lang]]
    )
}

fb_dt_formats <- list(
    "en_GB" = "%* %d %B %Y at %H:%M %*%z",
    "en_US" = "%* %B %d, %Y at %I:%M%p",
    "ru"    = "%d %B %Y г. в %H:%M"
)

readr::parse_datetime(
    "Friday, 5 December 2014 at 23:34 UTC-0200",
    "%* %d %B %Y %* %H:%M UTC%z"
)


