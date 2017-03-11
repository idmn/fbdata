fb_dt_formats <- tibble::tribble(
    ~id,     ~locale, ~format,
    "en_GB", "en",    "%* %d %B %Y at %H:%M %*%z",
    "en_US", "en",    "%* %B %d, %Y at %I:%M%p",
    "ru",    "ru",    "%d %B %Y г. в %H:%M %*%z" # readr::parse_datetime fails for unknowr reasons
)


fb_dt_guess_format <- function(x){
    x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0){
        warning("Datetime vector contains no non-missing values, can't guess the language.")
        return(NA)
    }
    x <- sample(x, min(n, 5))
    res <- sapply(
        seq_along(fb_dt_formats$id),
        function(i){
            suppressWarnings(
                p <- readr::parse_datetime(
                    x,
                    format = fb_dt_formats[[i, "format"]],
                    locale = readr::locale(fb_dt_formats[[i, "locale"]])
                )
            )
            sum(!is.na(p))
        }
    )
    names(res) <- fb_dt_formats[["id"]]
    if (all(res == 0)) {
        warning("Failed to guess the language of the datetime vector.")
        return(NA)
    }
    names(res)[which.max(res)]
}

#' Parse datetime in FB data
#'
#' @param x character vector of datetimes
#' @param format datetime format id. See <link here>
#'
#' @export
#'
#' @examples
#' fb_parse_dt("Wednesday, 9 March 2016 at 16:01 UTC+02")
fb_parse_dt <- function(x, format = NULL){
    ## test format argument somehow
    if (is.null(format)){
        format <- fb_dt_guess_format(x)
    }
    ind <- which(fb_dt_formats$id == format)
    if (length(ind) != 1){
        warning("Can't find the format.")
        return(rep(NA, length(x)))
    }
    readr::parse_datetime(
        x,
        format = fb_dt_formats[[ind, "format"]],
        locale = readr::locale(fb_dt_formats[[ind, "locale"]])
    )
}
