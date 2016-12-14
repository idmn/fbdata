#' Read messages from Facebook html/messages.htm file
#'
#' @param path location of your facebook data folder or precise location of messages.htm file
#'
#' @return \code{data.frame} with 5 variables:
#' \describe{
#'  \item{\code{thread}}{thread name - participants names separated by commas}
#'  \item{\code{user}}{name of the message's author}
#'  \item{\code{dt}}{time when the message was posted, in your time settings}
#'  \item{\code{tz}}{your timezone at the moment when you recieved the message}
#'  \item{\code{message}}{message itself}
#' }
#'
#' @export
#'
#' @examples
#' msgs <- fb_messages("my_fb_data")
#' msgs <- fb_messages("my_fb_data/html/messages.htm")
fb_messages <- function(path){
    file <- fb_get_file(path, "html/messages")
    contents <- fb_contents(file)
    ## it is possible that there would be no outer div
    threads <- xml2::xml_find_all(contents, "div/div[@class = 'thread']")
    data <- lapply(
        threads,
        function(x){
            df <- tibble::tibble(
                thread  = xml2::xml_text(xml2::xml_find_first(x, "text()")),
                user    = xml2::xml_text(xml2::xml_find_all(x, "div/div/span[@class = 'user']")),
                str_dt  = xml2::xml_text(xml2::xml_find_all(x, "div/div/span[@class = 'meta']")),
                message = xml2::xml_text(xml2::xml_find_all(x, "p"))
            )
        }
    )
    data <- do.call(rbind, data)
    data
}

fb_messages <- function(path){
    file <- fb_get_file(path, "html/messages")
    parsed <- XML::htmlParse(readLines(file, encoding = "UTF-8", warn = F))
    threads <- XML::getNodeSet(parsed, "//div[@class = 'thread']")
    data <- lapply(
        threads,
        function(x){
            df <- data.frame(
                thread = as.factor(XML::xpathSApply(x,"text()", XML::xmlValue)),
                user   = as.factor(XML::xpathSApply(x, "div/div/span[@class = 'user']", XML::xmlValue)),
                str_dt = XML::xpathSApply(x, "div/div/span[@class = 'meta']", XML::xmlValue),
                ##locale???
                message = XML::xpathSApply(x, "p", XML::xmlValue),
                stringsAsFactors = F
            )
            df$tz <- stringr::str_extract(df$str_dt, "UTC[+-]\\d\\d")
            ## how to get lang
            df$dt <- fb_dt(stringr::str_replace(df$str_dt, "[, ]*UTC[+-]\\d\\d", ""), "en_GB")
            df[, c("thread", "user", "dt", "tz", "message", "str_dt")]
        }
    )
    do.call(rbind, data)
}


