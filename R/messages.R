#' Read messages from Facebook html/messages.htm file
#'
#' @param file location of your messages.htm file
#'
#' @return \code{data.frame} with 5 variables:
#' \describe{
#'  \item{thread}{thread name - participants names separated by commas}
#'  \item{user}{name of the message's author}
#'  \item{dt}{time when message was posted}
#'  \item{tz}{your timezone at the moment when you recieved the message}
#'  \item{message}{message itself}
#' }
#'
#' @export
#'
#' @examples
#' msgs <- fb_messages("my_fb_data/html/messages.htm")
fb_messages <- function(file){
    parsed <- XML::htmlParse(readLines(file, encoding = "UTF-8", warn = F))
    threads <- XML::getNodeSet(parsed, "//div[@class = 'thread']")
    data <- lapply(threads,
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
                       df[, c("thread", "user", "dt", "tz", "message")]
                   }
    )
    do.call(rbind, data)
}
