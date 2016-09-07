#' Title
#'
#' @param File containing messages. If you haven't ...
#'
#' @return data.frame with collumns:
#' @export
#'
#' @examples
fb_messages <- function(file){
    parsed <- XML::htmlParse(readLines(file, encoding = "UTF-8", warn = F))
    threads <- XML::getNodeSet(parsed, "//div[@class = 'thread']")
    data <- lapply(threads,
        function(x){
            data.frame(
                thread  = as.factor(XML::xpathSApply(x,"text()", XML::xmlValue)),
                user    = as.factor(XML::xpathSApply(x, "div/div/span[@class = 'user']", XML::xmlValue)),
                time    = fbTime(XML::xpathSApply(x, "div/div/span[@class = 'meta']", XML::xmlValue)),
                ##locale???
                message = XML::xpathSApply(x, "p", XML::xmlValue),
                stringsAsFactors = F
            )
        }
    )
    do.call(rbind, data)
}
