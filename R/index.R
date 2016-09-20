#' Read profile information from Facebook index.htm file
#'
#' @param file location of your index.htm file
#'
#' @return \code{data.frame} with two columns - \code{field} and \code{value}
#' @export
#'
#' @examples
#' prfl <- fb_index("my_fb_data/index.htm")
fb_index <- function(file){
    parsed <- XML::xmlParseDoc(file, encoding = "UTF-8")
    table <- XML::xpathApply(
        parsed, "/html/body/div/div/table",
        ## stringsAsFactors = F doesn't work
        function(x) XML::xmlToDataFrame(x, stringsAsFactors = F))[[1]]
    field <- table[[1]]
    value <- table[[2]]
    name <- XML::xpathSApply(parsed, "/html/body/div/h1", XML::xmlValue)
    value <- strsplit(value[-18], ", ")
    apps <- XML::xpathSApply(parsed, "/html/body/div/div/table/tr[18]/td/ul/li", XML::xmlValue)
    value <- c(list(name), value, list(apps))
    field <- c("name", field)
    tibble::data_frame(field, value)
}
