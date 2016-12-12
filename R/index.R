#' Read profile information from Facebook index.htm file
#'
#' @param path location of your Facebook data folder or precise location of index.htm file
#'
#' @return \code{data.frame} with two columns - \code{field} and \code{value}
#' @export
#'
#' @examples
#' prfl <- fb_index("my_fb_data")
#' prfl <- fb_index("my_fb_data/index.htm")
fb_index <- function(path){
    if (!(file.exists(path) | dir.exists(path))) stop("File/directory doesn't exist.")
    if (file.info(path)$isdir){
        file <- paste0(path, "/index.htm")
    } else file <- path
    parsed <- XML::xmlParseDoc(file, encoding = "UTF-8")
    xmlValue_2 <- function(x){
        res <- XML::xmlToList(x)
        res <- unlist(res, use.names = F)
        if (!is.null(res)) res <- unlist(strsplit(res, ", "))
        res
    }
    field <- XML::xpathSApply(
        parsed,
        "/html/body/div[@class = 'contents']/div/table/tr/th",
        XML::xmlValue,
        xmlValue_2
    )
    value <- XML::xpathSApply(
        parsed,
        "/html/body/div[@class = 'contents']/div/table/tr/td",
        xmlValue_2
    )
    name <- XML::xpathSApply(parsed, "/html/body/div/h1", XML::xmlValue)
    tibble::data_frame(field, value)
}

fb_table <- function(node){

}



"/html/body/div[@class = 'contents']/div"
