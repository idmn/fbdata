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
    file <- fb_get_file(path, "index")
    contents <- fb_contents(file)
    get_text <- function(x){
        res <- unlist(xml2::as_list(x), use.names = F)
        if (!is.null(res)) res <- unlist(strsplit(res, ", "))
        res
    }
    field <- lapply(
        xml2::xml_find_all(contents, "/div/table/tr/th"),
        get_text
    )
    value <- lapply(
        xml2::xml_find_all(contents, "/div/table/tr/td"),
        get_text
    )
    tibble::data_frame(field, value)
}
