fb_get_file <- function(path, what){
    if (!(file.exists(path) | dir.exists(path))) stop("File/directory doesn't exist.")
    if (file.info(path)$isdir){
        path <- gsub("/$", "", path)
        file <- paste0(path, "/", what, ".htm")
    } else file <- path
    file
}

fb_contents <- function(file){
    parsed <- xml2::read_html(file)
    contents <- xml2::xml_new_document()
    contents <- xml2::xml_add_child(
        contents,
        xml2::xml_find_first(parsed, "/html/body/div[@class = 'contents']")
    )
    contents
}
