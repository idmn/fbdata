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
    what <- xml2::xml_find_all(parsed, "/html/body/div[@class = 'nav']//li[@class = 'selected']")
    what <- xml2::xml_text(what)
    what <- tolower(what)
    ind <- xml2::xml_find_num(parsed, "count(/html/body/div[@class = 'nav']//li[@class = 'selected']/preceding-sibling::li)") + 1
    contents <- xml2::xml_find_first(parsed, "/html/body/div[@class = 'contents']")
    root <- xml2::xml_root(parsed)
    xml2::xml_replace(root, contents)
    list(ind = ind, what = what, contents = contents)
}

fb_get_text <- function(x, split_by_comma = F){
    res <- unlist(xml2::as_list(x), use.names = F)
    if (split_by_comma & !is.null(res)) res <- unlist(strsplit(res, ", "))
    res
}



