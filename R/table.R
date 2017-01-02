fb_table <- function(contents){
    field <- lapply(
        xml2::xml_find_all(contents, "//table/tr/th"),
        function(x) fb_get_text(x, split_by_comma = T)
    )
    value <- lapply(
        xml2::xml_find_all(contents, "//table/tr/td"),
        function(x) fb_get_text(x, split_by_comma = T)
    )
    res <- tibble::data_frame(field, value)
    for(i in 1:ncol(res)){
        if (all(sapply(res[[i]], length) <= 1)) res[[i]] <- unlist(res[[i]])
    }
    res
}
