fb_table <- function(contents){
    field <- lapply(
        xml2::xml_find_all(contents, "//table/tr/th"),
        function(x) fb_get_text(x, split_by_comma = T)
    )
    value <- lapply(
        xml2::xml_find_all(contents, "//table/tr/td"),
        function(x) fb_get_text(x, split_by_comma = T)
    )
    tibble::data_frame(field, value)
}
