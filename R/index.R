fb_index <- function(file){
    parsed <- XML::xmlParseDoc(file, encoding = "UTF-8")
    table <- XML::xpathApply(
        parsed, "/html/body/div/div/table",
        ## stringsAsFactors = F doesn't work
        function(x) XML::xmlToDataFrame(x, stringsAsFactors = T))[[1]]
    names(table) <- c("field", "value")
    name <- XML::xpathSApply(parsed, "/html/body/div/h1", XML::xmlValue)
    name_row <- data.frame(field = "name", value = name)
    table <- rbind(name_row, table)
    apps <- XML::xpathSApply(parsed, "/html/body/div/div/table/tr[18]/td/ul/li", XML::xmlValue)
    table
}
