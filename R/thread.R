fb_thread <- function(contents){
    threads <- xml2::xml_find_all(contents, "//div[@class = 'thread']")
    data <- lapply(
        threads,
        function(x){
            tibble::tibble(
                thread  = xml2::xml_text(xml2::xml_find_first(x, "text()")),
                user    = xml2::xml_text(xml2::xml_find_all(x, "div/div/span[@class = 'user']")),
                str_dt  = xml2::xml_text(xml2::xml_find_all(x, "div/div/span[@class = 'meta']")),
                message = xml2::xml_text(xml2::xml_find_all(x, "p"))
            )
        }
    )
    data <- do.call(rbind, data)
    data$thread <- as.factor(data$thread)
    data$user <- as.factor(data$user)
    data$orig_tz <- stringr::str_extract(data$str_dt, "UTC[+-]\\d\\d")
    data$dt <- fb_parse_dt(data$str_dt, "en_GB")
    data
}



