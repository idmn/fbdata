fb_messages <- function(messages){
    readLines(messages, encoding = 'UTF-8') %>%
        htmlParse() %>%
        getNodeSet("//div[@class = 'thread']") %>%
        lapply(
            function(node){
                data.frame(
                    thread  = xpathSApply(node,"text()",xmlValue) %>% as.factor(),
                    user    = xpathSApply(node, "div/div/span[@class = 'user']", xmlValue)
                    %>% as.factor(),
                    time    = xpathSApply(node, "div/div/span[@class = 'meta']", xmlValue)
                    %>% fbTime(), ##locale???
                    message = xpathSApply(node, "p", xmlValue),
                    stringsAsFactors = F
                )
            }
        ) %>%
        do.call(rbind, .)
}
