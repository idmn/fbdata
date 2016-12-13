fb_ul <- function(node){
    lstnd <- XML::xmlToList(node)
    lstnd <- lstnd[names(lstnd) %in% c("h2", "ul")]
    n <- length(lstnd)
    nms <- names(lstnd)
    splt <- cumsum(1 - c(0, (nms[-n] == "h2") & (nms[-1] == "ul")))
    lstnd <- split(lstnd, splt)
    h2 <- sapply(
        lstnd,
        function(x){
            res <- x[["h2"]]
            ifelse(is.null(res), NA, res)
        }
    )
    ul <- lapply(
        lstnd,
        function(x){
            x[["ul"]]
        }
    )
    names(ul) <- h2
}


