fb_get_entitled_uls <- function(x){
    h2ul <- xml2::xml_find_all(x, "h2|ul")
    nms <- xml2::xml_name(h2ul)
    names(h2ul) <- nms
    n <- length(nms)
    splt <- cumsum(1 - c(0, (nms[-n] == "h2") & (nms[-1] == "ul")))
    h2ul <- split(h2ul, splt)
    ul <- lapply(h2ul, function(y) y$ul)
    h2 <- lapply(h2ul, function(y) xml2::xml_text(y$h2))
    names(ul) <- h2
    ul
}

fb_parse_ul <- function(ul){
    res <- lapply(xml2::xml_children(ul), fb_get_text)
    if (all(sapply(res, length) <= 1)) res <- unlist(res)
    res
}

fb_ul <- function(contents){
    div <- xml2::xml_find_all(contents, "//div[ul]")
    ul <- lapply(div, fb_get_uls)
    ul <- do.call(c, ul)
    lapply(ul, fb_parse_ul)
}





