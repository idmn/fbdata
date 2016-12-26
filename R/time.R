fb_dt <- function(x, lang){
    # try to guess locale ???
    #locale = "en_GB"
    readr::parse_datetime(
        x,
        fb_dt_formats[[lang]]
    )
}

fb_dt_formats <- list(
    "en_GB" = "%* %d %B %Y at %H:%M",
    "en_US" = "%* %B %d, %Y at %I:%M%p",
    "ru"    = "%d %B %Y г. в %H:%M"
)
