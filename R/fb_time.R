# maybe write in a way that it captures timezone when present
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
    "ru"    = ""
)

readr::parse_datetime(
    "5 December 2014 at 16:10 UTC+03",
    "%d %B %Y at %H:%M %*",
    locale = readr::locale(tz = "Etc/GMT+3")
)

tm <- "Wednesday, 25 March 2015 at 22:30 UTC+02"
readr::parse_datetime(
    "Wednesday, 25 March 2015 at 22:30",
    "%+ %d %B %Y at %H:%M"
)



