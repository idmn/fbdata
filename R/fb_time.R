fbTime <- function(x, locale = 'en_GB'){
    strptime(x, format =
                 switch(locale,
                        'en_GB' = '%A, %d %B %Y at %H:%M',
                        'en_US' = '%A, %B %d, %Y at %I:%M%p'
                 )
    )
}

d3 <- readr::parse_datetime(
    "5 December 2014 at 16:10",
    '%d %B %Y at %H:%M',
    locale = readr::locale(tz = "Etc/GMT+3")
)

d2 <- readr::parse_datetime(
    "5 December 2014 at 16:10",
    '%d %B %Y at %H:%M',
    locale = readr::locale(tz = "Etc/GMT+2")
)

"5 December 2014 at 16:10 UTC+03"
