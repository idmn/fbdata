fbTime <- function(x, locale = 'en_GB'){
    strptime(x, format =
                 switch(locale,
                        'en_GB' = '%A, %d %B %Y at %H:%M',
                        'en_US' = '%A, %B %d, %Y at %I:%M%p'
                 )
    )
}

readr::parse_datetime(
    "5 December 2014 at 16:10 UTC+03",
    '%d %B %Y at %H:%M UTC%z',
    locale = readr::locale(tz = "Etc/GMT+3")

)

