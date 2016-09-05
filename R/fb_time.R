fbTime <- function(x, locale = 'en_GB'){
    strptime(x, format =
                 switch(locale,
                        'en_GB' = '%A, %d %B %Y at %H:%M',
                        'en_US' = '%A, %B %d, %Y at %I:%M%p'
                 )
    )
}
