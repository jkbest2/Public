pretty_sci <- function(x, digits = 4) {
  library(stringr)
  x <- format(x, digits = digits)
  if(grepl('e', x)){
    value <- str_extract(x, '[0-9.]*e')
    value <- str_extract(value, '[0-9.-]*')
    pow   <- str_extract(x, 'e[0-9-]*')
    pow   <- str_extract(pow, '-?[0-9]+')
    return(paste('$', value, ' \\times 10^{', pow, '}$', sep = ''))
  }
  return(x)
}