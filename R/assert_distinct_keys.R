#' assert_distinct_keys
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' assert_distinct_keys(my.data.frame, key1, key2, key3)
assert_distinct_keys <- function(data, ...){

  is.duplicated = data %>%
    select(...) %>%
    duplicated %>%
    any

  if (is.duplicated) stop(glue('Data frame [{deparse(substitute(indata)}] is not distinct by the specified keys'))

  return(is.duplicated)
}


