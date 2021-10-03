#' Determine if an object can have measurement units.

#' @name is_unitable
#' @param x [R object]
#' @return \code{is_unitable}: [Logical(1)]
#' @examples

#' is_unitable(list('a', 'b', 'c'))
#' is_unitable(data.table(3, 2, 1))
#' is_unitable(Sys.time())
#' is_unitable(Sys.Date())
#' is_unitable(1L:10L)
#' is_unitable(NA_real_)
#' is_unitable(NA_integer_)
#' is_unitable(NA_complex_)
#' is_unitable(NA_character_)
#' is_unitable(1.0)
#' @export
is_unitable <- function(x) {
    # Determine if an object can have measurement units
  (is.double(x)|is.integer(x))&!is_data_date(x)
    # Returns: [Logical(1)]
}

