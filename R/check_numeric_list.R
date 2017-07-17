#' check_numeric_list
#'
#' \code{check_numeric_list} checks if all of the elements in the \code{lst} can be a numeric value. It
#' returns \code{TRUE} if all of the elements of \code{lst} are \code{numeric} or are numeric values in character form,
#' such as "1", "3.4", "-76", etc.
#' If \code{with_bool} is set to \code{TRUE}, \code{check_numeric_list} assumes booleans
#' can also have an equivalent numeric form (being 1 and 0), and returns TRUE if it detects a boolean \code{lst}.
#'
#' @param lst a vector or list.
#' @param with_bool bool. Specifies whether we consider booleans as numeric values.
#'
#' @return This function returns True if the object can be converted to an equivalent numeric form.
#'
#' @section Errors: If the \code{lst} is of type \code{factor} then \code{check_numeric_list}, and any
#'                  of the \code{numericColumns} family of functions, won't be able to detect whether the
#'                  object can be numeric or not, and it may lead to unexpected results.
#'
#' @examples
#'
#' \dontrun{
#' check_numeric_list(c(1,2,-5,6.7))
#' [1] TRUE
#' check_numeric_list(c("-2.7", 7, 9, -5, "9.8"))
#' [1] TRUE
#' check_numeric_list(list("-2.7", 7, 9, -5, "9.8"))
#' [1] TRUE
#' check_numeric_list(c("1","3", -5, "bacon"))
#' [1] FALSE
#' check_numeric_list(c(TRUE,TRUE, FALSE))
#' [1] FALSE
#' check_numeric_list(c(TRUE,TRUE, FALSE),with_bool = T)
#' [1] TRUE
#' }
#'
#' @export


check_numeric_list = function(lst, with_bool = FALSE){
  ## Applies check_numeric_object to all elemnts in lst
  ## Returns whether all elements passes the test.
  isNumeric = suppressWarnings(all(lapply(lst, check_numeric_object, with_bool = with_bool)))
  return(isNumeric)
}
