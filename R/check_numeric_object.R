#' check_numeric_object
#'
#' \code{check_numeric_object} returns \code{TRUE} if an \code{object} can be a numeric value. It returns true
#' if the \code{object} is \code{numeric} or if it detects numeric values in character form,
#' such as "1", "3.4", "-76", etc.
#' If \code{with_bool} is set to \code{TRUE}, \code{check_numeric_object} assumes booleans
#' can also have an equivalent numeric form (being 1 and 0).
#'
#' @param object an object (size-1 vector) of type numeric, character, or boolean.
#' @param with_bool bool. Specifies whether we consider booleans as numeric values.
#'
#' @return This function returns True if the object can be converted to an equivalent numeric form.
#'
#' @section Errors: If the \code{object} is of type \code{factor} then \code{check_numeric_object}, and any
#'                  of the \code{numericColumns} family of functions, won't be able to detect whether the
#'                  object can be numeric.
#'          Errors: Must be given vectors of size = 1, otherwise the return values maybe of unexpected size.
#'
#' @examples
#'
#' \dontrun{
#' check_numeric_object(1)
#' [1] TRUE
#' check_numeric_object("-2.7")
#' [1] TRUE
#' check_numeric_object("bacon1")
#' [1] FALSE
#' check_numeric_object(TRUE)
#' [1] FALSE
#' check_numeric_object(TRUE, with_bool = TRUE)
#' [1] TRUE
#' }
#'
#' @export

check_numeric_object = function(object, with_bool = FALSE){
  ## Detects whether the object is numeric or has a direct numeric equivalent
  ## It filters out for factors, since words as factors can be converted
  ## numeric values, but not have a meaningful numeric equivalent
  if (with_bool){
    isNumber = suppressWarnings( (is.numeric(object) | !is.na(as.numeric(object))) &
                                   !is.factor(object))
  } else{
    ## If with_bool is FALSE, also check the object is not boolean
    isNumber = suppressWarnings( ( is.numeric(object) |
                                     (!is.na(as.numeric(object)) && !is.logical(object))
                                 ) &
                                 !is.factor(object)
                               )
  }
  return(isNumber)
}
