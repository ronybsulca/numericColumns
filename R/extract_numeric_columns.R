#' extract_numeric_columns
#'
#' This function returns all of the columns of the given \code{dataset} whose values are numeric or have a
#' meaningfull numerical equivalent. If \code{isCSV} is \code{TRUE}, then \code{dataset} is taken a string
#' depicitng the name of a CSV file. \code{with_bool} specifies if booleans are taken as numeric as well.
#'
#' @param dataset A DataFrame of Data.Table object whose columns will be inspected for numeric values.
#'                If \code{isCSV} is \code{TRUE}, then dataset is the name of the CSV file to inspect.
#' @param isCSV bool. If \code{TRUE}, the dataset will be read from the CSV file depicted in \code{dataset}.
#' @param columns vector list of column names to coerce which columns the analysis will apply to. The given
#'                \code{columns} that do not pass the numeric test will be returned as NAs. If default
#'                \code{extract_numeric_columns} will apply the numeric test to all columns and return
#'                only the ones that pass it.
#' @param row_names bool or integer. Only applied when \code{isCSV} is \code{TRUE}. If \code{TRUE},
#'                  the dataset will be read and returned with their row names. If \code{FALSE},
#'                  the dataset won't be read with row names. If \code{row_names} is an integer,
#'                  then that index will be used consider the row names column.
#' @param with_bool bool. Specifies whether we consider booleans as numeric values.
#' @param header bool. Same Parameters as that in \code{read.csv}. Specifies if the CSV being read contains
#'               headers in the first row. \code{TRUE} by default.
#'
#' @return This function returns a Data Frame object, with row names given based on the \code{row_names} argument.
#'         It includes all the columns of the given dataset whose elements are all numeric or have
#'         a meaningful numerical equivalent.
#' @section Errors: \code{extract_numeric_columns} won't be able to detect numeric values in columns of
#'          data type \code{factor}. If a numeric column is being read from a csv as factor by this function,
#'          then the dataset must be given as data table or data frame object (isCSV = FALSE).
#'
#' @examples
#'
#' \dontrun{
#' extract_numeric_columns(mtcars)
#' extract_numeric_columns(mtcars, columns = c("mpg", "cyl"))
#' extract_numeric_columns("test_data_1.csv", isCSV = TRUE)
#' extract_numeric_columns("test_data_2.csv", isCSV = TRUE, row_names = TRUE, with_bool = TRUE)
#' }
#'
#' @export
#'

extract_numeric_columns = function(dataset, isCSV = FALSE, columns = NULL,
                                   row_names = FALSE, with_bool = FALSE, header = TRUE){
  ## Read CSV
  if (isCSV){
    ## Verifying if rownames must be read
    if(!row_names){
      dataset = read.csv(dataset, header = header)
    } else {
      dataset = read.csv(dataset, header = header, row.names = as.numeric(row_names))
    }
  }
  r_names = row.names(dataset) # remember row names for later
  dataset = data.frame(dataset)

  if (is.null(columns)){
    ## Idenitfy the numerical columns
    numeric_columns = unlist(lapply(dataset, check_numeric_list, with_bool = with_bool))
    proper_dataset = dataset[,eval(numeric_columns)]
  } else{
    ## Coerce which columns will be returned
    proper_dataset = dataset[, columns]
  }
  proper_dataset = data.frame(lapply(proper_dataset, as.numeric)) # Convert the columns to numeric
  row.names(proper_dataset) = r_names # Apply the row names (in case they were deleted)
  return(proper_dataset)
}
