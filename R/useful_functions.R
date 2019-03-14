# save with backup =======================================================================
#' Save Dataframe to .rds or .csv with Backup File
#'
#' @description 
#' Saves a dataframe to the specified location and creates another backup file with 
#' date-time extension (yy-mm-dd_HH-MM-SS)
#'
#' @param x A dataframe
#' @param full_path The full path containing the folder and filename with file extension
#' @param compress Only available for .rds files (one of 'none' or 'gz')
#'
#' @export
#'
#' @examples
#' library(RFtex)
#' 
#' df <- tibble::tibble(col = c(1,2,3))
#' bu_save(df, "test_df.rds", "none")
bu_save <- function(x, full_path, compress = c("none", "gz")) {

  file.ext <- tools::file_ext(full_path)
  
  bu.time <- format(Sys.time(), "%y-%m-%d_%H-%M-%S")
  bu.path <- paste0(dirname(full_path), "/", bu.time, "_", basename(full_path))
  
  if (file.ext == "rds") {
    readr::write_rds(x = x, path = full_path, compress = compress[1])
    readr::write_rds(x = x, path = bu.path, compress = compress[1])
  } else if (file.ext == "csv") {
    readr::write_delim(x = x, path = full_path, delim = ";", na = "")
    readr::write_delim(x = x, path = bu.path, delim = ";", na = "")
  }
  
}