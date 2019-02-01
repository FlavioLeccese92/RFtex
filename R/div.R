#' Create directories
#'
#' @param dirs A character string of full path dirnames
#' @param rec Shall dirs be created recursively? (default: TRUE)
#'
#' @export
#'
#' @examples
#' dirs <- c("test/testdir1", "test/testdir2", "test/testdir1")
#' dir_create(dirs)
#' list.dirs("test/", recursive = FALSE)
#' system(paste0("rm -r ", "test/"))
#' 
dir_create <- function(dirs, rec = TRUE) {
  for (i in dirs) if(!dir.exists(i)) dir.create(i, recursive = rec)
}
