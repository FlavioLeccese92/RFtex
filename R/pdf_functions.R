# PDF TO TEXT ============================================================================
#' Converts a PDF to text file
#'
#' @description
#' This function is part of the 'pdf operations' (pop) function set\cr
#' This function converts pdf files into plain text files
#'
#' @param file
#' A PDF file
#'
#' @param outdir
#' The folder where the txt file should be saved\cr
#' (If NULL a new folder called 'txt' will be created in the same directory where the pdf
#' file is located)
#'
#' @param wait
#' Shall the program wait to trigger another instance of pdftotext.exe? (meaningfull if
#' a lot of different pdf files have to be converted, but is slower)
#'
#'
#' @return A text file (the text file will have the same name as the pdf file)
#' @export
pdf_to_txt <- function(file, outdir = NULL, wait = FALSE) {
  if (is.null(outdir)) {
    outdir <- paste0(dirname(file), "/txt")
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  } else {
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  }
  
  outfile <- paste0(outdir, "/", gsub("\\.pdf", ".txt", basename(file)))
  file    <- paste0('"', file, '"')
  outfile <- paste0('"', outfile, '"')
  
tryCatch(
    system(paste("pdftotext", file, outfile), ignore.stderr = TRUE, wait = wait),
    error = function(e)
      "error"
  )
}