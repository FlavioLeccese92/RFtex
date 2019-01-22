#' GENERATE IDs for chunk splits
#'
#' @param num Number of chunks
#' @param len Length of the chunk vector
#' @param sample Shall the IDs be randomized
#'
#' @return
#' An integer vector
#' @export
#'
#' @examples
#' id_chunk(5, 28, FALSE)
#' id_chunk(5, 28, TRUE)
id_chunk <- function(num, len, sample = FALSE) {
  id <- rep(1:ceiling(len / num), each = num)
  if (isTRUE(sample))
    id <- sample(id)
  id <- id[1:len]
  return(id)
}

#' GENERATE IDs for splits
#'
#' @param num Number of splits
#' @param len Length of the split vector
#' @param sample Shall the IDs be randomized
#'
#' @return
#' An integer vector
#' @export
#'
#' @examples
#' id_split(5, 28, FALSE)
#' id_split(5, 28, TRUE)
id_split <- function(num, len, sample = FALSE) {
  id <- rep(1:num, each = ceiling(len / num))
  if (isTRUE(sample))
    id <- sample(id)
  id <- id[1:len]
  return(id)
}
