# PROFILE BASED STANDARDIZATION (PBS) ====================================================
#' Profile Based Standardization (PBS)
#'
#' @param table 
#' A dataframe with 4 columns (columns need to be in the right order)\cr
#' column 1: A document identifier\cr
#' column 2: The concept id\cr
#' column 3: A term identifier\cr
#' column 4: The term frequency
#' 
#' @param cl
#' Number of cores to be used
#' 
#' @return
#' A dataframe with showing the PBS for each unique firm-pair for each concept
#' @export
#'
#' @examples
#' library (RFtex)
#' 
#' table <- tibble::tibble(
#' doc_id = c(1,1,1,1,1,2,2,2,2,3,3,3,3),
#' con_id = c(1,1,2,2,2,1,1,3,3,1,3,2,3),
#' term   = c("a", "b", "c", "d", "e", "a", "b", "f", "g", "a", "f", "e", "g"),
#' freq   = c(21,12,58,32,14,21,14,66,14,12,85,100,12))
#' 
#' pbs(table)
pbs <- function(table, clus = 1) {
  
  `%>%` <- magrittr::`%>%`
  colnames(table) <- c("doc_id", "con_id", "term", "freq")
  
  miss <- table %>% dplyr::distinct(con_id, term) %>%
    tidyr::crossing(doc_id = table$doc_id) %>%
    dplyr::anti_join(table, by = c("con_id", "term", "doc_id")) %>%
    dplyr::mutate(freq = 0)
  
  table <- dplyr::bind_rows(table, miss) %>%
    dplyr::group_by(doc_id, con_id) %>%
    dplyr::mutate(rel = freq / sum(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-freq) %>%
    dplyr::filter(!is.nan(rel)) %>%
    tidyr::spread(doc_id, rel) %>%
    dplyr::arrange(con_id)
  
  con.list <- split(table, dplyr::group_indices(table, con_id))
  names(con.list) <- sort(unique(table$con_id))


  clus <- parallel::makeCluster(mc <- getOption("cl.cores", clus))
  parallel::clusterExport(clus, c("con.list", "%>%"), envir = environment())



  pbs <- parallel::parLapply(1:length(con.list), function(a) {
    man <- lapply(1:nrow(con.list[[a]]), function(x) {
      as.matrix(dist(t(as.matrix(con.list[[a]][x, -(1:2)])), method = "manhattan"))
    })
    man <- 1 - purrr::reduce(man, `+`) / 2
    man[lower.tri(man, TRUE)] <- NA
    man <- reshape2::melt(man) %>% 
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(con_id = as.integer(names(con.list)[a]))
    return(man)
  }, cl = clus) %>% dplyr::bind_rows() %>%
    dplyr::select(con_id, doc1 = Var1, doc2 = Var2, pbs = value)
  
  parallel::stopCluster(clus)
  
  return(pbs)
}
