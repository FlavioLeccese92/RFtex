# PROFILE BASED STANDARDIZATION (cbs) ====================================================
#' Profile Based Standardization (cbs)
#'
#' @param table 
#' A dataframe with 4 columns (columns need to be in the right order)\cr
#' column 1: A document identifier\cr
#' column 2: The concept id\cr
#' column 3: A term identifier\cr
#' column 4: The term frequency
#' 
#' @param clus
#' Number of cores to be used
#' 
#' @return
#' A dataframe with showing the cbs for each unique firm-pair for each concept
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
#' cbs_mult(table)
#' clus <- parallel::makeCluster(getOption("cl.cores", 3))
#' cbs_mult(table, clus = clus)
cbs_mult <- function(table, clus = NULL) {
  
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


  if (!is.null(clus)) {
    parallel::clusterExport(clus, c("con.list", "%>%"), envir = environment())
    
    cbs <- parallel::parLapply(1:length(con.list), function(a) {
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
      dplyr::select(con_id, doc1 = Var1, doc2 = Var2, cbs = value)
    
    parallel::stopCluster(clus)
  } else {
    cbs <- lapply(1:length(con.list), function(a) {
      man <- lapply(1:nrow(con.list[[a]]), function(x) {
        as.matrix(dist(t(as.matrix(con.list[[a]][x, -(1:2)])), method = "manhattan"))
      })
      man <- 1 - purrr::reduce(man, `+`) / 2
      man[lower.tri(man, TRUE)] <- NA
      man <- reshape2::melt(man) %>% 
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(con_id = as.integer(names(con.list)[a]))
      return(man)
    }) %>% dplyr::bind_rows() %>%
      dplyr::select(con_id, doc1 = Var1, doc2 = Var2, cbs = value)
  }
  return(cbs)
}

# CONCEPT BASED STANDARDIZATION (cbs) ====================================================
#' Concept Based Standardization (cbs)
#'
#' @param table 
#' A dataframe with 4 columns (columns need to be in the right order)\cr
#' column 1: A document identifier\cr
#' column 2: The concept id\cr
#' column 3: A term identifier\cr
#' column 4: The term frequency
#' 
#' @return
#' A dataframe with showing the cbs for each unique firm-pair for each concept
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
#' con.list <- split(table, dplyr::group_indices(table, con_id))
#' names(con.list) <- sort(unique(table$con_id))
#' 
#' cbs_sing(con.list[[1]])
#' lapply(con.list, cbs_sing)
cbs_sing <- function(table, out = c("table", "matrix")) {
  
  `%>%` <- magrittr::`%>%`
  colnames(table) <- c("doc_id", "con_id", "term", "freq")
  
  if (length(unique(table$con_id)) > 1) stop("enter only one concept id")
  out <- out[1]
  
  
  con.freq <- table %>% dplyr::select(-con_id, -term) %>%
    mutate(doc_id = as.integer(doc_id))
  
  
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
  
  cbs <- lapply(1:nrow(table), function(x) {
    as.matrix(dist(t(as.matrix(table[x, -(1:2)])), method = "manhattan"))
  })
  cbs <- 1 - purrr::reduce(cbs, `+`) / 2
  
  if (out == "table") {
    cbs[lower.tri(cbs, TRUE)] <- NA
    cbs <- reshape2::melt(cbs) %>% dplyr::filter(!is.na(value)) %>%
      dplyr::select(doc1 = Var1, doc2 = Var2, cbs = value) %>%
      dplyr::left_join(con.freq, by = c("doc1" = "doc_id")) %>%
      dplyr::left_join(con.freq, by = c("doc2" = "doc_id")) %>%
      dplyr::mutate(freq = freq_adj.x + freq_adj.y) %>%
      dplyr::select(-freq_adj.x, -freq_adj.y)
  }
  
  return(cbs)
}

