##########################################################################################
#                                 Form 10-K and 20-F                                     #
##########################################################################################
`%>%` <- magrittr::`%>%`
f20 <- readtext::readtext("data-raw/form_20-f_definition_document_keyword_pages.txt") %>%
  tidytext::unnest_tokens(words, text) %>%
  dplyr::filter(!words %in% tidytext::stop_words$word) %>%
  dplyr::count(words) %>%
  dplyr::filter(!stringi::stri_detect(words, regex = "[0-9]")) %>%
  dplyr::rename(n_20f = n) %>%
  tidyr::replace_na(, 0)

k10 <- readtext::readtext("data-raw/form_10-k_definition_document_keyword_pages.txt") %>%
  tidytext::unnest_tokens(words, text) %>%
  dplyr::filter(!words %in% tidytext::stop_words$word) %>%
  dplyr::count(words) %>%
  dplyr::filter(!stringi::stri_detect(words, regex = "[0-9]")) %>%
  dplyr::rename(n_10k = n)

tab_k10f20 <- k10 %>% 
  dplyr::full_join(f20, by = "words") %>% 
  dplyr::arrange(words) %>%
  tidyr::replace_na(list(n_10k = 0, n_20f = 0))

usethis::use_data(tab_k10f20, overwrite = TRUE)