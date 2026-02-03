# helper fn
prep_matches <- function(
  data,
  ...,
  type,
  by = "match_id",
  obj = "m"
) {
  if (type > 1) {
    type_prev <- seq_len(type - 1L)
    names_prev <- paste0(obj, type_prev)

    m_prev_l <- lapply(names_prev, get)
    m_prev <- map_dfr(m_prev_l, ~ select(.x, all_of(by))) %>% distinct()
    data_sub <- data %>% anti_join(m_prev, by = by)
  } else {
    data_sub <- data
  }

  out <- data_sub %>%
    filter(...) %>%
    mutate(match_type = type, .before = 1)

  return(out)
}
