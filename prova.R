db$works[1:100,] %>%
  mutate(internal_references =
           map(references,
               ~str_extract_all(.,
                                str_c(db$works$id,
                                      collapse = "|"))
  )) %>%
  mutate(internal_references =
             map(internal_references,
                 discard, ~length(.x) == 0)) -> prova2


db$works %>%
  unnest(references) %>%
  transmute(from = id,
            to = references,
            from_j = journal) %>%
  filter(to %in% db$works$id) %>%
  left_join(.,db$works %>% select(to_j = journal,id),
            by = c("to" = "id")
            )-> edgelist

db$works$id
