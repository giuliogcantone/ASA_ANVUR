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


db$works[1:100,] %>%
  mutate(internal_references =
           map(references,
               ~str_extract_all(.,id_list)
           )) -> prova2

str_c(db$works$id,
      collapse = "|") -> id_list

str_c(db$works$id

prova2 <- db$works[1:10,] %>%
  mutate(internal_references = sapply(references, function(x) {
    str_extract_all(x, str_c(db$works$id, collapse = "|"))
  }))

db$works[1:10,] %>%
  mutate(internal_references =
           str_remove_all(
             references,
             str_c("?!(",
                    str_c(db$works$id, collapse = "|"),
                    ")\\w+")
             )
         ) -> prova2


prova2$internal_references[1]

db$works$i