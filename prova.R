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


total_g %>%
  ggraph() +
  geom_edge_link(aes(alpha = n), show.legend = FALSE) +
  geom_node_point(aes(colour = macro_cluster),
                  size = 2) +
  scale_color_manual(values = c("STEM-oriented Statistics" = "violet",
                                "Social-oriented Statistics" = "yellow"
  )) +
  geom_text(x = -1.1, y = 1.85, label = "International Sample",
            size = 5.5, hjust = 0.5, vjust = 0.5) +
  theme_graph() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.99),
    legend.justification = c(0, 1),
    legend.title = element_blank()
  )
