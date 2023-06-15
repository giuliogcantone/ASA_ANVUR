
db$journals %>%
  filter(!is.na(x_concepts)) %>%
  unnest(con)


mutate(x_concepts = map(x_concepts,
                        ~select(.x,
                                c("display_name",
                                  "score",
                                  "level")))
) %>%
  select(display_name, x_concepts) %>%
  unnest(x_concepts,
         names_sep = "_") %>%
  rename(Concept = x_concepts_display_name,
         Score = x_concepts_score,
         Level = x_concepts_level) -> db$joun_conc

db$joun_conc %>%
  summarise(Score = sum(Score),
            .by = c("Concept", "Level")
  ) %>%
  arrange(Score) -> db$concepts



International Authors

```{r}
db$works %>% unnest(author) -> db$authors

db$authors %>%
  summarise(
    score = sum(cited_by_count)/n(),
    .by = c(au_display_name,
            institution_country_code,
            institution_id)
  ) %>% nrow()

```
