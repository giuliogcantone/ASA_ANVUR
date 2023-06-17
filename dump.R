
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
ù


"management, torino" %>% str_detect("(?=.*?manage)(?=.*?turin|torino)")


tibble(department = c("economics and engineering", "econom")) %>%
  mutate(
    department = department %>% tolower(),
    D_13 =
      case_when(
        department %>%
          str_detect("engin|medic|chemical|software|energ") ~ 0,
        department %>%
          str_detect("econom|statis|busines|impres|manage|finan") ~ 1,
        TRUE ~ 0
      ))

sampling_frame %>%
  filter(title %>% str_detect("Model-based clustering via new p")) -> assign
