original_tibble <- tribble(
  ~ID, ~type, ~name, ~p,
  1,   "Journal", "a",   1,
  1,   "Journal", "b",   2,
  1,   "Journal", "c",   3,
  1,   "Paper",   "b",   4,
  1,   "Paper",   "c",   5,
  1,   "Paper",   "d",   6,
  2,   "Journal", "a",   1,
  2,   "Journal", "b",   2,
  2,   "Paper",   "a",   3,
  2,   "Paper",   "c",   4,
  2,   "Paper",   "f",   5
)

original_tibble %>%
  complete(ID, type, name, fill = list(p = 0))


  pivot_wider(
    names_from = type,
    values_from = p,
    names_prefix = "p_",
    values_fill = 0
  )

# Generate the derived tibble
original_tibble %>%
  filter(type == "Paper") %>%
  rename(Paper = name, Paper_p = p) %>%
  select(-type) %>%
  right_join(original_tibble %>%
               filter(type == "Journal") %>%
               rename(Journal = name, Journal_p = p) %>%
               select(-type), by = c("ID","Journal") %>%
  mutate(Paper_p = ifelse(is.na(Paper_p), 0, Paper_p),
         Journal_p = ifelse(is.na(Journal_p), 0, Journal_p)) %>%
  select(ID, Paper, Journal, Paper_p, Journal_p) %>% View()










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
