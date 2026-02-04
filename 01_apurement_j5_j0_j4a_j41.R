

# Calcul du nombre de caractère dans j5 selon j0
res_deb %>%
  mutate(j5_length = nchar(as.character(j5))) %>%
  group_by(j0, j5_length) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(j0, j5_length)