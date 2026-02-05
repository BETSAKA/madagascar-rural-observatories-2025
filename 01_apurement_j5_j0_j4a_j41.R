#Voir les classes de toutes les variables d'un dataframe
classe <- function(file) {
  class(file)
  purrr::map_chr(file, ~ class(.x)[1])
}

classe (res_deb)


# Calcul du nombre de caractère dans j5 selon j0
res_deb %>%
  mutate(j5_length = nchar(as.character(j5))) %>%
  group_by(j0, j5_length) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(j0, j5_length)


length_analysis <- function(data, y = c("j5", "j0", "j4_a", "j41")){ 
  # Step 1: keep only the columns that exist 
  existing_cols <- intersect(y, names(data)) 
  # Step 2: convert to character if numeric 
  data <- data %>% 
    mutate(across(all_of(existing_cols),  ~ if(is.numeric(.x)) as.character(.x) else .x)) 
  # Step 3: create *_length columns 
  data <- data %>% 
    mutate(across(all_of(existing_cols), 
                  ~ nchar(as.character(.x)), 
                  .names = "{.col}_length")
    ) 
  # Step 4: group_by on original columns + length columns 
  data %>% group_by(
    j0,
    #across(all_of(existing_cols)), 
    across(ends_with("_length"))
  ) %>% 
    summarise(n = n(), .groups = "drop"
              ) %>% 
    arrange(
      j0,
      #across(all_of(existing_cols)), 
      across(ends_with("_length"))
    ) 
  }


length_analysis (res_deb)


# Ce code convertit les ID de ménage en caractrères et rajoute un 0 devant 
# pour les observatoires avec code "3" (Marovoay") et un identifiant à 6 chiffres
res_deb <- read_dta("data/dta_format/res_deb.dta") %>%
  mutate(
    j5 = as.character(j5),
    j5 = if_else(j0 == 3 & nchar(j5) == 6, paste0("0", j5), j5)
  )