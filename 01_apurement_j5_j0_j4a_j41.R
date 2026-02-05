#Voir les classes de toutes les variables d'un dataframe
classe <- function(file) {
  class(file)
  purrr::map_chr(file, ~ class(.x)[1])
}

classe (res_deb)

# Calcul du nombre de caractère dans j5 selon j0

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
  grouped <- data %>% 
    group_by(
      j0,
      across(ends_with("_length"))
      ) %>% 
    summarise(
      n = n(), 
      .groups = "drop"
      ) %>% 
    arrange(
      j0,
      across(ends_with("_length")
            )
      ) 
  
  # Step 5: compute min for selected *_length columns 
  min_table <- data %>% 
    summarise( 
      j5_length_min = min(j5_length, na.rm = TRUE),
      j0_length_min = min(j0_length, na.rm = TRUE), 
      j4_a_length_min = min(j4_a_length, na.rm = TRUE), 
      j41_length_min = min(j41_length, na.rm = TRUE) 
      ) %>% 
  rename( 
    hh_ID_length_min = j5_length_min, 
    Obs_length_min = j0_length_min, 
    village_length_min = j4_a_length_min, 
    commune_length_min = j41_length_min 
  )
  
  # Step 6: compute max for selected *_length columns 
  max_table <- data %>% 
    summarise( 
      j5_length_max = max(j5_length, na.rm = TRUE), 
      j0_length_max = max(j0_length, na.rm = TRUE), 
      j4_a_length_max = max(j4_a_length, na.rm = TRUE), 
      j41_length_max = max(j41_length, na.rm = TRUE) 
    ) %>% 
    rename( 
      hh_ID_length_max = j5_length_max, 
      Obs_length_max = j0_length_max, 
      village_length_max = j4_a_length_max, 
      commune_length_max = j41_length_max 
      )
  
  # Return both results as a list 
  list( 
    grouped_table = grouped, 
    min_table = min_table,
    max_table = max_table 
    )
  
  }


length_analysis (res_deb)

result <- length_analysis(res_deb) 
result$grouped_table # tableau groupé 
result$min_table # tableau min
result$max_table # tableau max

# Ce code convertit les ID de ménage en caractrères et rajoute un 0 devant 
# pour les observatoires avec code "3" (Marovoay") et un identifiant à 6 chiffres
res_deb <- read_dta("data/dta_format/res_deb.dta") %>%
  mutate(
    j5 = as.character(j5),
    j5 = if_else(j0 == 3 & nchar(j5) == 6, paste0("0", j5), j5)
  )