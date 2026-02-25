# Function 01 : View the classes of all variables in a dataframe
classe <- function(file) {
  class(file)
  purrr::map_chr(file, ~ class(.x)[1])
}

# Function 02 : Calcul du nombre de caractère dans j5 selon j0

length_analysis <- function(data, y = c("j5", "j0", "j4_a", "j41")){ 
  # Keep only the columns that exist 
  existing_cols <- intersect(y, names(data)) 
  # Convert to character if numeric 
  data <- data %>% 
    mutate(across(all_of(existing_cols),  ~ if(is.numeric(.x)) as.character(.x) else .x)) 
  # Create *_length columns 
  data <- data %>% 
    mutate(across(all_of(existing_cols), 
                  ~ nchar(as.character(.x)), 
                  .names = "{.col}_length")
    ) 
  # Group_by on original columns + length columns 
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
  
  # Compute min for selected *_length columns 
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
  
  # Compute max for selected *_length columns 
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


# Function 03 : Adding a zero in front of j5 observatory codes when their length is six
add_j5 <- function(data){ 
    data %>% 
    mutate( 
      j5 = as.character(j5), 
      j5 = if_else(j0 == 3 & nchar(j5) == 6, paste0("0", j5), j5) 
    ) 
}


