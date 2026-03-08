res_deb |> mutate( 
      j5 = as.character(j5), 
      j5 = if_else(j0 == 3 & nchar(j5) == 6, paste0("0", j5), j5), 
  site_id = substr(j5, 1, 3)) |> 
  group_by(j0, j42, site_id) |>
  summarise(
    n = n())


test <- res_deb |>
    mutate( 
      j5 = as.character(j5), 
      j5 = if_else(j0 == 3 & nchar(j5) == 6, paste0("0", j5), j5) 
    ) |> 
  # extract the first 3 characters of j5
  site_id = substr(j5, 1, 3) |> 
  group_by(j0, site_id, j4) |>
  

