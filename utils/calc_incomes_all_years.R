# Compute incomes  --------------------------------------------------------

# Cette collection de fonctions sert à calculer les revenus agrégés 
# à partir des données OR pour les années 1995 à 2015

# Libraries 
library(tidyverse)
library(haven)

# In Stata: 
# gen revcou = revppal + revsec + rev_riz + rev_cu + revel + revpeche
# gen revexcept = rente_riz + rente_cu + autre_rev + himo + decap + vte_biens + transrecmo + transrecnomo
# gen revtot = revcou + revexcept

# gen decap = vte_par + vente_bovin + vente_beef + vte_equip


# revppal OK-----------------------------------------------------------------

# Process incomes from main activities
process_revppal <- function(path = "data/ROS_MDG_microdata/", year) {
  # From 2012: number of weeks * week income (in cash and in kind)
  if(year > 2011) {
    read_dta(paste0(path, year, "/res_m_a.dta")) %>%
      mutate(across(c(a3b, a3c, a3e, a3f), ~ replace_na(.x, 0))) %>%
      mutate(revppal = ((a3c * a3b * 1000) + (a3e * a3f * 1000))) %>%
      group_by(j5, year) %>%
      summarise(revppal = sum(revppal, na.rm = TRUE))
    # Before 2012: only cash income
  } else if (year > 1997) {
    read_dta(paste0(path, year, "/res_m_a.dta")) %>%
      mutate(across(c(a3b, a3c), ~ replace_na(.x, 0))) %>%
      mutate(revppal = a3c * a3b * 1000) %>%
      group_by(j5, year) %>%
      summarise(revppal = sum(revppal, na.rm = TRUE))
    # in 1996 & 1997: the number of weeks is missing: we assume 52 (most cases in)
  } else if (year > 1995) {
    read_dta(paste0(path, year, "/res_m_a.dta")) %>%
      mutate(a3b = replace_na(a3b, 0)) %>%
      mutate(revppal = a3b * 52 * 1000) %>%
      group_by(j5, year) %>%
      summarise(revppal = sum(revppal, na.rm = TRUE), .groups = "drop")
    # Not available in 1995
  } else {
    return(tibble(j5 = character(), year = year, revppal = NA_real_))
  }
}


# revsec OK------------------------------------------------------------------

# Process incomes from secondary activities
process_revsec <- function(path = "data/ROS_MDG_microdata/", year) {
  # From 2011: income per culture in cash and in nature
  if (year > 2011) {
    read_dta(paste0(path, year, "/res_as.dta")) %>%
      mutate(across(c(as4, as3, as4a, as3a), ~ replace_na(.x, 0))) %>%
      mutate(revsec = (as3 * as4) + (as4a * as3a)) %>%
      group_by(j5, year) %>%
      summarise(revsec = sum(revsec, na.rm = TRUE))
    # Before 2012, only income in cash (duration * income per week)
    # but specific case in 2005
  } else if (year == 2005) {
    read_dta(paste0(path, year, "/res_as.dta")) %>%
      mutate(across(c(as4_1, as3_1), ~ replace_na(.x, 0))) %>%
      mutate(revsec = as3_1 * as4_1) %>%
      group_by(j5, year) %>%
      summarise(revsec = sum(revsec, na.rm = TRUE))
    # in 1996 : only income in cash, but month begin and month ends
  } else if (year > 1996) {
    read_dta(paste0(path, year, "/res_as.dta")) %>%
      mutate(as4 = as.numeric(as4), # As character in 2003
             across(c(as4, as3), ~ replace_na(.x, 0)),
             revsec = as3 * as4) %>%
      group_by(j5, year) %>%
      summarise(revsec = sum(revsec, na.rm = TRUE))
    # in 1996 : only income in cash, but month begin and month ends
  } else if (year == 1996) {
    read_dta(paste0(path, year, "/res_as.dta")) %>%
      group_by(j5, year) %>%
      summarise(revsec = sum(as3b, na.rm = TRUE))
    # Value not available in 1995
  } else if (year == 1995) {
    return(tibble(j5 = character(), year = year, revsec = NA_real_))
  }
} 
# rev_riz -----------------------------------------------------------------

## prod_riz_val OK ----------------------------------------------------

process_prod_riz_val <- function(year, path = "data/ROS_MDG_microdata/") {
  stopifnot(year %in% 1995:2015)
  
  # Identifier j5 -> obs
  household_to_obs <- read_dta(paste0(path, year, "/res_deb.dta")) %>%
    transmute(j5, obs = j0)
  
  # Charger données de production
  df_r <- read_dta(paste0(path, year, "/res_r.dta"))
  
  # Charger données de prix
  df_dc21 <- read_dta(paste0(path, year, "/res_dc21.dta")) %>%
    left_join(household_to_obs, by = "j5")
  
  # Cas simple : r23 unique (1995–1996, 2009–2015)
  
  if (year %in% c(1995, 2009:2015)) {
    prod_riz <- df_r %>%
      mutate(prod_riz = r23) %>%
      group_by(j5, year) %>%
      summarise(prod_riz = sum(prod_riz, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
    px_paddy_obs <- df_dc21 %>%
      group_by(obs, year) %>%
      summarise(
        dc22 = sum(dc22, na.rm = TRUE),
        dc25 = sum(dc25, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pxpaddy_obs = if_else(dc22 > 0, dc25 / dc22, NA_real_))
    
    prod_riz_val <- prod_riz %>%
      left_join(px_paddy_obs, by = c("obs", "year")) %>%
      mutate(prod_riz_val = prod_riz * pxpaddy_obs) %>%
      select(j5, year, prod_riz_val)
    
    return(prod_riz_val)
  } else if (year %in% 1996:2008) { # Cas complexe : r23a / r23b séparés (1997–2008)
    
    if (year == 2001) {
      # Production principale
      prod_riz_a <- df_r %>%
        transmute(j5, year, prod_riz_a = rowSums(across(c(r23a1, r23a2)), 
                                                 na.rm = TRUE)) %>%
        group_by(j5, year) %>%
        summarise(prod_riz_a = sum(prod_riz_a, na.rm = TRUE), .groups = "drop") %>%
        left_join(household_to_obs, by = "j5")
      # Production contre-saison
      prod_riz_b <- df_r %>%
        transmute(j5, year, prod_riz_b = rowSums(across(c(r23b1, r23b2)), 
                                                 na.rm = TRUE)) %>%
        group_by(j5, year) %>%
        summarise(prod_riz_b = sum(prod_riz_b, na.rm = TRUE), .groups = "drop") %>%
        left_join(household_to_obs, by = "j5")
    } else {
      # Production principale
      prod_riz_a <- df_r %>%
        transmute(j5, year, prod_riz_a = coalesce(r23a, 0)) %>%
        group_by(j5, year) %>%
        summarise(prod_riz_a = sum(prod_riz_a, na.rm = TRUE), .groups = "drop") %>%
        left_join(household_to_obs, by = "j5")
      
      # Production contre-saison
      prod_riz_b <- df_r %>%
        transmute(j5, year, prod_riz_b = coalesce(r23b, 0)) %>%
        group_by(j5, year) %>%
        summarise(prod_riz_b = sum(prod_riz_b, na.rm = TRUE), .groups = "drop") %>%
        left_join(household_to_obs, by = "j5")
    }
    # Prix moyen saison principale (mois ≠ 9:12)
    prix_a <- df_dc21 %>%
      filter(!dc21 %in% 9:12) %>%
      group_by(obs, year) %>%
      summarise(
        dc22a = sum(dc22, na.rm = TRUE),
        dc25a = sum(dc25, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pxpaddy_a = dc25a / dc22a,
             pxpaddy_a = if_else(year == 1997, pxpaddy_a * 1000, pxpaddy_a)) %>%
      select(obs, year, pxpaddy_a)
    
    # Prix moyen contre-saison (mois 9:12)
    prix_b <- df_dc21 %>%
      filter(dc21 %in% 9:12) %>%
      group_by(obs, year) %>%
      summarise(
        dc22b = sum(dc22, na.rm = TRUE),
        dc25b = sum(dc25, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pxpaddy_b = dc25b / dc22b,
             pxpaddy_b = if_else(year == 1997, pxpaddy_b * 1000, pxpaddy_b)) %>%
      select(obs, year, pxpaddy_b)
    
    # Valorisation
    prod_riz_val <- full_join(prod_riz_a, prod_riz_b, by = c("j5", "year", "obs")) %>%
      left_join(prix_a, by = c("obs", "year")) %>%
      left_join(prix_b, by = c("obs", "year")) %>%
      mutate(
        prod_riz_a = replace_na(prod_riz_a, 0),
        prod_riz_b = replace_na(prod_riz_b, 0),
        pxpaddy_a = replace_na(pxpaddy_a, 0),
        pxpaddy_b = replace_na(pxpaddy_b, 0),
        prod_riz_val = prod_riz_a * pxpaddy_a + prod_riz_b * pxpaddy_b
      ) %>%
      select(j5, year, prod_riz_val)
    
    return(prod_riz_val)
  }
  
  stop("Année non prise en charge.")
}



## recette_riz ------------------
process_prod_riz_val <- function(year, path = "data/ROS_MDG_microdata/") {
  stopifnot(year %in% 1995:2015)
  
  household_to_obs <- read_dta(paste0(path, year, "/res_deb.dta")) %>%
    transmute(j5, obs = j0)
  
  df_r <- read_dta(paste0(path, year, "/res_r.dta"))
  
  df_dc21 <- read_dta(paste0(path, year, "/res_dc21.dta")) %>%
    left_join(household_to_obs, by = "j5")
  
  if (year %in% c(1995, 2009:2015)) {
    
    prod_riz <- df_r %>%
      mutate(prod_riz = r23) %>%
      group_by(j5, year) %>%
      summarise(prod_riz = sum(prod_riz, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
    px_paddy_obs <- df_dc21 %>%
      group_by(obs, year) %>%
      summarise(
        dc22 = sum(dc22, na.rm = TRUE),
        dc25 = sum(dc25, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pxpaddy_obs = if_else(dc22 > 0, dc25 / dc22, NA_real_))
    
    prod_riz_val <- prod_riz %>%
      left_join(px_paddy_obs, by = c("obs", "year")) %>%
      mutate(prod_riz_val = prod_riz * pxpaddy_obs) %>%
      select(j5, year, prod_riz_val)
    
    return(prod_riz_val)
    
  } else if (year == 2001) {
    
    prod_riz_a <- df_r %>%
      transmute(j5, year, prod_riz_a = rowSums(across(c(r23a1, r23a2)), na.rm = TRUE)) %>%
      group_by(j5, year) %>%
      summarise(prod_riz_a = sum(prod_riz_a, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
    prod_riz_b <- df_r %>%
      transmute(j5, year, prod_riz_b = rowSums(across(c(r23b1, r23b2)), na.rm = TRUE)) %>%
      group_by(j5, year) %>%
      summarise(prod_riz_b = sum(prod_riz_b, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
  } else if (year %in% c(1996:2000, 2002:2008)) {
    
    prod_riz_a <- df_r %>%
      transmute(j5, year, prod_riz_a = coalesce(r23a, 0)) %>%
      group_by(j5, year) %>%
      summarise(prod_riz_a = sum(prod_riz_a, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
    prod_riz_b <- df_r %>%
      transmute(j5, year, prod_riz_b = coalesce(r23b, 0)) %>%
      group_by(j5, year) %>%
      summarise(prod_riz_b = sum(prod_riz_b, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5")
    
  } else {
    stop("Année non prise en charge (pas de variables disponibles dans res_r.dta)")
  }
  
  # Traitement prix
  prix_a <- df_dc21 %>%
    filter(!dc21 %in% 9:12) %>%
    group_by(obs, year) %>%
    summarise(dc22a = sum(dc22, na.rm = TRUE), dc25a = sum(dc25, na.rm = TRUE), .groups = "drop") %>%
    mutate(pxpaddy_a = dc25a / dc22a,
           pxpaddy_a = if_else(year == 1997, pxpaddy_a * 1000, pxpaddy_a)) %>%
    select(obs, year, pxpaddy_a)
  
  prix_b <- df_dc21 %>%
    filter(dc21 %in% 9:12) %>%
    group_by(obs, year) %>%
    summarise(dc22b = sum(dc22, na.rm = TRUE), dc25b = sum(dc25, na.rm = TRUE), .groups = "drop") %>%
    mutate(pxpaddy_b = dc25b / dc22b,
           pxpaddy_b = if_else(year == 1997, pxpaddy_b * 1000, pxpaddy_b)) %>%
    select(obs, year, pxpaddy_b)
  
  prod_riz_val <- full_join(prod_riz_a, prod_riz_b, by = c("j5", "year", "obs")) %>%
    left_join(prix_a, by = c("obs", "year")) %>%
    left_join(prix_b, by = c("obs", "year")) %>%
    mutate(
      prod_riz_a = replace_na(prod_riz_a, 0),
      prod_riz_b = replace_na(prod_riz_b, 0),
      pxpaddy_a = replace_na(pxpaddy_a, 0),
      pxpaddy_b = replace_na(pxpaddy_b, 0),
      prod_riz_val = prod_riz_a * pxpaddy_a + prod_riz_b * pxpaddy_b
    ) %>%
    select(j5, year, prod_riz_val)
  
  return(prod_riz_val)
}



## charge_riz ----------------------------------

process_charge_riz <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement des charges riz - année ", year)
  
  # Correspondance ménage-observatoire
  household_to_obs <- read_dta(paste0(path, year, "/res_deb.dta")) %>%
    select(j5, obs = j0) %>%
    distinct() # Household 	012173 duplicated in 1995
  
  # Prix du paddy observé (px_paddy_obs)
  px_paddy_obs <- read_dta(paste0(path, year, "/res_dc21.dta")) %>%
    left_join(household_to_obs, by = "j5") %>%
    group_by(obs, year) %>%
    summarise(
      dc22 = sum(dc22, na.rm = TRUE),
      dc25 = sum(dc25, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pxpaddy_obs = dc25 / dc22, # prix / 1000 en 1997
           pxpaddy_obs = if_else(year == 1997, pxpaddy_obs * 1000, 
                                 pxpaddy_obs)) %>%
    select(obs, year, pxpaddy_obs)
  
  # Main d'œuvre non permanente
  file_mo <- case_when(
    year == 1995 ~ "/res_mo3.dta",
    year %in% c(2013, 2014) ~ "/res_mo.dta",
    TRUE ~ "/res_mo1.dta"
  )
  df <- read_dta(paste0(path, year, file_mo)) 
  
  main_oeuvre <- if (year == 1995) {
    required <- c("j5", "mo31f", "wg", "moc")
    if (!all(required %in% names(df))) 
      stop("Missing variables in 1995 for coutmori")
    
    df %>%
      filter(moc == 1) %>%
      mutate(across(c(mo31f, wg), ~replace_na(.x, 0))) %>%
      mutate(
        salarie = mo31f,
        entraide = wg,
        coutmori = salarie + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmori = sum(coutmori, na.rm = TRUE), .groups = "drop")
    
  } else if (year == 1996) {
    required <- c("j5", "w1b", "w1c", "w1d", "w1e", "w2b", "w2c", "w2d", 
                  "w2e", "w3a", "w3b")
    if (!all(required %in% names(df))) 
      stop("Missing variables in 1996 for coutmori")
    
    df %>%
      mutate(across(all_of(required[-1]), ~replace_na(.x, 0))) %>%
      mutate(
        salarie = w1b * w1c * w1d + w1b * w1c * w1e,
        tache   = w2b * w2c * w2d + w2b * w2c * w2e,
        entraide = w3a + w3b,
        coutmori = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmori = sum(coutmori, na.rm = TRUE), .groups = "drop")
    
  } else if (year == 1997) {
    required <- c("j5", "mo11b", "mo11c", "mo11d", "mo11d2", "mo11e", "mo11e2",
                  "mo12", "mo12b", "mo23", "mo24")
    if (!all(required %in% names(df))) 
      stop("Missing variables in 1997 for coutmori")
    
    df %>%
      mutate(across(all_of(required[-1]), ~replace_na(.x, 0))) %>%
      mutate( # manque : mo11f mo11g mo11h
        salarie = mo11b * mo11d + mo11b * mo11d2 + #pour les hommes
          mo11c * mo11e + mo11c * mo11e2, # pour les femmes
        tache = mo12 + mo12b,
        entraide = mo23 + mo24,
        coutmori = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmori = sum(coutmori, na.rm = TRUE), .groups = "drop")
    
  } else if (year >= 1998 && year <= 2008) {
    required <- c("j5", "mo11c", "mo11e", "mo12", "mo23")
    if (!all(required %in% names(df))) 
      stop("Missing variables 1998–2008 for coutmocu")
    
    df %>%
      mutate(across(all_of(required[-1]), ~replace_na(.x, 0))) %>%
      mutate(
        salarie = mo11c * mo11e,
        tache = mo12,
        entraide = mo23,
        coutmori = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmori = sum(coutmori, na.rm = TRUE), .groups = "drop")
    
  } else {
    required <- c("j5", "mo11b", "mo11c", "mo11e", "mo51a", "mo51b", "mo12", 
                  "mo51c", "mo23", "mo11d")
    if (!all(required %in% names(df))) 
      stop("Missing variables post-2008 for coutmocu")
    
    df %>%
      mutate(across(all_of(required[-1]), ~replace_na(.x, 0))) %>%
      mutate(
        salarie = mo11b * mo11d + mo11b * mo51a + mo11c * mo11e + mo11c * mo51b,
        tache = mo12 + mo51c,
        entraide = mo23,
        coutmori = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmori = sum(coutmori, na.rm = TRUE), .groups = "drop")
  }

  # Intrants (pas disponible en 1995)
  coutint <- tryCatch({
    read_dta(paste0(path, year, "/res_itb.dta")) %>%
      group_by(j5, year) %>%
      summarise(coutint = sum(ita2, na.rm = TRUE), .groups = "drop")
  }, error = function(e) tibble(j5 = character(), year = year, 
                                coutint = NA_real_))

  # Métayage/location
  coutmetloc <- tryCatch({
    df_r <- read_dta(paste0(path, year, "/res_r.dta"))
    
    coutmetloc1 <- df_r %>%
      mutate(rimetloc = r6 * (r4 == 2 | r4 == 3)) %>%
      group_by(j5, year) %>%
      summarise(rimetloc = sum(rimetloc, na.rm = TRUE), .groups = "drop") %>%
      left_join(household_to_obs, by = "j5") %>%
      left_join(px_paddy_obs, by = c("obs", "year")) %>%
      mutate(coutmetloc1 = rimetloc * pxpaddy_obs) %>%
      select(j5, year, coutmetloc1)
    
    coutmetloc2 <- df_r %>%
      mutate(coutmetloc2 = r7 * (r4 == 2 | r4 == 3)) %>%
      group_by(j5, year) %>%
      summarise(coutmetloc2 = sum(coutmetloc2, na.rm = TRUE), .groups = "drop")
    
    coutmetloc1 %>%
      left_join(coutmetloc2, by = c("j5", "year")) %>%
      mutate(coutmetloc = coalesce(coutmetloc1, 0) + coalesce(coutmetloc2, 0)) %>%
      select(j5, year, coutmetloc)
  }, error = function(e) tibble(j5 = character(), year = year, coutmetloc = NA_real_))
  
  # Agrégation finale
  df <- reduce(
    list(main_oeuvre, coutint, coutmetloc),
    full_join, by = c("j5", "year")
  ) %>%
    mutate(
      charge_riz = rowSums(across(starts_with("cout"), ~ replace_na(.x, 0)))
    ) %>%
    select(j5, year, charge_riz)
  
  return(df)
}

## wrapper

process_rev_riz <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement revenu riz - année ", year)
  
  # Valeur de production
  prod_riz_val <- process_prod_riz_val(year, path)
  
  # Charges (main-d'œuvre, intrants, métayage)
  charge_riz <- process_charge_riz(year, path)
  
  # Revenu net = production - charges
  rev_riz <- prod_riz_val %>%
    left_join(charge_riz, by = c("j5", "year")) %>%
    mutate(
      recette_riz = prod_riz_val,
      charge_riz = replace_na(charge_riz, 0),
      rev_riz = recette_riz - charge_riz
    ) %>%
    select(j5, year, rev_riz)
  
  return(rev_riz)
}


# rev_cu ------------------------------------------------------------------


## prodcu_val ----------------------------------------

process_prodcu_val <- function(path = "data/ROS_MDG_microdata/", year) {
  # Définition des données de base selon l'année
  if (year == 1995) {
    df <- read_dta(paste0(path, year, "/res_c.dta"))
    
    prix_cu <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = c1,
        prix_cu = if_else(c4 > 0, c6a, NA_real_)
      ) %>%
      group_by(cult, obs, year) %>%
      summarise(prix_cu = mean(prix_cu, na.rm = TRUE), .groups = "drop")
    
    prodcu_val <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = c1,
        qte = c2
      ) %>%
      group_by(j5, cult, obs, year) %>%
      summarise(qte = sum(qte, na.rm = TRUE), .groups = "drop") %>%
      left_join(prix_cu, by = c("cult", "obs", "year")) %>%
      mutate(prodcu_val = qte * prix_cu) %>%
      group_by(j5, year) %>%
      summarise(prodcu_val = sum(prodcu_val, na.rm = TRUE), .groups = "drop")
    
  } else if (year == 1996) {
    df <- read_dta(paste0(path, year, "/res_c19.dta"))
    
    prix_cu <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = c20,
        prix_cu = if_else(c22 > 0, c24 / c22, NA_real_)
      ) %>%
      group_by(cult, obs, year) %>%
      summarise(prix_cu = mean(prix_cu, na.rm = TRUE), .groups = "drop")
    
    prodcu_val <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = c20,
        qte = c21
      ) %>%
      group_by(j5, cult, obs, year) %>%
      summarise(qte = sum(qte, na.rm = TRUE), .groups = "drop") %>%
      left_join(prix_cu, by = c("cult", "obs", "year")) %>%
      mutate(prodcu_val = qte * prix_cu) %>%
      group_by(j5, year) %>%
      summarise(prodcu_val = sum(prodcu_val, na.rm = TRUE), .groups = "drop")
    
  } else {
    df <- read_dta(paste0(path, year, "/res_c.dta"))
    
    prix_cu <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = if ("c37" %in% names(.)) paste0(c1, c37) else c1,
        c6b = if (!"c6b" %in% names(.)) c4 * c6a else c6b
      ) %>%
      group_by(cult, obs, year) %>%
      summarise(c4 = sum(c4, na.rm = TRUE),
                c6b = sum(c6b, na.rm = TRUE), .groups = "drop") %>%
      mutate(prix_cu = if_else(c4 > 0, c6b / c4, NA_real_)) %>%
      select(cult, obs, year, prix_cu)
    
    prodcu_val <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        cult = if ("c37" %in% names(.)) paste0(c1, c37) else c1,
        qte = c2
      ) %>%
      group_by(j5, cult, obs, year) %>%
      summarise(qte = sum(qte, na.rm = TRUE), .groups = "drop") %>%
      left_join(prix_cu, by = c("cult", "obs", "year")) %>%
      mutate(prodcu_val = qte * prix_cu) %>%
      group_by(j5, year) %>%
      summarise(prodcu_val = sum(prodcu_val, na.rm = TRUE), .groups = "drop")
  }
  
  return(prodcu_val)
}

## coutmocu ---------------------------------------------------

process_coutmocu <- function(path = "data/ROS_MDG_microdata/", year) {
  file <- paste0(path, year, "/res_mo3.dta")
  if (!file.exists(file)) stop("Fichier manquant : ", file)
  
  df <- haven::read_dta(file) %>% mutate(year = year)
  
  if (year == 1995) {
    required <- c("j5", "mo31f", "wg", "moc")
    stopifnot(all(required %in% names(df)))
    
    df %>%
      filter(moc != 1) %>%  # exclut le riz
      mutate(across(c(mo31f, wg), ~replace_na(.x, 0))) %>%
      mutate(
        salarie = mo31f,
        entraide = wg,
        coutmocu = salarie + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmocu = sum(coutmocu, na.rm = TRUE), .groups = "drop")
    
  } else if (year == 1996) {
    required <- c("j5", "w4b", "w4c", "w4d", "w4e", "w5b", "w5c", "w5d", 
                  "w5e", "w6a", "w6b")
    stopifnot(all(required %in% names(df)))
    
    df %>%
      mutate(across(required[-1], ~replace_na(.x, 0))) %>%
      mutate(
        salarie = w4b * w4c * w4d + w4b * w4c * w4e,
        tache   = w5b * w5c * w5d + w5b * w5c * w5e,
        entraide = w6a + w6b,
        coutmocu = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmocu = sum(coutmocu, na.rm = TRUE), .groups = "drop")
    
  } else if (year == 1997) {
    required_base <- c("j5", "mo31c", "mo31e", "mo31f", "mo31d", "mo31g", "mo31h", "mo44", "mo45")
    optional <- c("mo32a", "mo32b")
    stopifnot(all(required_base %in% names(df)))
    
    df <- df %>%
      mutate(across(all_of(required_base[-1]), ~replace_na(as.numeric(.x), 0)))
    
    # Crée les variables mo32a et mo32b à 0 si elles sont absentes
    for (v in optional) {
      if (!v %in% names(df)) df[[v]] <- 0
    }
    
    df %>%
      mutate(
        salarie = mo31c * mo31e + mo31c * mo31f + mo31d * mo31g + mo31d * mo31h,
        tache   = mo32a + mo32b,
        entraide = mo44 + mo45,
        coutmocu = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmocu = sum(coutmocu, na.rm = TRUE), .groups = "drop")
    
  } else if (year >= 1998 && year <= 2008) {
    required <- c("j5", "mo31c", "mo31e", "mo32", "mo44")
    stopifnot(all(required %in% names(df)))
    
    df %>%
      mutate(across(required[-1], ~replace_na(as.numeric(.x), 0))) %>%
      mutate(
        salarie = mo31c * mo31e,
        tache = mo32,
        entraide = mo44,
        coutmocu = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmocu = sum(coutmocu, na.rm = TRUE), .groups = "drop")
    
  } else {
    required <- c("j5", "mo31c", "mo31e", "mo61a", "mo32", "mo61c", "mo44")
    stopifnot(all(required %in% names(df)))
    
    df %>%
      mutate(across(required[-1], ~replace_na(.x, 0))) %>%
      mutate(
        salarie = mo31c * mo31e + mo31c * mo61a,
        tache = mo32 + mo61c,
        entraide = mo44,
        coutmocu = salarie + tache + entraide
      ) %>%
      group_by(j5, year) %>%
      summarise(coutmocu = sum(coutmocu, na.rm = TRUE), .groups = "drop")
  }
}


## coutintcu -------------------------------------------------

process_coutintcu <- function(path = "data/ROS_MDG_microdata/", year) {
  if (year == 1995) return(NULL)  # pas de données en 1995
  
  file <- paste0(path, year, "/res_itb.dta")
  if (!file.exists(file)) stop("Fichier manquant : ", file)
  
  df <- haven::read_dta(file) %>%
    mutate(year = year)
  
  if (year == 2004 && "itb2" %in% names(df)) {
    df <- df %>% rename(itb5 = itb2)
  }
  
  if (!"itb5" %in% names(df)) stop("Variable 'itb5' absente pour l'année ", year)
  
  df %>%
    rename(coutintcu = itb5) %>%
    mutate(coutintcu = replace_na(as.numeric(coutintcu), 0)) %>%
    group_by(j5, year) %>%
    summarise(coutintcu = sum(coutintcu, na.rm = TRUE), .groups = "drop")
}


## coutloccu -------------------------------------------------

process_coutloccu <- function(path = "data/ROS_MDG_microdata/", year) {
  if (year == 1995) {
    return(NULL)
  }
  
  if (year == 1996) {
    df <- read_dta(paste0(path, year, "/res_c1.dta"))
    
    df <- df %>%
      mutate(
        obs = substr(j5, 1, 2),
        # c4 == 2 : argent, on récupère c7
        coutloccu = if_else(c4 == 2, c7, 0)
      ) %>%
      group_by(j5, year) %>%
      summarise(coutloccu = sum(coutloccu, na.rm = TRUE), .groups = "drop")
    
    return(df)
  }
  
  # Années > 1996 : version standard avec c3a (nature) et c3b (argent)
  df <- read_dta(paste0(path, year, "/res_c.dta")) %>%
    mutate(
      obs = substr(j5, 1, 2),
      cult = if ("c37" %in% names(.)) paste0(c1, c37) else c1
    )
  
  if (!"c6b" %in% names(df) && all(c("c4", "c6a") %in% names(df))) {
    df <- df %>% mutate(c6b = c4 * c6a)
  }
  
  prix_cu <- df %>%
    group_by(cult, obs, year) %>%
    summarise(
      c4 = sum(c4, na.rm = TRUE),
      c6b = sum(c6b, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(prix_cu = if_else(c4 > 0, c6b / c4, NA_real_)) %>%
    select(cult, obs, year, prix_cu)
  
  # Nature
  coutloccu1 <- df %>%
    group_by(j5, cult, obs, year) %>%
    summarise(cumetloc = sum(c3a, na.rm = TRUE), .groups = "drop") %>%
    left_join(prix_cu, by = c("cult", "obs", "year")) %>%
    mutate(coutloccu1 = cumetloc * prix_cu) %>%
    group_by(j5, year) %>%
    summarise(coutloccu1 = sum(coutloccu1, na.rm = TRUE), .groups = "drop")
  
  # Argent
  coutloccu2 <- df %>%
    group_by(j5, year) %>%
    summarise(coutloccu2 = sum(c3b, na.rm = TRUE), .groups = "drop")
  
  # Total
  full <- full_join(coutloccu1, coutloccu2, by = c("j5", "year")) %>%
    mutate(coutloccu = replace_na(coutloccu1, 0) + replace_na(coutloccu2, 0)) %>%
    select(j5, year, coutloccu)
  
  return(full)
}


process_rev_cu <- function(path = "data/ROS_MDG_microdata/", year) {
  
  prod <- process_prodcu_val(path, year)
  coutmo <- process_coutmocu(path, year)
  
  if (year == 1995) {
    # Pas de coutint ni coutloc en 1995
    full <- prod %>%
      full_join(coutmo, by = c("j5", "year")) %>%
      mutate(across(c(prodcu_val, coutmocu), ~replace_na(., 0))) %>%
      mutate(rev_cu = prodcu_val - coutmocu) %>%
      select(j5, year, rev_cu)
  } else {
    coutint <- process_coutintcu(path, year)
    coutloc <- process_coutloccu(path, year)
    
    # Fonction pour créer un tibble vide avec colonnes données si NULL
    ensure_tbl <- function(df, cols) {
      if (is.null(df)) {
        df <- tibble(!!!rlang::set_names(rep(list(NA), length(cols)), 
                                         names(cols)))
        df[] <- Map(function(x, t) vector(typeof(t), 0), df, cols)
      }
      df
    }
    
    # Colonnes attendues pour chaque composante
    coutint <- ensure_tbl(coutint, c(j5 = "", year = 0L, coutintcu = 0))
    coutloc <- ensure_tbl(coutloc, c(j5 = "", year = 0L, coutloccu = 0))
    
    full <- prod %>%
      full_join(coutmo, by = c("j5", "year")) %>%
      full_join(coutint, by = c("j5", "year")) %>%
      full_join(coutloc, by = c("j5", "year")) %>%
      mutate(across(c(prodcu_val, coutmocu, coutintcu, coutloccu), 
                    ~replace_na(., 0))) %>%
      mutate(rev_cu = prodcu_val - coutmocu - coutintcu - coutloccu) %>%
      select(j5, year, rev_cu)
  }
  
  return(full)
}


# revel -------------------------------------------------------------------
process_revel <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement revenu élevage - année ", year)
  
  # Vérifier que le dossier et les fichiers existent
  ele_path <- file.path(path, year, "res_ele.dta")
  pe_path  <- file.path(path, year, "res_pe.dta")
  cie_path <- file.path(path, year, "res_cie.dta")
  
  if (!file.exists(ele_path) || !file.exists(pe_path) || !file.exists(cie_path)) {
    warning("Fichiers manquants pour l'année ", year, ". Retourne NA.")
    return(tibble(j5 = character(), year = year, revel = numeric()))
  }
  
  # Chargement des données élevage
  ele <- read_dta(paste0(path, year, "/res_ele.dta")) %>%
    mutate(year = year)
  
  # Table de correspondance pour identifier 'obs'
  obs_lookup <- ele %>%
    distinct(j5) %>%
    mutate(obs = substr(j5, 1, 2))
  
  # Ventes d’animaux : bovins (lig 1 à 3) vs autres
  vente_ani <- ele %>%
    mutate(
      vente_bovin = if_else(ele_lig %in% 1:3, ele_i2, 0),
      vente_otrani = if_else(!ele_lig %in% 1:3, ele_i2, 0)
    ) %>%
    group_by(j5, year) %>%
    summarise(across(c(vente_bovin, vente_otrani), ~sum(.x, na.rm = TRUE)), 
              .groups = "drop")
  
  # Conso zébu pour exclusion éventuelle (uniquement utile pour la suite)
  conso_zebu <- ele %>%
    filter(ele_lig %in% 1:3, !is.na(ele_e), ele_e != 0) %>%
    group_by(j5, year) %>%
    summarise(conso_zebu = 1, .groups = "drop")
  
  # Chargement des produits animaux
  pe <- read_dta(paste0(path, year, "/res_pe.dta")) %>%
    mutate(year = year)
  
  # Ajout sécurisé de la variable pe_c si elle existe
  if ("pe_c" %in% names(pe)) {
    pe <- pe %>%
      mutate(
        pe_c = as.numeric(pe_c),
        pe_c = replace_na(pe_c, 0)
      )
  } else {
    # Si pe_c n’existe pas (ex. 1996), on met pe_c = 1 par défaut
    pe <- pe %>%
      mutate(pe_c = 1)
  }
  if (year == 1995) {
    # Rien n’est comptabilisé en 1995
    vente_prodel <- tibble(j5 = character(),
                           year = integer(), 
                           vente_beef = numeric(), 
                           vente_nobeef = numeric())
    
  } else if (year == 1996) {
    # Pour 1996 : 5 mois pour produits < 4, 1 mois pour viande (>=4)
    pe <- pe %>%
      mutate(
        mois_vente = if_else(pe_lig < 4, 5, 1),
        valeur = pe_e * mois_vente
      )
    
    modalites <- sort(unique(pe$pe_lig[pe$pe_lig < 4]))
    
    vente_lait <- if (length(modalites) >= 1) if_else(pe$pe_lig == modalites[1],
                                                      pe$valeur, 0) else 0
    vente_oeuf <- if (length(modalites) >= 2) if_else(pe$pe_lig == modalites[2], 
                                                      pe$valeur, 0) else 0
    vente_beef <- if (length(modalites) >= 3) if_else(pe$pe_lig == modalites[3], 
                                                      pe$valeur, 0) else 0
    vente_meat <- if_else(pe$pe_lig >= 4 | 
                            pe$pe_lig > max(modalites, na.rm = TRUE), 
                          pe$valeur, 0)
    
    pe <- pe %>%
      mutate(
        vente_lait = vente_lait,
        vente_oeuf = vente_oeuf,
        vente_beef = vente_beef,
        vente_meat = vente_meat
      )
    
  } else if (year < 2011) {
    # Avant 2011 : toutes les viandes sont incluses (pas de distinction)
    pe <- pe %>%
      mutate(
        valeur = pe_e * pe_c
      )
    
    modalites <- sort(unique(pe$pe_lig[pe$pe_lig < 4]))
    
    vente_lait <- if (length(modalites) >= 1) if_else(pe$pe_lig == modalites[1], 
                                                      pe$valeur, 0) else 0
    vente_oeuf <- if (length(modalites) >= 2) if_else(pe$pe_lig == modalites[2], 
                                                      pe$valeur, 0) else 0
    vente_beef <- if (length(modalites) >= 3) if_else(pe$pe_lig == modalites[3], 
                                                      pe$valeur, 0) else 0
    vente_meat <- if_else(pe$pe_lig >= 4 | 
                            pe$pe_lig > max(modalites, na.rm = TRUE), 
                          pe$valeur, 0)
    
    pe <- pe %>%
      mutate(
        vente_lait = vente_lait,
        vente_oeuf = vente_oeuf,
        vente_beef = vente_beef,
        vente_meat = vente_meat
      )
    
  } else {
    # À partir de 2011 : distinction explicite entre boeuf et autres viandes
    pe <- pe %>%
      left_join(conso_zebu, by = c("j5", "year")) %>%
      mutate(
        valeur = pe_e * pe_c,
        vente_lait = if_else(pe_lig == 1, valeur, 0),
        vente_oeuf = if_else(pe_lig == 2, valeur, 0),
        vente_beef = if_else(pe_lig == 3, valeur, 0),
        vente_meat = if_else(pe_lig %in% c(4, 6, 7), valeur, 0)
      )
  }
  
  # Agrégation ventes produits animaux
  vente_prodel <- pe %>%
    group_by(j5, year) %>%
    summarise(
      vente_beef = sum(vente_beef, na.rm = TRUE),
      vente_nobeef = sum(vente_lait + vente_oeuf + vente_meat, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Valorisation autoconsommation (lig 6 à 9)
  conso_ani <- ele %>%
    group_by(j5, ele_lig, year) %>%
    summarise(conso_ani = sum(ele_e, na.rm = TRUE), .groups = "drop") %>%
    mutate(obs = substr(j5, 1, 2))
  
  prix_ani <- ele %>%
    mutate(obs = substr(j5, 1, 2)) %>%
    group_by(obs, ele_lig, year) %>%
    summarise(
      vte_ani = sum(ele_i1, na.rm = TRUE),
      val_vte = sum(ele_i2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(prix_ani = val_vte / vte_ani) %>%
    select(obs, year, ele_lig, prix_ani)
  
  conso_ani_val <- conso_ani %>%
    left_join(prix_ani, by = c("obs", "year", "ele_lig")) %>%
    mutate(conso_ani_val = conso_ani * prix_ani) %>%
    filter(ele_lig %in% 6:9) %>%
    group_by(j5, year) %>%
    summarise(conso_ani_val = sum(conso_ani_val, na.rm = TRUE), .groups = "drop")
  
  # Charges élevage : consommation intermédiaire
  cie <- read_dta(paste0(path, year, "/res_cie.dta")) %>%
    rename(coutintel = cie2) %>%
    group_by(j5, year) %>%
    summarise(coutintel = sum(coutintel, na.rm = TRUE), .groups = "drop")
  
  # Charges élevage : achat d’animaux
  achat_ani <- ele %>%
    mutate(
      achanimoval = if_else(!ele_lig %in% 1:3, ele_l2, 0),
      achzebuval = if_else(ele_lig %in% 1:3, ele_l2, 0)
    ) %>%
    group_by(j5, year) %>%
    summarise(
      achanimoval = sum(achanimoval, na.rm = TRUE),
      achzebuval = sum(achzebuval, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Fusion finale
  df <- reduce(list(
    vente_ani, vente_prodel, conso_ani_val, cie, achat_ani
  ), full_join, by = c("j5", "year")) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(
      recette_el = vente_otrani + vente_nobeef + conso_ani_val,
      charge_el = coutintel + achanimoval,
      revel = recette_el - charge_el
    ) %>%
    select(j5, year, revel)
  
  return(df)
}


# revpeche ----------------------------------------------------------------

process_revpeche <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement revenu pêche - année ", year)
  
  # Le module pêche n'existe que depuis 2015
  if (year < 2015) {
    warning("Pas de module pêche avant 2015. Retourne NA.")
    return(tibble(j5 = character(), year = year, revpeche = numeric()))
  }
  
  # Vérification des fichiers
  ppec_path <- file.path(path, year, "res_ppec.dta")
  cipec_path <- file.path(path, year, "res_cipec.dta")
  mopec_path <- file.path(path, year, "res_mopec.dta")
  
  if (!file.exists(ppec_path) || !file.exists(cipec_path) || !file.exists(mopec_path)) {
    warning("Fichiers manquants pour l'année ", year, ". Retourne NA.")
    return(tibble(j5 = character(), year = year, revpeche = numeric()))
  }
  
  # Chargement des données de pêche
  ppec <- read_dta(ppec_path) %>%
    mutate(year = year)
  
  # Prix moyen pondéré par produit
  prix_peche <- ppec %>%
    group_by(ppec_lig, year) %>%
    summarise(
      ppec_i = sum(ppec_i, na.rm = TRUE),
      ppec_l = sum(ppec_l, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(prix_peche = ppec_l / ppec_i) %>%
    select(ppec_lig, year, prix_peche)
  
  # Recette de pêche = production * prix moyen
  recette_peche <- ppec %>%
    left_join(prix_peche, by = c("ppec_lig", "year")) %>%
    mutate(recette_peche = ppec_f * prix_peche) %>%
    group_by(j5, year) %>%
    summarise(recette_peche = sum(recette_peche, na.rm = TRUE), .groups = "drop")
  
  # Consommations intermédiaires (cipec)
  depeche1 <- read_dta(cipec_path) %>%
    rename(depeche1 = cipec2) %>%
    group_by(j5, year) %>%
    summarise(depeche1 = sum(depeche1, na.rm = TRUE), .groups = "drop")
  
  # Main d’œuvre (mopec)
  depeche2 <- read_dta(mopec_path) %>%
    group_by(j5, year) %>%
    summarise(
      mopec_4 = sum(mopec_4, na.rm = TRUE),
      mopec_5 = sum(mopec_5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(depeche2 = mopec_4 + mopec_5) %>%
    select(j5, year, depeche2)
  
  # Fusion finale et calcul revenu pêche
  df <- reduce(
    list(recette_peche, depeche1, depeche2),
    full_join,
    by = c("j5", "year")
  ) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(revpeche = recette_peche - (depeche1 + depeche2)) %>%
    select(j5, year, revpeche)
  
  return(df)
}


# rente_riz ---------------------------------------------------------------

process_rente_riz <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement rente riz - année ", year)
  
  # Cas particulier : 1995 → retourner NA
  if (year == 1995) {
    return(tibble(j5 = NA_character_, year = 1995, rente_riz = NA_real_))
  }
  
  
  # Lecture des données de riziculture (parcelles) et correspondance ménage-observatoire
  df_r <- read_dta(paste0(path, year, "/res_r.dta"))
  df_deb <- read_dta(paste0(path, year, "/res_deb.dta")) %>%
    transmute(j5, obs = j0)
  df_deb <- df_deb %>%
    distinct(j5, .keep_all = TRUE)
  
  # Lecture des prix du paddy au niveau de l'observatoire
  df_dc21 <- read_dta(paste0(path, year, "/res_dc21.dta")) %>%
    left_join(df_deb, by = "j5") %>%
    group_by(obs, year) %>%
    summarise(
      dc22 = sum(dc22, na.rm = TRUE),
      dc25 = sum(dc25, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pxpaddy_obs = if_else(dc22 > 0, dc25 / dc22, NA_real_)
    )
  
  # Recettes en nature (r6) pour les parcelles mises en métayage (r4 == 5 ou 6)
  rente_riz1 <- df_r %>%
    filter(r4 %in% c(5, 6)) %>%
    mutate(recmetloc = coalesce(r6, 0)) %>%
    group_by(j5, year) %>%
    summarise(
      recmetloc = sum(recmetloc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(df_deb, by = "j5") %>%
    left_join(df_dc21, by = c("obs", "year")) %>%
    mutate(
      rente_riz1 = recmetloc * pxpaddy_obs
    ) %>%
    select(j5, year, rente_riz1)
  
  # Recettes monétaires (r7) sur les mêmes parcelles (r4 == 5 ou 6)
  rente_riz2 <- df_r %>%
    filter(r4 %in% c(5, 6)) %>%
    mutate(rente_riz2 = coalesce(r7, 0)) %>%
    group_by(j5, year) %>%
    summarise(
      rente_riz2 = sum(rente_riz2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Fusion des deux sources de revenu (nature + monétaire)
  rente_riz <- full_join(rente_riz1, rente_riz2, by = c("j5", "year")) %>%
    mutate(
      rente_riz1 = replace_na(rente_riz1, 0),
      rente_riz2 = replace_na(rente_riz2, 0),
      rente_riz = rente_riz1 + rente_riz2
    ) %>%
    select(j5, year, rente_riz)
  
  # En 1997, les prix sont encore en milliers
  if (year == 1997) {
    rente_riz <- rente_riz %>%
      mutate(rente_riz = rente_riz * 1000)
  }
  
  return(rente_riz)
}

# rente_cu ----------------------------------------------------------------
process_rente_cu <- function(year, path = "data/ROS_MDG_microdata/") {
  message("Traitement rente cultures autres - année ", year)
  
  # Cas particulier : pas de calcul possible en 1995
  if (year == 1995) {
    return(tibble(j5 = NA_character_, year = 1995, rente_cu = NA_real_))
  }
  
  file_c <- paste0(path, year, "/res_c.dta")
  file_fr21 <- paste0(path, year, "/res_fr21.dta")
  
  rente_cu1 <- NULL
  rente_cu2 <- NULL
  
# Rente monétaire : fr21g sur fr21c == 7
  if (file.exists(file_fr21)) {
    df_fr21 <- read_dta(file_fr21) %>% mutate(year = year)
    
    rente_cu1 <- df_fr21 %>%
      filter(fr21c == 7, fr21a != 1) %>%
      mutate(rente_cu1 = coalesce(fr21g, 0)) %>%
      group_by(j5, year) %>%
      summarise(rente_cu1 = sum(rente_cu1, na.rm = TRUE), .groups = "drop")
  }
  
  # Rente en nature : recmetloccu × prix_cu
  if (file.exists(file_c)) {
    df_c <- read_dta(file_c) %>%
      mutate(
        obs  = substr(j5, 1, 2),
        cult = if ("c37" %in% names(.)) paste0(c1, c37) else as.character(c1),
        c6b  = if ("c6b" %in% names(.)) c6b else c4 * c6a,
        year = year
      )
    
    if ("c21" %in% names(df_c)) {
      prix_cu <- df_c %>%
        group_by(cult, obs, year) %>%
        summarise(
          c4 = sum(c4, na.rm = TRUE),
          c6b = sum(c6b, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          prix_cu = if_else(c4 > 0, c6b / c4, NA_real_)
        ) %>%
        select(cult, obs, year, prix_cu)
      
      rente_cu2 <- df_c %>%
        group_by(j5, cult, obs, year) %>%
        summarise(recmetloccu = sum(c21, na.rm = TRUE), .groups = "drop") %>%
        left_join(prix_cu, by = c("cult", "obs", "year")) %>%
        mutate(rente_cu2 = recmetloccu * prix_cu) %>%
        group_by(j5, year) %>%
        summarise(rente_cu2 = sum(rente_cu2, na.rm = TRUE), .groups = "drop")
    }
  }
  
  # Fusion des deux composantes
  if (is.null(rente_cu1) && is.null(rente_cu2)) {
    warning("Aucune composante disponible pour l'année ", year)
    return(tibble(j5 = NA_character_, year = year, rente_cu = NA_real_))
  }
  
  # Init composants manquants comme tibbles vides
  if (is.null(rente_cu1)) {
    rente_cu1 <- tibble(j5 = character(), year = integer(), rente_cu1 = numeric())
  }
  if (is.null(rente_cu2)) {
    rente_cu2 <- tibble(j5 = character(), year = integer(), rente_cu2 = numeric())
  }
  
  # Fusionner et sommer
  rente_cu <- full_join(rente_cu1, rente_cu2, by = c("j5", "year")) %>%
    mutate(
      rente_cu1 = replace_na(rente_cu1, 0),
      rente_cu2 = replace_na(rente_cu2, 0),
      rente_cu = rente_cu1 + rente_cu2
    ) %>%
    select(j5, year, rente_cu)
  
  # les prix sont en milliers en 2003 et 2004
  if (year %in% c(2003, 2004)) {
    rente_cu <- rente_cu %>%
      mutate(rente_cu = rente_cu * 1000)
  }

  
  return(rente_cu)
}

# autre_rev  --------------------------------------------------------------

process_autre_rev <- function(year, path = "data/ROS_MDG_microdata/") {
  file <- file.path(path, year, "res_rha.dta")
  if (!file.exists(file)) return(tibble(j5 = character(), year = year, autre_rev = numeric()))
  haven::read_dta(file) %>%
    mutate(
      year = if (!"year" %in% names(.)) year else year,
      autre_rev = dplyr::coalesce(as.numeric(.data$rha2), 0)  # recode missing -> 0
    ) %>%
    group_by(j5, year) %>%
    summarise(autre_rev = sum(autre_rev, na.rm = TRUE), .groups = "drop")
}

# himo --------------------------------------------------------------------

process_himo <- function(year, path = "data/ROS_MDG_microdata/") {
  f <- file.path(path, year, "res_m_a.dta")
  if (!file.exists(f)) {
    # module absent (e.g., 1995): return empty; treat as 0 after merging
    return(tibble(j5 = character(), year = integer(), himo = numeric()))
  }
  
  df <- read_dta(f)
  if (!"year" %in% names(df)) df$year <- year
  if (!"a4b" %in% names(df)) df$a4b <- NA_real_
  if (!"a4d" %in% names(df)) df$a4d <- NA_real_
  
  df %>%
    transmute(j5, year,
              act = coalesce(as.numeric(a4b), 0) * 1000,
              vct = coalesce(as.numeric(a4d), 0) * 1000,
              himo = act + vct) %>%
    group_by(j5, year) %>%
    summarise(himo = sum(himo, na.rm = TRUE), .groups = "drop")
}

# decap -----------------------------------------------------------
# gen decap = vte_par + vente_bovin + vente_beef + vte_equip

# Vente de parcelles: fr21g if fr21c in {1,20}, then sum by j5,year
process_vte_par <- function(year, path = "data/ROS_MDG_microdata/") {
  # 2003: res_fr.dta, montant total des parcelles vendues
  if (year == 2003) {
    f <- file.path(path, year, "res_fr.dta")
    if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), vte_par = numeric()))
    df <- read_dta(f)
    if (!"year" %in% names(df)) df$year <- year
    # fr21g01: "montant total des parcelles vendues (en ar)"
    if (!"fr21g01" %in% names(df)) {
      warning("fr21g01 absent in 2003 file; returning empty vte_par.")
      return(tibble(j5 = character(), year = integer(), vte_par = numeric()))
    }
    return(
      df %>%
        transmute(j5, year, vte_par = coalesce(as.numeric(fr21g01), 0)) %>%
        group_by(j5, year) %>%
        summarise(vte_par = sum(vte_par, na.rm = TRUE), .groups = "drop")
    )
  }
  
  # 2004: res_fr.dta, rizières (fr32) + parcelles (fr48)
  if (year == 2004) {
    f <- file.path(path, year, "res_fr.dta")
    if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), vte_par = numeric()))
    df <- read_dta(f)
    if (!"year" %in% names(df)) df$year <- year
    # fr32: somme totale reçue par VENTE de rizières
    # fr48: somme totale obtenue par VENTE de parcelles
    if (!("fr32" %in% names(df) | "fr48" %in% names(df))) {
      warning("Neither fr32 nor fr48 present in 2004 file; returning empty vte_par.")
      return(tibble(j5 = character(), year = integer(), vte_par = numeric()))
    }
    
    df %>%
      transmute(
        j5, year,
        vte_par = coalesce(as.numeric(.data[["fr32"]]), 0) +
          coalesce(as.numeric(.data[["fr48"]]), 0)
      ) %>%
      group_by(j5, year) %>%
      summarise(vte_par = sum(vte_par, na.rm = TRUE), .groups = "drop") %>%
      return()
  }
  
  # 2005+: res_fr21.dta
  f21 <- file.path(path, year, "res_fr21.dta")
  if (file.exists(f21)) {
    df <- read_dta(f21)
    if (!"year" %in% names(df)) df$year <- year
    if (!("fr21c" %in% names(df) && "fr21g" %in% names(df))) {
      warning("fr21c/fr21g not found in res_fr21.dta for year ", year, "; returning empty vte_par.")
      return(tibble(j5 = character(), year = integer(), vte_par = numeric()))
    }
    return(
      df %>%
        mutate(
          fr21c_num = as.numeric(fr21c),
          vte_par = if_else(fr21c_num %in% c(1, 20), as.numeric(fr21g), 0)
        ) %>%
        group_by(j5, year) %>%
        summarise(vte_par = sum(vte_par, na.rm = TRUE), .groups = "drop")
    )
  }
  
  # Fallback if no file or unknown structure
  tibble(j5 = character(), year = integer(), vte_par = numeric())
}

# NB : in 1996, all animal sales are registered as non-bovine as they 
# Because ele_i1 is coded 0 everywhere. Not a problem for rev_tot as non-bovine
# animal sales are accounted for in rev_elev

# Vente d’animaux bovins (cheptel): ele_i2 if ele_lig in 1:3
process_vente_bovin <- function(year, path = "data/ROS_MDG_microdata/") {
  f <- file.path(path, year, "res_ele.dta")
  if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), vente_bovin = numeric()))
  read_dta(f) %>%
    mutate(year = if (!"year" %in% names(.)) year else year) %>%
    mutate(vente_bovin = if_else(ele_lig %in% 1:3, as.numeric(ele_i2), 0)) %>%
    group_by(j5, year) %>%
    summarise(vente_bovin = sum(vente_bovin, na.rm = TRUE), .groups = "drop")
}


# Vente de produits d’élevage "beef": pe_e * pe_c if pe_lig == 3 (2011+)
# For <2011 the dofile doesn’t separate beef in exceptionnels; set to 0 to avoid double-counting with revel.
process_vente_beef <- function(year, path = "data/ROS_MDG_microdata/") {
  if (year < 2011) return(tibble(j5 = character(), year = integer(), vente_beef = numeric()))
  f <- file.path(path, year, "res_pe.dta")
  if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), vente_beef = numeric()))
  pe <- read_dta(f) %>% mutate(year = if (!"year" %in% names(.)) year else year)
  if (!"pe_c" %in% names(pe)) pe <- pe %>% mutate(pe_c = 1)  # safety, rare after 2011
  pe %>%
    mutate(valeur = as.numeric(pe_e) * as.numeric(pe_c),
           vente_beef = if_else(pe_lig == 3, valeur, 0)) %>%
    group_by(j5, year) %>%
    summarise(vente_beef = sum(vente_beef, na.rm = TRUE), .groups = "drop")
}

# Vente d’équipements (et biens): vb2b is equip; vb2a,c,d,e,f,g are "biens"
process_vte_equip <- function(year, path = "data/ROS_MDG_microdata/") {
  f <- file.path(path, year, "res_vb.dta")
  if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), 
                                     vte_equip = numeric(), vte_biens = numeric()))
  read_dta(f) %>%
    mutate(year = if (!"year" %in% names(.)) year else year) %>%
    mutate(across(c(vb2a, vb2b, vb2c, vb2d, vb2e, vb2f, vb2g),
                  ~coalesce(as.numeric(.), 0))) %>%
    transmute(j5, year,
              vte_biens = vb2a + vb2c + vb2d + vb2e + vb2f + vb2g,
              vte_equip = vb2b) %>%
    group_by(j5, year) %>%
    summarise(across(c(vte_biens, vte_equip), ~sum(.x, na.rm = TRUE)), 
              .groups = "drop")
}

# Only since 2010
process_vte_equip <- function(year, path = "data/ROS_MDG_microdata/") {
  f <- file.path(path, year, "res_vb.dta")
  if (!file.exists(f)) return(tibble(j5 = character(), year = integer(), 
                                     vte_equip = numeric(), vte_biens = numeric()))
  read_dta(f) %>%
    mutate(year = if (!"year" %in% names(.)) year else year) %>%
    mutate(across(c(vb2a, vb2b, vb2c, vb2d, vb2e, vb2f, vb2g),
                  ~coalesce(as.numeric(.), 0))) %>%
    transmute(j5, year,
              vte_biens = vb2a + vb2c + vb2d + vb2e + vb2f + vb2g,
              vte_equip = vb2b) %>%
    group_by(j5, year) %>%
    summarise(across(c(vte_biens, vte_equip), ~sum(.x, na.rm = TRUE)), 
              .groups = "drop")
}

process_decap <- function(year, path = "data/ROS_MDG_microdata/") {
  vp   <- process_vte_par(year, path)
  vbov <- process_vente_bovin(year, path)
  vbee <- process_vente_beef(year, path)
  veq  <- process_vte_equip(year, path)
  
  # join & sum
  reduce(list(vp, vbov, vbee, veq), full_join, by = c("j5","year")) %>%
    mutate(across(c(vte_par, vente_bovin, vente_beef, vte_equip), 
                  ~coalesce(.x, 0))) %>%
    mutate(decap = vte_par + vente_bovin + vente_beef + vte_equip) %>%
    select(j5, year, decap)
}

# transrecmo + transrecnomo

process_transrec <- function(year, path = "data/ROS_MDG_microdata/") {
  f <- file.path(path, year, "res_t1.dta")
  if (!file.exists(f)) {
    return(tibble(j5 = character(), year = integer(),
                  transrecmo = numeric(), transrecnomo = numeric()))
  }
  df <- read_dta(f)
  if (!"year" %in% names(df)) df$year <- year
  
  # Pick columns robustly (handles labelled)
  pick_col <- function(nms, candidates) {
    cand <- intersect(candidates, nms)
    if (length(cand)) cand[1] else NA_character_
  }
  a_col <- pick_col(names(df), c("t11a","t1a"))
  d_col <- pick_col(names(df), c("t11d","t1d"))
  if (is.na(a_col) || is.na(d_col)) {
    warning("Missing t11a/t11d in ", f, "; returning empty.")
    return(tibble(j5 = character(), year = integer(),
                  transrecmo = numeric(), transrecnomo = numeric()))
  }
  
  df %>%
    transmute(
      j5, year,
      t11a = as.numeric(.data[[a_col]]),
      amt  = as.numeric(.data[[d_col]])
    ) %>%
    mutate(
      amt = coalesce(amt, 0),
      # Stata: t11a==99 -> monetary; t11a!=99 (including missing) -> non-monetary
      transrecmo   = if_else(t11a == 99, amt, 0),
      transrecnomo = if_else(is.na(t11a) | t11a != 99, amt, 0)
    ) %>%
    group_by(j5, year) %>%
    summarise(
      transrecmo   = sum(transrecmo, na.rm = TRUE),
      transrecnomo = sum(transrecnomo, na.rm = TRUE),
      .groups = "drop"
    )
}


# Wrapper for all incomes -----------------------------------------------------

compute_income_year <- function(year, path = "data/ROS_MDG_microdata/") {
  # core components
  revppal   <- process_revppal(path, year)
  revsec    <- process_revsec(path, year)
  rev_riz   <- process_rev_riz(year, path)
  rev_cu    <- process_rev_cu(path, year)
  revel     <- process_revel(year, path)
  revpeche  <- process_revpeche(year, path) # will be empty pre-2015
  rente_riz <- process_rente_riz(year, path)
  rente_cu  <- process_rente_cu(year, path)
  autre_rev <- process_autre_rev(year, path)
  himo      <- process_himo(year, path)
  decap     <- process_decap(year, path) # using vte_par + vente_bovin + vente_beef + vte_equip
  transrec  <- process_transrec(year, path) # transrecmo + transrecnomo
  
  # vte_biens comes from the same equipment/biens process used by decap
  vte_biens <- process_vte_equip(year, path) %>% 
    select(j5, year, vte_biens = vte_biens)
  
  # merge everything
  df <- reduce(
    list(revppal, revsec, rev_riz, rev_cu, revel, revpeche,
         rente_riz, rente_cu, autre_rev, himo, decap, vte_biens, transrec),
    full_join, by = c("j5", "year")
  ) %>%
    # Stata collapses treat missings as 0 in sums → coalesce to 0
    mutate(across(
      c(revppal, revsec, rev_riz, rev_cu, revel, revpeche,
        rente_riz, rente_cu, autre_rev, himo, decap, vte_biens,
        transrecmo, transrecnomo),
      ~replace_na(., 0)
    )) %>%
    mutate(
      revcou    = revppal + revsec + rev_riz + rev_cu + revel + revpeche,
      revexcept = rente_riz + rente_cu + autre_rev + himo + decap + vte_biens + transrecmo + transrecnomo,
      revtot    = revcou + revexcept
    )
  
  df
}

compute_income_all_years <- function(years = 1995:2015, path = "data/ROS_MDG_microdata/") {
  map_dfr(years, ~compute_income_year(.x, path))
}

