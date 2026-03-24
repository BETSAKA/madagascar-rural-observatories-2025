# utils/translations.R
# ─────────────────────────────────────────────────────────────────
# FR→MG translation infrastructure for bilingual report outputs.
#
# Provides:
#   FR_TO_MG          Named character vector: French → Malagasy lookup
#   translate_mg()    Translate a character vector using FR_TO_MG
#   translate_data_mg()  Translate columns of a data frame
#   translate_html_mg()  Translate HTML string (for gt auto-translation)
#   load_var_labels_mg() Load variable-level MG labels from Excel
#
# This file is sourced by helpers_report.R so that translations are
# available to the modified knit_print.gt_tbl method.
# ─────────────────────────────────────────────────────────────────

# ── Hand-curated category label translations ──────────────────
# Keys must match EXACTLY what classify_*() functions produce.
# Sorted by function of origin for maintainability.

FR_TO_MG <- c(
  # ── classify_sex() ──
  "Homme" = "Lahy",
  "Femme" = "Vavy",
  "Hommes" = "Lahy",
  "Femmes" = "Vavy",

  # ── classify_literacy() ──
  "Oui" = "Eny",
  "Avec effort" = "Sarotra",
  # "Non" handled below with longer context matches first

  # ── classify_education() ──
  "Primaire" = "Ambaratonga voalohany",
  "Secondaire 1er cycle" = "Sekondery andiny voalohany",
  "Secondaire 2\u00e8me cycle & sup." = "Sekondery andiny faharoa & ambony",
  "Non scolaris\u00e9" = "Tsy nianatra",
  "Inconnu" = "Tsy fantatra",

  # ── classify_marital() ──
  "C\u00e9libataire" = "Tsy manambady",
  "Mari\u00e9(e) selon la tradition" = "Manambady ara-drazana",
  "En concubinage" = "Mpiray trano",
  "Divorc\u00e9(e)" = "Nisara-bady",
  "Veuf(ve)" = "Mpitondra tena",
  "Mari\u00e9(e) civil / religieux / polygame" = "Manambady ara-dal\u00e0na / ara-pinoana / maro vady",

  # ── classify_activity() ──
  "Cultivateur exploitant" = "Mpamboly",
  "Ouvrier agricole" = "Mpiasa tany",
  "Commerce & Restauration" = "Varotra sy fisakafoanana",
  "Elevage, P\u00eache & Ress. nat." = "Fiompiana, Jono & Harena voajanahary",
  "Artisanat, BTP & Transport" = "Asa tanana, Fanorenana & Fitaterana",
  "El\u00e8ve / Etudiant" = "Mpianatra",
  "Inactif / Retrait\u00e9" = "Tsy miasa / Misotro ronono",
  "Services & Autres" = "Serivisy sy hafa",

  # ── classify_age_group() ──
  "0-10 ans" = "0-10 taona",
  "11-20 ans" = "11-20 taona",
  "21-30 ans" = "21-30 taona",
  "31-40 ans" = "31-40 taona",
  "41-50 ans" = "41-50 taona",
  "51-60 ans" = "51-60 taona",
  "61-70 ans" = "61-70 taona",
  "71 ans et plus" = "71 taona na mihoatra",

  # ── classify_origin() — already Malagasy; keep unchanged ──
  "Tompon-tany" = "Tompon-tany",
  "Zana-tany" = "Zana-tany",
  "Valivotaka" = "Valivotaka",
  "Mpihavy" = "Mpihavy",
  "Mpandalo (Migrant temporaire)" = "Mpandalo",

  # ── classify_reason() ──
  "Sociale (naissance, mariage, famille)" = "Ara-tsosialy (fahaterahana, fanambadiana, fianakaviana)",
  "\u00c9conomique (travail, ressources)" = "Ara-toekarena (asa, loharanon-karena)",
  "Conjoncturelle (\u00e9cologie, ins\u00e9curit\u00e9)" = "Ara-toe-javatra (tontolo iainana, tsy fandriampahalemana)",
  "Autre / NSP" = "Hafa / Tsy fantatra",

  # ── Common terms in column headers / group names ──
  "Chef de m\u00e9nage" = "Lohan\u2019ny tokantrano",
  "Chefs de m\u00e9nage" = "Lohan\u2019ny tokantrano",
  "Conjoint" = "Vady",
  "Conjoints" = "Vady",
  "Taille moyenne" = "Haben\u2019ny tokantrano",
  "Tranche d\u2019\u00e2ge" = "Fizarana taona",
  "Tranche d'\u00e2ge" = "Fizarana taona",
  "Activit\u00e9" = "Asa",
  "Niveau d\u2019\u00e9ducation" = "Haavo-pahaizana",
  "Niveau d'\u00e9ducation" = "Haavo-pahaizana",
  "Niveau" = "Haavo",
  "Sexe" = "Lahy/Vavy",
  "Statut matrimonial" = "Toetry ny fanambadiana",
  "Cat\u00e9gorie" = "Sokajy",
  "Origine" = "Fiaviana",
  "Raison" = "Antony",
  "R\u00e9gion" = "Faritra",
  "Hameau" = "Tan\u00e0na kely",
  "Scolaris\u00e9s (%)" = "Mianatra (%)",
  "Non scolaris\u00e9s (%)" = "Tsy mianatra (%)",
  "Cat\u00e9gorie d\u2019\u00e2ge" = "Sokajin-taona",
  "Cat\u00e9gorie d'\u00e2ge" = "Sokajin-taona",
  "Caract\u00e9ristique" = "Toetra",
  "Statistique" = "Statistika",
  "M\u00e9diane" = "Median",
  "Moyenne" = "Salan\u2019isa",
  "Minimum" = "Kely indrindra",
  "Maximum" = "Lehibe indrindra",
  "Mode" = "Mode",

  # ── Schooling reasons ──
  "Niveau suffisant" = "Efa ampy ny haavo",
  "Main-d'oeuvre exploitation" = "Mila mpiasa ao amin\u2019ny fambolena",
  "Main-d\u2019oeuvre exploitation" = "Mila mpiasa ao amin\u2019ny fambolena",
  "Besoin de revenu" = "Mila vola miditra",
  "Besoin revenu" = "Mila vola miditra",
  "Frais trop \u00e9lev\u00e9s" = "Lafo loatra ny saram-pianarana",
  "Frais scolarit\u00e9 trop \u00e9lev\u00e9s" = "Lafo loatra ny saram-pianarana",
  "Sans int\u00e9r\u00eat" = "Tsy mahaliana",
  "Sans int\u00e9r\u00eat (famille)" = "Tsy mahaliana (fianakaviana)",
  "\u00c9chec scolaire" = "Tsy nahomby tamin\u2019ny fianarana",
  "Grossesse / mariage" = "Bevohoka / manambady",
  "Grossesse/mariage" = "Bevohoka / manambady",
  "Handicap / maladie" = "Fahasembanana / aretina",
  "Handicap/maladie" = "Fahasembanana / aretina",
  "\u00c9cole en mauvais \u00e9tat" = "Ratsy ny sekoly",
  "Instituteur absent" = "Tsy ao ny mpampianatra",
  "\u00c9cole trop \u00e9loign\u00e9e" = "Lavitra loatra ny sekoly",
  "\u00c9cole \u00e9loign\u00e9e" = "Lavitra loatra ny sekoly",
  "Trop jeune" = "Mbola kely loatra",
  "Autre raison" = "Antony hafa",
  "Famine" = "Mosary",
  "Paresse" = "Hakamoana",
  "Autre" = "Hafa",

  # ── Chapter 03: Habitat & living conditions ──
  "Propri\u00e9taire" = "Tompon-trano",
  "Locataire" = "Mpanofa",
  "H\u00e9berg\u00e9" = "Ampiantranon\u2019olona",
  "Tr\u00e8s insuffisante" = "Tena tsy ampy",
  "Insuffisante" = "Tsy ampy",
  "Suffisante" = "Ampy",
  "Satisfaisante" = "Mahafa-po",
  "Tr\u00e8s satisfaisante" = "Tena mahafa-po",
  "Mauvaise" = "Ratsy",

  # ── Chapter 04: Land tenure ──
  "Rizi\u00e8re" = "Tanimbary",
  "Champs" = "Tanimboly",
  "Terrain bois\u00e9" = "Tany misy ala",
  "Culture p\u00e9renne" = "Voly maharitra",
  "Plaine" = "Lemaka",
  "Achat" = "Novidina",
  "Don" = "Fanomezana",
  "Mise en valeur" = "Namboarina",
  "H\u00e9ritage (partage d\u00e9finitif)" = "Lova (fizarana farany)",
  "H\u00e9ritage (partage non d\u00e9finitif)" = "Lova (fizarana tsy farany)",
  "H\u00e9ritage tournant" = "Lova mifandimby",
  "Pr\u00eat" = "Findramana",
  "Location" = "Hofana",
  "M\u00e9tayage" = "Tesaka",
  "Mise en gage" = "Antoka",
  "Direct" = "Mivantana",
  "Pr\u00eat gratuit" = "Findramana maimaim-poana",
  "Aucun document" = "Tsy misy taratasy",

  # ── Chapter 05: Agriculture ──
  "Pas d'engrais" = "Tsy misy zezika",
  "Fumure/compost seul" = "Zezika biby/compost irery",
  "Engrais chimique seul" = "Zezika simika irery",
  "Fumure + chimique" = "Zezika biby + simika",
  "Aucun probl\u00e8me" = "Tsy misy olana",
  "Inondation" = "Tondra-drano",
  "Manque d'eau" = "Tsy ampy rano",
  "Repiquage traditionnel" = "Ketsa nentim-paharazana",
  "\u00c9pargne / assurance" = "Tahiry / fiantohana",
  "Autoconsommation" = "Fihinanana manokana",
  "Vente" = "Fivarotana",

  # ── Chapter 06: Food security ──
  "Bonne" = "Tsara",
  "Moyenne" = "Antonony",
  "S\u00e9v\u00e8re" = "Mafy",
  "Mod\u00e9r\u00e9e" = "Antonony",
  "L\u00e9g\u00e8re" = "Maivana",
  "Jamais" = "Tsy mbola",
  "Rarement" = "Indraindray",
  "Parfois" = "Matetika kely",
  "Souvent" = "Matetika",

  # ── Chapter 08: Disasters ──
  "Cyclone" = "Rivo-doza",
  "S\u00e9cheresse" = "Hain-tany",

  "Invasion acridienne" = "Fanafihan\u2019ny valala",
  "Grêle" = "Havandra",

  # ── Common table elements ──
  "Pourcentages" = "Isan-jato",
  "par observatoire" = "isaky ny toeram-pikarohana",
  "par hameau" = "isaky ny tan\u00e0na kely",

  # ── Source note ──
  "**Source\u00a0: Enqu\u00eate aupr\u00e8s des OR 2025**" = "**Loharano\u00a0: Fanadihadiana OR 2025**",
  "Source\u00a0: Enqu\u00eate aupr\u00e8s des OR 2025" = "Loharano\u00a0: Fanadihadiana OR 2025",
  "Source : Enqu\u00eate aupr\u00e8s des OR 2025" = "Loharano : Fanadihadiana OR 2025",

  # ── Common table titles (chapter 02) ──
  "Taille moyenne des m\u00e9nages par observatoire" = "Haben\u2019ny tokantrano isaky ny toeram-pikarohana",
  "Taille moyenne des m\u00e9nages" = "Haben\u2019ny tokantrano",
  "Taille moyenne des m\u00e9nages par hameau" = "Haben\u2019ny tokantrano isaky ny tan\u00e0na kely",
  "R\u00e9partition des membres du m\u00e9nage par tranche d'\u00e2ge" = "Fizarana ny mambra araka ny taona",
  "Origine des chefs de m\u00e9nage et conjoints" = "Fiavian\u2019ny lohan\u2019ny tokantrano sy ny vady",
  "Distribution de l'origine des CM et conjoints" = "Fizarana ny fiavian\u2019ny LT sy ny vady",
  "R\u00e9gion d'origine des CM et conjoints" = "Faritry ny fiavian\u2019ny LT sy ny vady",
  "Raison d'implantation des CM et conjoints" = "Antony nipetrahan\u2019ny LT sy ny vady",
  "Distribution des raisons d'implantation des CM et conjoints" = "Fizarana ny antony nipetrahan\u2019ny LT sy ny vady",
  "Caract\u00e9ristiques des chefs de m\u00e9nage" = "Toetoetry ny lohan\u2019ny tokantrano",
  "R\u00e9partition des chefs de m\u00e9nage selon leur niveau d'\u00e9ducation" = "Fizarana ny LT araka ny haavo-pahaizana",
  "R\u00e9partition des CM selon le niveau d'\u00e9ducation et le sexe" = "Fizarana ny LT araka ny haavo-pahaizana sy ny lahy/vavy",
  "Aptitude \u00e0 la lecture des chefs de m\u00e9nage" = "Fahaizan\u2019ny LT mamaky teny",
  "Aptitude \u00e0 la lecture des CM selon le sexe" = "Fahaizan\u2019ny LT mamaky teny araka ny lahy/vavy",
  "Aptitude \u00e0 l'\u00e9criture des chefs de m\u00e9nage" = "Fahaizan\u2019ny LT manoratra",
  "Aptitude \u00e0 l'\u00e9criture des CM selon le sexe" = "Fahaizan\u2019ny LT manoratra araka ny lahy/vavy",
  "Activit\u00e9s principales du chef de m\u00e9nage" = "Asa fototr\u2019ny lohan\u2019ny tokantrano",
  "Activit\u00e9s principales des CM (%)" = "Asa fototr\u2019ny LT (%)",
  "Activit\u00e9s principales des CM par sexe" = "Asa fototr\u2019ny LT araka ny lahy/vavy",
  "Activit\u00e9s principales des CM par observatoire et sexe (%)" = "Asa fototr\u2019ny LT isaky ny toeram-pikarohana sy lahy/vavy (%)",
  "Caract\u00e9ristiques des autres membres de m\u00e9nage" = "Toetoetry ny mambra hafa ao amin\u2019ny tokantrano",
  "Niveau d'\u00e9ducation des autres membres de m\u00e9nage" = "Haavo-pahaizana ny mambra hafa",
  "Niveau d'\u00e9ducation des autres membres par sexe" = "Haavo-pahaizana ny mambra hafa araka ny lahy/vavy",
  "Aptitude \u00e0 la lecture des autres membres" = "Fahaizan\u2019ny mambra hafa mamaky teny",
  "Aptitude \u00e0 la lecture des autres membres selon le sexe" = "Fahaizan\u2019ny mambra hafa mamaky teny araka ny lahy/vavy",
  "Aptitude \u00e0 l'\u00e9criture des autres membres" = "Fahaizan\u2019ny mambra hafa manoratra",
  "Aptitude \u00e0 l'\u00e9criture des autres membres selon le sexe" = "Fahaizan\u2019ny mambra hafa manoratra araka ny lahy/vavy",
  "Taux de scolarisation des enfants de 6 \u00e0 14 ans" = "Tahan\u2019ny fanabeazana ho an\u2019ny ankizy 6 ka hatramin\u2019ny 14 taona",
  "Classe suivie en 2024-2025 par cat\u00e9gorie d'\u00e2ge" = "Kilasy narahina tamin\u2019ny 2024-2025 araka ny sokajin-taona",
  "Principales raisons de non-scolarisation ou d'arr\u00eat des \u00e9tudes (6-15 ans)" = "Antony tsy fianarana na fijanonana (6-15 taona)",
  "Raisons de non-fr\u00e9quentation de l'\u00e9cole (6-15 ans)" = "Antony tsy fanarahana sekoly (6-15 taona)",
  "Activit\u00e9s principales des autres membres" = "Asa fototr\u2019ny mambra hafa",
  "Activit\u00e9s principales des autres membres (%)" = "Asa fototr\u2019ny mambra hafa (%)",
  "Activit\u00e9s principales des autres membres par sexe" = "Asa fototr\u2019ny mambra hafa araka ny lahy/vavy",
  "Activit\u00e9s des autres membres par observatoire et sexe (%)" = "Asa ny mambra hafa isaky ny toeram-pikarohana sy lahy/vavy (%)",

  # ── Short terms that must come LAST to avoid partial matches ──
  # "Non" alone is risky (appears in "Non scolarisé" etc.)
  # Only match as standalone word — handled via translate_html_mg()
  "Non" = "Tsia"
)


# ── Translation helpers ──────────────────────────────────────────

#' Translate a character vector using FR_TO_MG lookup
#' @param x Character vector
#' @return Character vector with translations applied
translate_mg <- function(x) {
  ifelse(!is.na(x) & x %in% names(FR_TO_MG), FR_TO_MG[x], x)
}

#' Translate selected columns of a data frame from FR to MG
#' @param data Data frame
#' @param cols Character vector of column names to translate
#' @return Data frame with translated columns
translate_data_mg <- function(data, cols) {
  for (col in cols) {
    if (!col %in% names(data)) {
      next
    }
    if (is.factor(data[[col]])) {
      levels(data[[col]]) <- translate_mg(levels(data[[col]]))
    } else if (is.character(data[[col]])) {
      data[[col]] <- translate_mg(data[[col]])
    }
  }
  data
}

#' Auto-detect and translate all translatable columns in a data frame
#'
#' Scans all character and factor columns for values matching FR_TO_MG
#' keys and translates them. Useful when you don't know which columns
#' to translate ahead of time.
#' @param data Data frame
#' @return Data frame with translated columns
auto_translate_data_mg <- function(data) {
  fr_keys <- names(FR_TO_MG)
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      lvls <- levels(data[[col]])
      if (any(lvls %in% fr_keys)) {
        levels(data[[col]]) <- translate_mg(lvls)
      }
    } else if (is.character(data[[col]])) {
      vals <- unique(data[[col]])
      if (any(vals %in% fr_keys, na.rm = TRUE)) {
        data[[col]] <- translate_mg(data[[col]])
      }
    }
  }
  data
}

#' Translate an HTML string by replacing known French terms with Malagasy
#'
#' Applies FR_TO_MG replacements in longest-first order to avoid
#' partial matches (e.g. "Non scolarisé" is replaced before "Non").
#' Only operates on text content; HTML tags/attributes are unlikely
#' to contain French survey terms.
#'
#' @param html_str Character string of HTML
#' @return HTML string with French terms replaced by Malagasy
translate_html_mg <- function(html_str) {
  # Sort keys by decreasing length for greedy matching
  keys <- names(FR_TO_MG)
  ord <- order(-nchar(keys))
  for (i in ord) {
    html_str <- gsub(keys[i], FR_TO_MG[i], html_str, fixed = TRUE)
  }
  html_str
}


# ── Variable label translations from Excel ───────────────────────
# Loaded lazily on first access.

.var_labels_env <- new.env(parent = emptyenv())
.var_labels_env$loaded <- FALSE
.var_labels_env$fr_to_mg <- character(0)

#' Load FR→MG variable label mapping from Excel
#'
#' Returns a named character vector: names = French labels (from Stata),
#' values = Malagasy equivalents.  Useful for translating output from
#' tabulate_binary_set() which uses var_label().
#' @return Named character vector
load_var_labels_mg <- function() {
  if (.var_labels_env$loaded) {
    return(.var_labels_env$fr_to_mg)
  }

  xlsx_path <- "data/labels_valuelabels.xlsx"
  if (!file.exists(xlsx_path)) {
    .var_labels_env$loaded <- TRUE
    return(character(0))
  }

  dict <- readxl::read_excel(
    xlsx_path,
    sheet = "dictionnaire",
    .name_repair = "unique_quiet"
  ) |>
    dplyr::select(var_name = 7, label_fr = 8, label_mg = 9) |>
    dplyr::filter(!is.na(var_name), !is.na(label_fr), !is.na(label_mg))

  # Build lookup: FR label → MG label (excluding identical pairs)
  diff <- dict |> dplyr::filter(label_fr != label_mg)
  result <- stats::setNames(diff$label_mg, diff$label_fr)

  # Also add str_to_title() versions (tabulate_binary_set applies this)
  titled_keys <- stringr::str_to_title(names(result))
  titled <- stats::setNames(result, titled_keys)
  # Remove duplicates (keep original casing)
  titled <- titled[!names(titled) %in% names(result)]
  result <- c(result, titled)

  .var_labels_env$fr_to_mg <- result
  .var_labels_env$loaded <- TRUE
  result
}


# ── Extended HTML translation including Excel labels ──────────────

#' Full HTML translation: hand-curated + Excel variable labels
#'
#' Merges FR_TO_MG with variable labels from the Excel dictionary,
#' then applies longest-first replacement.
#' @param html_str Character string of HTML
#' @return Translated HTML string
translate_html_mg_full <- function(html_str) {
  var_labels <- load_var_labels_mg()
  # Hand-curated takes priority (added after, so overwrites in c())
  all_translations <- c(var_labels, FR_TO_MG)
  keys <- names(all_translations)
  ord <- order(-nchar(keys))
  for (i in ord) {
    html_str <- gsub(keys[i], all_translations[i], html_str, fixed = TRUE)
  }
  html_str
}
