# ── Distance from survey sites to nearest Protected Area ──────────────────────
library(tidyverse)
library(sf)

# ── 1. Survey site locations (fokontany-level) ───────────────────────────────
fokontany_centroids <- tribble(
  ~Observatory , ~Fokontany         , ~lat       , ~lon      ,
  "Alaotra"    , "Avaradrano"       , -17.805414 , 48.469958 ,
  "Alaotra"    , "Feramanga Atsimo" , -17.844153 , 48.373383 ,
  "Alaotra"    , "Mangabe"          , -17.879586 , 48.405244 ,
  "Alaotra"    , "Analamiranga"     , -17.572375 , 48.200017 ,
  "Alaotra"    , "Maritampona"      , -17.603500 , 48.207639 ,
  "Alaotra"    , "Ambohidrony"      , -17.729175 , 48.208611 ,
  "Alaotra"    , "Ambatomanga"      , -17.741178 , 48.200647 ,
  "Marovoay"   , "Ampijoroa"        , -16.233414 , 46.477984 ,
  "Marovoay"   , "Maroala"          , -16.228857 , 46.539738 ,
  "Marovoay"   , "Madiromiongana"   , -16.086480 , 46.749270 ,
  "Marovoay"   , "Bepako"           , -16.163712 , 46.658402
)

sites_sf <- st_as_sf(fokontany_centroids, coords = c("lon", "lat"), crs = 4326)

# ── 2. Load WDPA protected areas ─────────────────────────────────────────────
wdpa_mdg <- read_rds("data/wdpa/MDG_protected_areas.rds") |>
  filter(
    !str_detect(DESIG, "Ramsar"),
    !str_detect(DESIG, "UNESCO")
  )

# ── 3. Compute distance from each site to nearest PA ─────────────────────────# Repair invalid geometries (some WDPA polygons have self-intersections)
wdpa_mdg <- st_make_valid(wdpa_mdg)
# st_distance returns a matrix: rows = sites, cols = PAs (metres, great-circle)
dist_matrix <- st_distance(sites_sf, wdpa_mdg) # units: metres

# For each site, find the index and distance to the closest PA
nearest_idx <- apply(dist_matrix, 1, which.min)
nearest_dist <- apply(dist_matrix, 1, min)

sites_dist <- fokontany_centroids |>
  mutate(
    nearest_pa = wdpa_mdg$NAME[nearest_idx],
    nearest_pa_type = wdpa_mdg$DESIG[nearest_idx],
    distance_m = as.numeric(nearest_dist),
    distance_km = round(distance_m / 1000, 2)
  )

sites_dist |>
  select(Observatory, Fokontany, nearest_pa, nearest_pa_type, distance_km)
