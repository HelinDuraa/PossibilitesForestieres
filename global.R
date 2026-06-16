# global.R — Possibilités forestières du Québec (SYN-00624)
# Chargé une seule fois au démarrage : packages, données, fonctions.

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(DT)
library(plotly)
library(scales)


# ---- Chemins vers les données ----
path_xlsx      <- "data/SYN-00624-Principales-variables-forestieres-associees-au-calcul-4.12.0.xlsx"
path_shp_dir   <- "data/Shapefile"
path_shp_layer <- "POSS_FOR_2023_28_UA"
path_rds       <- "data/ua_simplifie.rds"


# ---- Fonctions utilitaires ----

# Uniformise les codes UA au format "011-71"
normaliser_ua <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "\\s")
  ifelse(!str_detect(x, "-") & nchar(x) == 5,
         paste0(substr(x, 1, 3), "-", substr(x, 4, 5)), x)
}

na_xlsx <- c("", "N/D", "n/d", "ND")

# Convertit un nombre écrit à la française en numérique.
# Retire espaces, espaces insécables ET le signe %, puis remplace la virgule
# par un point. Indispensable pour le taux de récolte, stocké comme "1,9 %".
# Sans danger sur une colonne déjà numérique.
num_fr <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "[\\s\u00a0%]")
  x <- str_replace_all(x, ",", ".")
  as.numeric(x)
}

# Formatage à la française (virgule décimale, espace insécable)
fmt_nb  <- function(x, d = 0) scales::comma(round(x, d), big.mark = "\u00a0",
                                            decimal.mark = ",", accuracy = 10^(-d))
fmt_dec <- function(x, d = 1) format(round(x, d), nsmall = d, decimal.mark = ",",
                                     big.mark = "\u00a0", trim = TRUE, scientific = FALSE)
fmt_pct <- function(x) paste0(fmt_dec(x * 100, 1), "\u00a0%")
fmt_ha  <- function(x) paste0(fmt_nb(x), "\u00a0ha")
fmt_m3  <- function(x) paste0(fmt_nb(x), "\u00a0m\u00b3")
fmt_dol <- function(x) paste0(fmt_nb(x), "\u00a0$")


# ---- Lecture des données ----
# Une seule fonction pour toutes les feuilles : lit, retire les lignes de
# totaux, normalise l'UA et convertit toutes les colonnes (sauf region/ua)
# en nombres via num_fr. keep = colonnes à conserver à la fin.
lire <- function(sheet, col_names, range = NULL, skip = 0, keep = NULL) {
  d <- read_excel(path_xlsx, sheet = sheet, range = range, skip = skip,
                  col_names = col_names, na = na_xlsx) |>
    filter(!is.na(ua), ua != "",
           !str_detect(as.character(ua), "Total|%|Retraits")) |>
    mutate(ua = normaliser_ua(ua),
           across(!any_of(c("region", "ua")), num_fr))
  if (!is.null(keep)) d <- select(d, all_of(keep))
  d
}

superficies <- lire(
  "Superficies", range = cell_limits(c(6, 1), c(NA, 8)),
  col_names = c("region", "ua", "sup_totale_ha", "sup_non_forestier_ha",
                "sup_peu_productif_ha", "sup_exclu_amenagement_ha",
                "sup_destinee_amenagement_ha", "pct_destinee_amenagement"))

possibilites <- lire(
  "Possibilités forestières", range = cell_limits(c(5, 1), c(NA, 12)),
  col_names = c("region", "ua", "SEPM", "Thuya", "Pruche", "Pins_blanc_rouge",
                "Peupliers", "Bouleau_papier", "Bouleau_jaune", "Erables",
                "Autres_feuillus_durs", "Possibilite_totale_m3an"))

sylvicole <- lire(
  "Stratégie sylvicole", range = cell_limits(c(6, 15), c(NA, 23)),
  col_names = c("region", "ua", "coupes_totales_haan", "eclaircie_commerciale_haan",
                "coupes_partielles_haan", "pct_coupes_totales_recolte",
                "plantations_haan", "education_haan", "preparation_terrain_haan"))

dollar_par_ha <- lire(
  "$ par ha", range = cell_limits(c(5, 14), c(NA, 18)),
  col_names = c("region", "ua", "budget_an", "sup_dest_check", "budget_par_ha"),
  keep = c("ua", "budget_an", "budget_par_ha"))

dollar_par_m3 <- lire(
  "$ par m³", range = cell_limits(c(5, 12), c(NA, 16)),
  col_names = c("region", "ua", "budget_check", "poss_check", "budget_par_m3"),
  keep = c("ua", "budget_par_m3"))

productivite <- lire(
  "m³ par ha par annnée", range = cell_limits(c(5, 15), c(NA, 19)),
  col_names = c("region", "ua", "poss_check2", "sup_check2", "productivite_m3hanan"),
  keep = c("ua", "productivite_m3hanan"))

dendro <- lire(
  "Dendrométrie", skip = 6,
  col_names = c("region", "ua", "volume_sur_pied_m3", "poss_check3",
                "taux_recolte_pct", "delai_entre_interv_an",
                "prelevement_moyen_cpf2025_m3ha", "prelevement_moyen_cpf2018_pct",
                "surface_terriere_m2ha", "age_moyen_recolte_an",
                "volume_moyen_recolte_m3ha", "dimension_bois_sepm_dcm3tige"),
  keep = c("ua", "volume_sur_pied_m3", "taux_recolte_pct", "delai_entre_interv_an",
           "prelevement_moyen_cpf2025_m3ha", "prelevement_moyen_cpf2018_pct",
           "surface_terriere_m2ha", "age_moyen_recolte_an",
           "volume_moyen_recolte_m3ha", "dimension_bois_sepm_dcm3tige")) |>
  # Excel stocke un pourcentage comme une fraction (0,019 = 1,9 %).
  # On multiplie par 100 pour obtenir un vrai pourcentage à afficher.
  mutate(taux_recolte_pct = taux_recolte_pct * 100)


# ---- Table maîtresse : une ligne par UA ----
donnees_ua <- superficies |>
  left_join(select(possibilites, -region), by = "ua") |>
  left_join(select(sylvicole, -region),    by = "ua") |>
  left_join(dollar_par_ha, by = "ua") |>
  left_join(dollar_par_m3, by = "ua") |>
  left_join(productivite,  by = "ua") |>
  left_join(dendro,        by = "ua")


# ---- Dictionnaire des variables (onglet Données) ----
dico_variables <- list(
  "Superficies (ha)" = list(
    "Superficie totale"             = "sup_totale_ha",
    "Sup. destinée à l'aménagement" = "sup_destinee_amenagement_ha",
    "% destinée à l'aménagement"    = "pct_destinee_amenagement",
    "Territoire non forestier"      = "sup_non_forestier_ha",
    "Forestier peu productif"       = "sup_peu_productif_ha",
    "Exclu de l'aménagement"        = "sup_exclu_amenagement_ha"
  ),
  "Possibilités forestières (m³/an)" = list(
    "Possibilité totale"   = "Possibilite_totale_m3an",
    "SEPM"                 = "SEPM",
    "Peupliers"            = "Peupliers",
    "Bouleau à papier"     = "Bouleau_papier",
    "Bouleau jaune"        = "Bouleau_jaune",
    "Érables"              = "Erables",
    "Pins blanc et rouge"  = "Pins_blanc_rouge",
    "Thuya"                = "Thuya",
    "Pruche"               = "Pruche",
    "Autres feuillus durs" = "Autres_feuillus_durs"
  ),
  "Stratégie sylvicole (ha/an)" = list(
    "Coupes totales"             = "coupes_totales_haan",
    "Éclaircie commerciale SEPM" = "eclaircie_commerciale_haan",
    "Coupes partielles"          = "coupes_partielles_haan",
    "% coupes totales / récolte" = "pct_coupes_totales_recolte",
    "Plantations"                = "plantations_haan",
    "Éducation"                  = "education_haan",
    "Préparation de terrain"     = "preparation_terrain_haan"
  ),
  "Économie ($)" = list(
    "Budget investi ($/an)" = "budget_an",
    "Budget investi ($/ha)" = "budget_par_ha",
    "Budget investi ($/m³)" = "budget_par_m3"
  ),
  "Productivité et dendrométrie" = list(
    "Productivité (m³/ha/an)"         = "productivite_m3hanan",
    "Volume sur pied (m³ brut)"       = "volume_sur_pied_m3",
    "Taux de récolte du volume (%)"   = "taux_recolte_pct",
    "Délai entre interventions (an)"  = "delai_entre_interv_an",
    "Prélèvement moyen CPF (m³/ha)"   = "prelevement_moyen_cpf2025_m3ha",
    "Surface terrière (m²/ha)"        = "surface_terriere_m2ha",
    "Âge moyen de récolte (an)"       = "age_moyen_recolte_an",
    "Volume moyen récolté (m³/ha)"    = "volume_moyen_recolte_m3ha",
    "Dimension bois SEPM (dcm³/tige)" = "dimension_bois_sepm_dcm3tige"
  )
)


# ---- Polygones des UA (données spatiales) ----
# On lit la version .rds simplifiée si elle existe (rapide), sinon le shapefile.
# st_transform(4326) convertit en WGS84 pour Leaflet.
champs_ua_candidats <- c("UA_TXT", "NO_UA", "UA", "ID_UA", "FMU")

if (file.exists(path_rds)) {
  ua_sf <- readRDS(path_rds) |>
    rename(ua = UA_TXT) |>
    mutate(ua = normaliser_ua(ua)) |>
    left_join(donnees_ua, by = "ua")
} else if (dir.exists(path_shp_dir)) {
  ua_shp <- sf::read_sf(dsn = path_shp_dir, layer = path_shp_layer)
  champ_ua_shp <- intersect(champs_ua_candidats, names(ua_shp))[1]
  if (is.na(champ_ua_shp))
    champ_ua_shp <- names(ua_shp)[sapply(st_drop_geometry(ua_shp), is.character)][1]
  ua_sf <- ua_shp |>
    st_make_valid() |>
    mutate(ua = normaliser_ua(.data[[champ_ua_shp]])) |>
    select(ua) |>
    st_transform(crs = 4326) |>
    left_join(donnees_ua, by = "ua")
} else {
  warning("Ni .rds ni shapefile trouvés. La carte sera vide.")
  ua_sf <- NULL
}


# ---- Thème visuel ----
theme_app <- bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#2C5F4E", secondary = "#A8B5A0",
  base_font = font_google("Inter"), heading_font = font_google("Inter"),
  font_scale = 0.95)


# ---- Fonctions graphiques (appelées depuis server.R) ----
# À garder dans global.R : rien ne doit être défini après server() dans server.R.

palette_essences <- c(
  "SEPM"                 = "#2C5F4E",
  "Peupliers"            = "#A8B5A0",
  "Bouleau a papier"     = "#D4C5A0",
  "Bouleau jaune"        = "#C2A85F",
  "Erables"              = "#B85C3C",
  "Pins blanc & rouge"   = "#5C7A8C",
  "Thuya"                = "#6B8E5A",
  "Pruche"               = "#8B7355",
  "Autres feuillus durs" = "#999999"
)

noms_essences <- c(
  "SEPM"                 = "SEPM",
  "Peupliers"            = "Peupliers",
  "Bouleau_papier"       = "Bouleau a papier",
  "Bouleau_jaune"        = "Bouleau jaune",
  "Erables"              = "Erables",
  "Pins_blanc_rouge"     = "Pins blanc & rouge",
  "Thuya"                = "Thuya",
  "Pruche"               = "Pruche",
  "Autres_feuillus_durs" = "Autres feuillus durs"
)

# Possibilité par essence : donut si 1 UA, barres empilées si plusieurs.
graph_essences <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  long <- df |>
    select(ua, all_of(names(noms_essences))) |>
    pivot_longer(-ua, names_to = "essence_raw", values_to = "vol") |>
    filter(!is.na(vol), vol > 0) |>
    mutate(essence = noms_essences[essence_raw])
  
  if (nrow(long) == 0) return(NULL)
  
  if (nrow(df) == 1) {
    agg <- long |> group_by(essence) |> summarise(vol = sum(vol), .groups = "drop")
    plot_ly(agg, labels = ~essence, values = ~vol, type = "pie", hole = 0.55,
            sort = TRUE,
            marker = list(colors = palette_essences[agg$essence],
                          line   = list(color = "white", width = 1.5)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value:,.0f} m\u00b3/an<br>%{percent}<extra></extra>") |>
      layout(
        showlegend  = FALSE,
        margin      = list(t = 5, b = 5, l = 5, r = 5),
        annotations = list(text = paste0("<b>", fmt_nb(sum(agg$vol)), "</b><br>m\u00b3/an"),
                           showarrow = FALSE, font = list(size = 13))
      )
  } else {
    plot_ly(long, x = ~ua, y = ~vol, color = ~essence, colors = palette_essences,
            type = "bar",
            hovertemplate = "<b>%{x}</b> \u2014 %{fullData.name}<br>%{y:,.0f} m\u00b3/an<extra></extra>") |>
      layout(
        barmode = "stack",
        xaxis   = list(title = ""),
        yaxis   = list(title = "m\u00b3/an", separatethousands = TRUE),
        legend  = list(orientation = "h", x = 0, y = -0.3, title = list(text = "")),
        margin  = list(t = 5, b = 90, l = 60, r = 5)
      )
  }
}

# Répartition des superficies
graph_superficies <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  long <- df |>
    select(ua,
           "Amén."         = sup_destinee_amenagement_ha,
           "Non forestier" = sup_non_forestier_ha,
           "Peu productif" = sup_peu_productif_ha,
           "Exclu"         = sup_exclu_amenagement_ha) |>
    pivot_longer(-ua, names_to = "cat", values_to = "ha") |>
    filter(!is.na(ha), ha > 0)
  
  couleurs_sup <- c("Amén."         = "#2C5F4E",
                    "Non forestier" = "#A8B5A0",
                    "Peu productif" = "#D4C5A0",
                    "Exclu"         = "#999999")
  
  plot_ly(long, x = ~ua, y = ~ha, color = ~cat, colors = couleurs_sup,
          type = "bar",
          hovertemplate = "<b>%{x}</b> \u2014 %{fullData.name}<br>%{y:,.0f} ha<extra></extra>") |>
    layout(barmode = "group", xaxis = list(title = ""),
           yaxis = list(title = "ha", separatethousands = TRUE),
           legend = list(orientation = "h", x = 0, y = -0.3, title = list(text = "")),
           margin = list(t = 5, b = 90, l = 60, r = 5))
}

# Travaux sylvicoles
graph_sylvicole <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  long <- df |>
    select(ua,
           "Coupes tot."   = coupes_totales_haan,
           "Eclaircie"     = eclaircie_commerciale_haan,
           "C. partielles" = coupes_partielles_haan,
           "Plantations"   = plantations_haan,
           "Education"     = education_haan,
           "Prep. terrain" = preparation_terrain_haan) |>
    pivot_longer(-ua, names_to = "trt", values_to = "ha_an") |>
    filter(!is.na(ha_an), ha_an > 0)
  
  plot_ly(long, x = ~ua, y = ~ha_an, color = ~trt, type = "bar",
          hovertemplate = "<b>%{x}</b> \u2014 %{fullData.name}<br>%{y:,.0f} ha/an<extra></extra>") |>
    layout(barmode = "group", xaxis = list(title = ""),
           yaxis = list(title = "ha/an", separatethousands = TRUE),
           legend = list(orientation = "h", x = 0, y = -0.3, title = list(text = "")),
           margin = list(t = 5, b = 90, l = 60, r = 5))
}

# Camembert : % de possibilité par essence, toutes UA agrégées
graph_pie_essences <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  cols <- names(noms_essences)
  agg <- df |>
    select(all_of(cols)) |>
    colSums(na.rm = TRUE) |>
    as.data.frame() |>
    rownames_to_column("essence_raw") |>
    rename(vol = 2) |>
    filter(vol > 0) |>
    mutate(essence = noms_essences[essence_raw],
           pct     = round(vol / sum(vol) * 100, 1))
  
  if (nrow(agg) == 0) return(NULL)
  
  plot_ly(agg, labels = ~essence, values = ~vol, type = "pie", sort = TRUE,
          marker = list(colors = palette_essences[agg$essence],
                        line = list(color = "white", width = 1.5)),
          textinfo = "percent", textposition = "inside",
          hovertemplate = "<b>%{label}</b><br>%{value:,.0f}\u00a0m\u00b3/an<br>%{percent}<extra></extra>") |>
    layout(showlegend = TRUE,
           legend = list(orientation = "v", x = 1, y = 0.5, font = list(size = 11)),
           margin = list(t = 5, b = 5, l = 5, r = 120))
}

# Camembert : part de la possibilité totale par UA (si 2+ UA)
graph_pie_ua <- function(df) {
  if (is.null(df) || nrow(df) == 0 || nrow(df) < 2) return(NULL)
  
  agg <- df |>
    select(ua, Possibilite_totale_m3an) |>
    filter(!is.na(Possibilite_totale_m3an)) |>
    mutate(pct = round(Possibilite_totale_m3an / sum(Possibilite_totale_m3an) * 100, 1))
  
  couleurs <- colorRampPalette(c("#2C5F4E", "#6B8E5A", "#A8B5A0", "#D4C5A0", "#C2A85F"))(nrow(agg))
  
  plot_ly(agg, labels = ~ua, values = ~Possibilite_totale_m3an, type = "pie", sort = TRUE,
          marker = list(colors = couleurs, line = list(color = "white", width = 1.5)),
          textinfo = "percent", textposition = "inside",
          hovertemplate = "<b>UA\u00a0%{label}</b><br>%{value:,.0f}\u00a0m\u00b3/an<br>%{percent}<extra></extra>") |>
    layout(showlegend = TRUE,
           legend = list(orientation = "v", x = 1, y = 0.5, font = list(size = 11)),
           margin = list(t = 5, b = 5, l = 5, r = 120))
}
