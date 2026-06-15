# =============================================================================
# global.R
# Statistiques des possibilités forestières du Québec — SYN-00624
# =============================================================================
#
# RÔLE DE CE FICHIER DANS UNE APPLICATION SHINY
# -----------------------------------------------
# Une application Shiny est divisée en trois fichiers :
#   - global.R  : s'exécute UNE SEULE FOIS au démarrage de l'application.
#                 On y met tout ce qui doit être disponible partout :
#                 packages, données, fonctions utilitaires.
#   - ui.R      : décrit l'INTERFACE (ce que l'usager voit).
#   - server.R  : décrit la LOGIQUE (ce qui se passe quand l'usager interagit).
#
# Tout objet créé dans global.R est accessible dans ui.R ET server.R.
# C'est pourquoi on y charge les données : elles n'ont besoin d'être lues
# qu'une fois, peu importe combien d'usagers se connectent.


# =============================================================================
# 1. PACKAGES
# =============================================================================
# La fonction library() charge un package installé.
# Si un package est manquant, l'installer avec :
#   install.packages("nom_du_package")
# Pour bslib et bsicons : install.packages(c("bslib", "bsicons"))

library(shiny)     # Le cœur de toute application Shiny
library(bslib)     # Thèmes modernes et composants UI (cards, value_box, etc.)
library(bsicons)   # Icônes Bootstrap (utilisées dans bslib)
library(tidyverse) # Ensemble de packages pour manipuler les données :
#   - dplyr  : filter(), mutate(), select(), left_join()...
#   - tidyr  : pivot_longer(), pivot_wider()...
#   - ggplot2: graphiques (non utilisé ici, remplacé par plotly)
#   - readr  : lecture de CSV
#   - stringr: manipulation de chaînes (str_detect(), etc.)
library(readxl)    # Lecture de fichiers Excel (.xlsx)
library(sf)        # "Simple Features" — manipulation de données spatiales vectorielles
# (polygones, points, lignes). Équivalent R de geopandas en Python.
library(leaflet)   # Cartes interactives dans le navigateur (basé sur Leaflet.js)
library(DT)        # Tableaux interactifs (tri, filtre, pagination)
library(plotly)    # Graphiques interactifs (basé sur Plotly.js)
library(scales)    # Formatage de nombres (virgules, pourcentages, etc.)


# =============================================================================
# 2. CHEMINS VERS LES FICHIERS DE DONNÉES
# =============================================================================
# Bonne pratique : centraliser les chemins ici plutôt que de les répéter
# dans le code. Si un fichier est déplacé, on ne corrige qu'une seule ligne.
#
# Les chemins sont RELATIFS à la racine du projet RStudio (.Rproj).
# Cela signifie que "data/fichier.xlsx" sera cherché dans le dossier "data/"
# situé au même niveau que vos fichiers global.R / ui.R / server.R.

path_xlsx      <- "data/SYN-00624-Principales-variables-forestieres-associees-au-calcul-4.12.0.xlsx"
path_shp_dir   <- "data/Shapefile"        # Dossier contenant le shapefile (.shp + fichiers associés)
path_shp_layer <- "POSS_FOR_2023_28_UA"  # Nom de la couche (sans extension .shp)
path_rds       <- "data/ua_simplifie.rds" # Version pré-simplifiée des polygones (plus rapide à charger)
# Généré par un script séparé (00_prep_data.R)


# =============================================================================
# 3. FONCTIONS UTILITAIRES
# =============================================================================

# --- 3.1 Normalisation des codes UA -------------------------------------------
# Les codes UA peuvent être écrits "01171" ou "011-71" selon la feuille Excel.
# Cette fonction standardise tout au format "011-71".
#
# Pourquoi une fonction ? Pour ne pas répéter le même code à 7 endroits.
# Principe DRY : "Don't Repeat Yourself".

normaliser_ua <- function(x) {
  x <- as.character(x)          # S'assurer que x est du texte (pas un nombre)
  x <- str_remove_all(x, "\\s") # Enlever tous les espaces (\\s = espace en regex)
  ifelse(
    !str_detect(x, "-") & nchar(x) == 5,  # Si pas de tiret ET 5 caractères...
    paste0(substr(x, 1, 3), "-", substr(x, 4, 5)),  # ...insérer un tiret après le 3e
    x                                                 # ...sinon laisser tel quel
  )
}

# --- 3.2 Valeurs à traiter comme NA dans Excel --------------------------------
# Excel peut contenir "N/D" pour "non disponible". On dit à read_excel
# de les convertir automatiquement en NA (valeur manquante en R).

na_xlsx <- c("", "N/D", "n/d", "ND")

# --- 3.3 Fonctions de formatage des nombres -----------------------------------
# Ces fonctions prennent un nombre et retournent une chaîne de caractères
# bien formatée pour l'affichage.
#
# \u00a0 = espace insécable (Unicode) — évite les coupures de ligne indésirables
# \u00b3 = exposant ³ (m³)
# \u00b2 = exposant ² (m²)
# \u2014 = tiret cadratin — (pour les valeurs manquantes)

fmt_nb  <- function(x, d = 0) {
  # Formate un nombre avec séparateur de milliers et d décimales
  # Exemple : fmt_nb(1234567) → "1 234 567"
  scales::comma(round(x, d), big.mark = "\u00a0", accuracy = 10^(-d))
}

fmt_pct <- function(x) {
  # x est une PROPORTION (entre 0 et 1), pas un pourcentage
  # Exemple : fmt_pct(0.754) → "75.4 %"
  # IMPORTANT : dans le fichier Excel, pct_destinee_amenagement vaut 0.754
  # (et non 75.4). On multiplie par 100 ici pour l'affichage.
  paste0(round(x * 100, 1), "\u00a0%")
}

fmt_ha  <- function(x) paste0(fmt_nb(x), "\u00a0ha")
fmt_m3  <- function(x) paste0(fmt_nb(x), "\u00a0m\u00b3")
fmt_dol <- function(x) paste0(fmt_nb(x), "\u00a0$")

fmt_nb  <- function(x, d = 0) {
  scales::comma(round(x, d), big.mark = "\u00a0",
                decimal.mark = ",", accuracy = 10^(-d))
}

# Nombre décimal formaté à la française (virgule décimale, espace insécable)
fmt_dec <- function(x, d = 1) {
  format(round(x, d), nsmall = d, decimal.mark = ",",
         big.mark = "\u00a0", trim = TRUE, scientific = FALSE)
}

fmt_pct <- function(x) paste0(fmt_dec(x * 100, 1), "\u00a0%")
# =============================================================================
# 4. LECTURE DES DONNÉES EXCEL (SYN-00624)
# =============================================================================
# read_excel() lit une feuille Excel. Les arguments importants :
#   - sheet      : nom de la feuille
#   - range      : plage de cellules à lire (évite les en-têtes complexes)
#   - col_names  : noms qu'on donne aux colonnes
#   - na         : valeurs à traiter comme NA
#
# cell_limits(c(ligne_début, col_début), c(NA, col_fin)) signifie :
#   "commence à (ligne, col), va jusqu'à la dernière ligne, arrête à col_fin"
#   NA dans la ligne de fin = lire jusqu'au bout

## 4.1 Superficies ------------------------------------------------------------
superficies <- read_excel(
  path_xlsx, sheet = "Superficies",
  range    = cell_limits(c(6, 1), c(NA, 8)),
  col_names = c("region", "ua", "sup_totale_ha", "sup_non_forestier_ha",
                "sup_peu_productif_ha", "sup_exclu_amenagement_ha",
                "sup_destinee_amenagement_ha", "pct_destinee_amenagement"),
  na = na_xlsx
) |>
  # Le pipe |> (ou %>%) passe le résultat à la fonction suivante.
  # Lire comme : "prends le tableau, PUIS filtre, PUIS transforme"
  filter(
    !is.na(ua),                        # Enlever les lignes sans code UA
    ua != "",                          # Enlever les lignes vides
    !str_detect(ua, "Total|%|Retraits") # Enlever les lignes de totaux/notes
  ) |>
  mutate(
    ua = normaliser_ua(ua),
    # pct_destinee_amenagement est déjà une proportion (0-1) dans Excel
    # On la garde telle quelle — on convertira en % seulement à l'affichage
    pct_destinee_amenagement = as.numeric(pct_destinee_amenagement)
  )

## 4.2 Possibilités forestières par essence -----------------------------------
possibilites <- read_excel(
  path_xlsx, sheet = "Possibilités forestières",
  range    = cell_limits(c(5, 1), c(NA, 12)),
  col_names = c("region", "ua", "SEPM", "Thuya", "Pruche",
                "Pins_blanc_rouge", "Peupliers", "Bouleau_papier",
                "Bouleau_jaune", "Erables", "Autres_feuillus_durs",
                "Possibilite_totale_m3an"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(ua, "Total|%")) |>
  mutate(ua = normaliser_ua(ua))

## 4.3 Stratégie sylvicole (ha/an) -------------------------------------------
# Note : les données sont décalées — elles commencent à la colonne 15 dans Excel.
# cell_limits(c(6, 15), c(NA, 23)) = ligne 6, colonnes 15 à 23.

sylvicole <- read_excel(
  path_xlsx, sheet = "Stratégie sylvicole",
  range    = cell_limits(c(6, 15), c(NA, 23)),
  col_names = c("region", "ua", "coupes_totales_haan",
                "eclaircie_commerciale_haan", "coupes_partielles_haan",
                "pct_coupes_totales_recolte", "plantations_haan",
                "education_haan", "preparation_terrain_haan"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(as.character(ua), "Total|%")) |>
  mutate(
    region = as.character(region),
    ua     = normaliser_ua(ua),
    # across() applique une transformation à plusieurs colonnes à la fois
    # c(col1:col2) = toutes les colonnes de col1 jusqu'à col2
    across(coupes_totales_haan:preparation_terrain_haan, as.numeric)
  )

## 4.4 Budget par hectare ($/ha) ----------------------------------------------
dollar_par_ha <- read_excel(
  path_xlsx, sheet = "$ par ha",
  range    = cell_limits(c(5, 14), c(NA, 18)),
  col_names = c("region", "ua", "budget_an", "sup_dest_check", "budget_par_ha"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(as.character(ua), "Total|%")) |>
  mutate(ua = normaliser_ua(ua),
         across(c(budget_an, budget_par_ha), as.numeric)) |>
  select(ua, budget_an, budget_par_ha) # Garder seulement les colonnes utiles

## 4.5 Budget par mètre cube ($/m³) ------------------------------------------
dollar_par_m3 <- read_excel(
  path_xlsx, sheet = "$ par m³",
  range    = cell_limits(c(5, 12), c(NA, 16)),
  col_names = c("region", "ua", "budget_check", "poss_check", "budget_par_m3"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(as.character(ua), "Total|%")) |>
  mutate(ua = normaliser_ua(ua), budget_par_m3 = as.numeric(budget_par_m3)) |>
  select(ua, budget_par_m3)

## 4.6 Productivité (m³/ha/an) -----------------------------------------------
productivite <- read_excel(
  path_xlsx, sheet = "m³ par ha par annnée",
  range    = cell_limits(c(5, 15), c(NA, 19)),
  col_names = c("region", "ua", "poss_check2", "sup_check2", "productivite_m3hanan"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(as.character(ua), "Total|%")) |>
  mutate(ua = normaliser_ua(ua), productivite_m3hanan = as.numeric(productivite_m3hanan)) |>
  select(ua, productivite_m3hanan)

## 4.7 Dendrométrie ----------------------------------------------------------
# skip = 6 : sauter les 6 premières lignes (en-têtes sur plusieurs lignes dans Excel)
dendro <- read_excel(
  path_xlsx, sheet = "Dendrométrie",
  skip      = 6,
  col_names = c("region", "ua", "volume_sur_pied_m3", "poss_check3",
                "taux_recolte_pct", "delai_entre_interv_an",
                "prelevement_moyen_cpf2025_m3ha", "prelevement_moyen_cpf2018_pct",
                "surface_terriere_m2ha", "age_moyen_recolte_an",
                "volume_moyen_recolte_m3ha", "dimension_bois_sepm_dcm3tige"),
  na = na_xlsx
) |>
  filter(!is.na(ua), ua != "", !str_detect(as.character(ua), "Total|%")) |>
  mutate(
    ua = normaliser_ua(ua),
    across(c(volume_sur_pied_m3, taux_recolte_pct, delai_entre_interv_an,
             prelevement_moyen_cpf2025_m3ha, prelevement_moyen_cpf2018_pct,
             surface_terriere_m2ha, age_moyen_recolte_an,
             volume_moyen_recolte_m3ha, dimension_bois_sepm_dcm3tige), as.numeric)
  ) |>
  select(-region, -poss_check3) # Enlever les colonnes de vérification inutiles


# =============================================================================
# 5. TABLE MAÎTRESSE : une ligne par UA, toutes les variables en colonnes
# =============================================================================
# left_join() fusionne deux tableaux par une colonne commune ("ua" ici).
# "left" signifie : garder TOUTES les lignes du tableau de gauche,
# même si elles n'ont pas de correspondance dans le tableau de droite.
# Résultat : un grand tableau avec toutes les variables pour chaque UA.

donnees_ua <- superficies |>
  left_join(possibilites |> select(-region), by = "ua") |>
  left_join(sylvicole    |> select(-region), by = "ua") |>
  left_join(dollar_par_ha,                   by = "ua") |>
  left_join(dollar_par_m3,                   by = "ua") |>
  left_join(productivite,                    by = "ua") |>
  left_join(dendro,                          by = "ua")


# =============================================================================
# 6. DICTIONNAIRE DES VARIABLES (pour l'onglet Données)
# =============================================================================
# Une liste imbriquée : groupe → label lisible → nom de colonne dans donnees_ua.
# Utilisée pour construire les menus déroulants et les étiquettes des axes.

dico_variables <- list(
  "Superficies (ha)" = list(
    "Superficie totale"             = "sup_totale_ha",
    "Sup. destinée à l'aménagement" = "sup_destinee_amenagement_ha",
    "% destinée à l'aménagement"   = "pct_destinee_amenagement",
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


# =============================================================================
# 7. DONNÉES SPATIALES (polygones des UA)
# =============================================================================
# sf (Simple Features) est le standard pour les données vectorielles en R.
# Un objet sf est un data frame ordinaire avec une colonne spéciale "geometry"
# qui contient la forme géographique de chaque entité (polygone ici).
#
# SYSTÈMES DE COORDONNÉES (CRS) :
#   - Les shapefiles forestiers du Québec utilisent souvent la projection
#     MTM (NAD83, EPSG:32187 ou similaire) — en mètres, adaptée au Québec.
#   - Leaflet exige le système WGS84 (EPSG:4326) — latitude/longitude en degrés.
#   - st_transform(crs = 4326) convertit d'un CRS à l'autre.
#
# STRATÉGIE DE CHARGEMENT :
#   On vérifie d'abord si un fichier .rds simplifié existe.
#   .rds = format binaire R, beaucoup plus rapide à lire qu'un shapefile.
#   Si absent, on lit le shapefile complet et on le transforme.

champs_ua_candidats <- c("UA_TXT", "NO_UA", "UA", "ID_UA", "FMU")

if (file.exists(path_rds)) {
  # Cas 1 : fichier .rds pré-simplifié disponible (chargement rapide)
  ua_sf <- readRDS(path_rds) |>
    rename(ua = UA_TXT) |>           # Renommer la colonne UA pour uniformité
    mutate(ua = normaliser_ua(ua)) |>
    left_join(donnees_ua, by = "ua") # Joindre les attributs statistiques
  
} else if (dir.exists(path_shp_dir)) {
  # Cas 2 : lire le shapefile depuis le disque
  ua_shp <- sf::read_sf(dsn = path_shp_dir, layer = path_shp_layer)
  
  # Trouver automatiquement quelle colonne contient le code UA
  champ_ua_shp <- intersect(champs_ua_candidats, names(ua_shp))[1]
  if (is.na(champ_ua_shp)) {
    # Si aucun nom connu, prendre la première colonne texte
    champ_ua_shp <- names(ua_shp)[sapply(st_drop_geometry(ua_shp), is.character)][1]
  }
  
  ua_sf <- ua_shp |>
    st_make_valid() |>                              # Corriger les géométries invalides
    mutate(ua = normaliser_ua(.data[[champ_ua_shp]])) |> # .data[[x]] : accès à une colonne par son nom
    select(ua) |>                                   # Garder seulement l'ID + la géométrie
    st_transform(crs = 4326) |>                     # Convertir en WGS84 pour Leaflet
    left_join(donnees_ua, by = "ua")
  
} else {
  # Cas 3 : aucune géométrie disponible — la carte sera vide
  warning("Ni .rds ni shapefile trouvés. La carte sera vide.")
  ua_sf <- NULL
}


# =============================================================================
# 8. THÈME VISUEL DE L'APPLICATION
# =============================================================================
# bs_theme() configure l'apparence générale via Bootstrap 5.
# bootswatch = "flatly" : thème de base propre et professionnel.
# On surcharge les couleurs principales avec la palette forêt.
# font_google() charge une police depuis Google Fonts (nécessite internet).

theme_app <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#2C5F4E",  # Vert forêt sombre — couleur principale
  secondary    = "#A8B5A0",  # Vert sauge — couleur secondaire
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale   = 0.95        # Légèrement plus petit que la taille par défaut
)


# =============================================================================
# 9. FONCTIONS GRAPHIQUES (appelées depuis server.R)
# =============================================================================
# Ces fonctions sont définies dans global.R (et NON dans server.R) pour deux
# raisons importantes :
#   1. Elles doivent être disponibles avant que server() soit défini.
#   2. server.R doit se terminer EXACTEMENT sur la fermeture de server().
#      Toute fonction définie APRÈS le } final de server() serait interprétée
#      comme la fonction serveur par Shiny — ce qui cause une erreur.
#
# PIÈGE CLASSIQUE : définir des fonctions auxiliaires après server() dans
# server.R. Toujours les mettre dans global.R.

# --- Palette de couleurs par essence ----------------------------------------
# Les noms correspondent aux valeurs produites par noms_essences ci-dessous.
palette_essences <- c(
  "SEPM"                = "#2C5F4E",
  "Peupliers"           = "#A8B5A0",
  "Bouleau a papier"    = "#D4C5A0",
  "Bouleau jaune"       = "#C2A85F",
  "Erables"             = "#B85C3C",
  "Pins blanc & rouge"  = "#5C7A8C",
  "Thuya"               = "#6B8E5A",
  "Pruche"              = "#8B7355",
  "Autres feuillus durs"= "#999999"
)

# Correspondance nom de colonne → étiquette d'affichage
noms_essences <- c(
  "SEPM"                = "SEPM",
  "Peupliers"           = "Peupliers",
  "Bouleau_papier"      = "Bouleau a papier",
  "Bouleau_jaune"       = "Bouleau jaune",
  "Erables"             = "Erables",
  "Pins_blanc_rouge"    = "Pins blanc & rouge",
  "Thuya"               = "Thuya",
  "Pruche"              = "Pruche",
  "Autres_feuillus_durs"= "Autres feuillus durs"
)

# --- 9.1 Possibilité par essence --------------------------------------------
# Donut si 1 UA sélectionnée, barres empilées si plusieurs.
# df = data frame des UA sélectionnées (une ou plusieurs lignes)

graph_essences <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # pivot_longer() transforme le tableau de "large" à "long" :
  # Avant : une colonne par essence (SEPM, Thuya, ...)
  # Après : deux colonnes (essence, volume) — une ligne par essence par UA
  long <- df |>
    select(ua, all_of(names(noms_essences))) |>
    pivot_longer(-ua, names_to = "essence_raw", values_to = "vol") |>
    filter(!is.na(vol), vol > 0) |>
    mutate(essence = noms_essences[essence_raw]) # Remplacer le nom de colonne par l'étiquette
  
  if (nrow(long) == 0) return(NULL)
  
  if (nrow(df) == 1) {
    # UNE seule UA : graphique en anneau (donut)
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
        # annotations : texte au centre du donut
        annotations = list(text = paste0("<b>", fmt_nb(sum(agg$vol)), "</b><br>m\u00b3/an"),
                           showarrow = FALSE, font = list(size = 13))
      )
  } else {
    # PLUSIEURS UA : barres empilées pour comparer les profils par essence
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

# --- 9.2 Répartition des superficies ----------------------------------------
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

# --- 9.3 Travaux sylvicoles --------------------------------------------------
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

# --- 9.5 Camembert : % de possibilité par essence (toutes UA agrégées) ------
# Cette fonction agrège TOUTES les UA sélectionnées en une seule série,
# puis calcule le % de chaque essence sur le total.
# Utile pour répondre à la question : "quelle proportion de la récolte
# prévue est du SEPM ?" indépendamment du nombre d'UA choisies.
#
# Différence avec graph_essences() :
#   graph_essences()   → comparaison entre UA (barres ou donut si 1 UA)
#   graph_pie_essences() → toujours un camembert, toujours agrégé

graph_pie_essences <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Sommer les volumes par essence sur TOUTES les UA sélectionnées
  # colSums() calcule la somme de chaque colonne d'un data frame numérique
  cols <- names(noms_essences)  # Noms des colonnes d'essences dans donnees_ua
  
  agg <- df |>
    select(all_of(cols)) |>   # Garder seulement les colonnes d'essences
    colSums(na.rm = TRUE) |>  # Sommer chaque colonne → vecteur nommé
    as.data.frame() |>        # Convertir en data frame
    rownames_to_column("essence_raw") |>  # La colonne de noms devient une variable
    rename(vol = 2) |>
    filter(vol > 0) |>
    mutate(
      essence = noms_essences[essence_raw],  # Étiquette lisible
      pct     = round(vol / sum(vol) * 100, 1)  # % sur le total
    )
  
  if (nrow(agg) == 0) return(NULL)
  
  plot_ly(
    agg,
    labels = ~essence,
    values = ~vol,
    type   = "pie",
    sort   = TRUE,   # Trier par valeur décroissante
    marker = list(
      colors = palette_essences[agg$essence],
      line   = list(color = "white", width = 1.5)
    ),
    # textinfo : quoi afficher sur les parts du camembert
    # "percent" seulement pour ne pas surcharger les petites parts
    textinfo      = "percent",
    textposition  = "inside",
    hovertemplate = "<b>%{label}</b><br>%{value:,.0f}\u00a0m\u00b3/an<br>%{percent}<extra></extra>"
  ) |>
    layout(
      showlegend  = TRUE,
      legend      = list(orientation = "v", x = 1, y = 0.5,
                         font = list(size = 11)),
      margin      = list(t = 5, b = 5, l = 5, r = 120)
    )
}


# --- 9.6 Camembert : part de la possibilité totale par UA ------------------
# Répond à : "quelle UA contribue le plus à la possibilité totale
# de la sélection ?" Utile quand on compare plusieurs UA de tailles très
# différentes — une grande UA peut dominer les agrégats provinciaux.

graph_pie_ua <- function(df) {
  if (is.null(df) || nrow(df) == 0 || nrow(df) < 2) return(NULL)
  
  agg <- df |>
    select(ua, Possibilite_totale_m3an) |>
    filter(!is.na(Possibilite_totale_m3an)) |>
    mutate(pct = round(Possibilite_totale_m3an / sum(Possibilite_totale_m3an) * 100, 1))
  
  # Palette automatique : une couleur par UA, dérivée de la palette verte
  # colorRampPalette() interpolle entre des couleurs pour en créer autant que nécessaire
  n_ua     <- nrow(agg)
  couleurs <- colorRampPalette(c("#2C5F4E", "#6B8E5A", "#A8B5A0", "#D4C5A0", "#C2A85F"))(n_ua)
  
  plot_ly(
    agg,
    labels = ~ua,
    values = ~Possibilite_totale_m3an,
    type   = "pie",
    sort   = TRUE,
    marker = list(colors = couleurs, line = list(color = "white", width = 1.5)),
    textinfo      = "percent",
    textposition  = "inside",
    hovertemplate = "<b>UA\u00a0%{label}</b><br>%{value:,.0f}\u00a0m\u00b3/an<br>%{percent}<extra></extra>"
  ) |>
    layout(
      showlegend = TRUE,
      legend     = list(orientation = "v", x = 1, y = 0.5,
                        font = list(size = 11)),
      margin     = list(t = 5, b = 5, l = 5, r = 120)
    )
}