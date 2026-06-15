# =============================================================================
# server.R
# Possibilités forestières du Québec — SYN-00624
# =============================================================================
#
# RÔLE DE CE FICHIER
# -------------------
# server.R contient la LOGIQUE RÉACTIVE de l'application.
# Il répond aux actions de l'usager (clics, sélections) et met à jour
# les outputs (graphiques, tableaux, textes).
#
# RÈGLE ABSOLUE : server.R doit contenir exactement une expression au niveau
# racine — la fonction server(). RIEN ne doit être défini après son }.
# Si vous définissez des fonctions après, Shiny les interprète comme étant
# la fonction serveur et cela cause une erreur. C'est pourquoi toutes les
# fonctions auxiliaires sont dans global.R.
#
# LA RÉACTIVITÉ SHINY — CONCEPTS FONDAMENTAUX
# ---------------------------------------------
# Shiny fonctionne comme un tableur : quand une cellule change, toutes les
# cellules qui en dépendent se recalculent automatiquement.
#
# Les objets réactifs principaux :
#
#   reactive({})       Un calcul qui se refait automatiquement quand ses
#                      dépendances changent. Retourne une valeur.
#                      On l'appelle avec df_sel() (parenthèses obligatoires).
#
#   reactiveVal()      Une variable réactive modifiable manuellement.
#                      ua_sel <- reactiveVal(character(0)) crée une variable
#                      initialisée à un vecteur vide.
#                      Pour lire : ua_sel()
#                      Pour modifier : ua_sel(nouvelle_valeur)
#
#   observe({})        Réagit aux changements sans retourner de valeur.
#                      Utilisé pour les effets de bord (ex. mettre à jour la carte).
#                      Se déclenche automatiquement dès qu'une dépendance change.
#
#   observeEvent(x, {}) Réagit SEULEMENT quand x change (ou est cliqué).
#                       Plus prévisible que observe() pour les boutons et clics.
#                       Le premier argument est le déclencheur, le second le code.
#
#   renderXxx({})      Produit un output (graphique, tableau, UI...).
#                      render et output$id vont toujours ensemble :
#                        output$mon_graphique <- renderPlotly({ ... })
#                        output$mon_tableau   <- renderDT({ ... })
#                        output$mon_ui        <- renderUI({ ... })


# Opérateur utilitaire : a %||% b retourne b si a est NULL ou vide
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


server <- function(input, output, session) {
  # Note : les arguments input, output, session sont fournis automatiquement
  # par Shiny. Ne pas les modifier directement.
  # - input   : liste en lecture seule des valeurs actuelles des inputs UI
  # - output  : liste dans laquelle on place les résultats des render*()
  # - session : informations sur la session courante (rarement utilisé directement)
  
  
  # ===========================================================================
  # 1. GESTION DE LA SÉLECTION D'UA
  # ===========================================================================
  
  # reactiveVal : variable réactive qui stocke le vecteur des UA sélectionnées.
  # character(0) = vecteur texte vide (aucune UA au départ).
  # Quand ua_sel() change, TOUS les observe/renderXxx qui la lisent se réexécutent.
  ua_sel <- reactiveVal(character(0))
  
  
  # --- Carte de sélection : rendu initial ------------------------------------
  # renderLeaflet() crée la carte UNE SEULE FOIS.
  # Les mises à jour suivantes utilisent leafletProxy() pour modifier la carte
  # existante sans la recréer entièrement (beaucoup plus efficace).
  output$carte_selection_fiche <- renderLeaflet({
    
    # Carte de base : fond CartoDB clair, centré sur le Québec
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 4)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -72, lat = 50, zoom = 5)
    
    # Ajouter les polygones des UA si la géométrie est disponible
    if (!is.null(ua_sf) && nrow(ua_sf) > 0) {
      m <- m |>
        addPolygons(
          data        = ua_sf,
          fillColor   = "#A8B5A0",   # Vert sauge — état "non sélectionné"
          fillOpacity = 0.45,
          color       = "white",     # Couleur de la bordure
          weight      = 0.6,         # Épaisseur de la bordure
          layerId     = ~ua,         # ID unique par polygone — crucial pour les clics !
          # ~ signifie "prendre la valeur de la colonne ua"
          label       = ~paste0("UA ", ua, " — Région ", region),
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#1a3a30", bringToFront = TRUE
          )
        )
    }
    m  # Retourner la carte (toujours la dernière ligne d'un renderXxx)
  })
  
  
  # --- Recoloration des polygones selon la sélection -------------------------
  # observe() se déclenche automatiquement quand ua_sel() change.
  # leafletProxy() modifie la carte existante sans la recréer.
  observe({
    if (is.null(ua_sf) || nrow(ua_sf) == 0) return()
    
    sel <- ua_sel()  # Lire la sélection actuelle (vecteur de codes UA)
    
    # ifelse() vectorisé : pour chaque UA, choisir la couleur selon son état
    leafletProxy("carte_selection_fiche", data = ua_sf) |>
      clearShapes() |>  # Effacer les polygones actuels
      addPolygons(
        fillColor   = ifelse(ua_sf$ua %in% sel, "#2C5F4E", "#A8B5A0"),
        fillOpacity = ifelse(ua_sf$ua %in% sel, 0.82, 0.45),
        color       = ifelse(ua_sf$ua %in% sel, "#1a3a30", "white"),
        weight      = ifelse(ua_sf$ua %in% sel, 2.0, 0.6),
        layerId     = ~ua,
        label       = ~paste0("UA ", ua, " — Région ", region),
        highlightOptions = highlightOptions(weight = 2.5, color = "#1a3a30",
                                            bringToFront = TRUE)
      )
  })
  
  
  # --- Clic sur un polygone : toggle de la sélection -------------------------
  # input$carte_selection_fiche_shape_click est généré automatiquement par Leaflet
  # quand l'usager clique sur un polygone dont le layerId est défini.
  # La valeur contient $id (le layerId cliqué), $lat et $lng.
  observeEvent(input$carte_selection_fiche_shape_click, {
    uid <- input$carte_selection_fiche_shape_click$id
    if (is.null(uid)) return()
    
    sel <- ua_sel()
    
    if (uid %in% sel) {
      # UA déjà sélectionnée → la retirer (désélectionner)
      ua_sel(sel[sel != uid])
    } else {
      # Nouvelle UA → l'ajouter à la fin du vecteur
      ua_sel(c(sel, uid))
    }
  })
  
  
  
  # --- Bouton "Tout le Québec" : sélectionner toutes les UA -----------------
  # sort(unique(...)) : récupère tous les codes UA sans doublons, triés.
  # On sélectionne depuis donnees_ua (la table attributaire) plutôt que ua_sf
  # (la table spatiale) car donnees_ua est toujours disponible, même si le
  # shapefile est absent.
  observeEvent(input$fiche_tout, {
    toutes_ua <- sort(unique(donnees_ua$ua))
    ua_sel(toutes_ua)
  })
  
  # --- Bouton "Effacer" : vider toute la sélection ---------------------------
  observeEvent(input$fiche_clear, {
    ua_sel(character(0))  # Remettre le vecteur vide
  })
  
  
  # --- Bouton × sur une pastille : retirer une UA spécifique -----------------
  # input$fiche_remove_ua est déclenché par un onclick JavaScript dans renderUI.
  # Shiny.setInputValue() côté JS → observeEvent côté R : communication JS→R.
  observeEvent(input$fiche_remove_ua, {
    uid <- input$fiche_remove_ua
    ua_sel(ua_sel()[ua_sel() != uid])  # Garder toutes les UA sauf celle cliquée
  })
  
  
  # --- Pastilles des UA sélectionnées ----------------------------------------
  # renderUI() génère du HTML dynamiquement selon la sélection.
  # Chaque pastille contient un × cliquable qui déclenche fiche_remove_ua.
  output$fiche_ua_tags <- renderUI({
    sel    <- ua_sel()
    n_sel  <- length(sel)
    n_total <- nrow(donnees_ua)
    
    if (n_sel == 0) {
      return(tags$span(style = "color:#bbb; font-size:0.8em;",
                       "Aucune UA sélectionnée"))
    }
    
    # CAS 1 : toutes les UA sont sélectionnées (ou presque — seuil à 90%)
    # → une seule pastille "Tout le Québec" pour éviter les 57 pastilles
    if (n_sel >= n_total) {
      return(tags$span(
        style = paste0(
          "display:inline-flex; align-items:center; gap:6px;",
          "background:#2C5F4E; color:white; border-radius:12px;",
          "padding:3px 12px; font-size:0.82em; font-weight:600;"
        ),
        paste0("Tout le Qu\u00e9bec (", n_sel, "\u00a0UA)"),
        # Le × vide toute la sélection via le bouton Effacer existant
        tags$span(
          "\u00d7",
          style   = "cursor:pointer; opacity:0.8; font-size:1.2em; line-height:1;",
          onclick = "Shiny.setInputValue('fiche_clear_js', Date.now(), {priority:'event'})"
        )
      ))
    }
    
    # CAS 2 : sélection partielle mais nombreuse (> 10 UA)
    # → résumé compact pour éviter une barre surchargée
    if (n_sel > 10) {
      return(tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        tags$span(
          style = paste0(
            "display:inline-flex; align-items:center; gap:6px;",
            "background:#2C5F4E; color:white; border-radius:12px;",
            "padding:3px 12px; font-size:0.82em; font-weight:600;"
          ),
          paste0(n_sel, "\u00a0UA s\u00e9lectionn\u00e9es"),
          tags$span(
            "\u00d7",
            style   = "cursor:pointer; opacity:0.8; font-size:1.2em; line-height:1;",
            onclick = "Shiny.setInputValue('fiche_clear_js', Date.now(), {priority:'event'})"
          )
        ),
        # Afficher les 3 premières UA pour donner un repère
        tags$span(
          style = "font-size:0.78em; color:#888;",
          paste0("(", paste(head(sel, 3), collapse = ", "), "\u2026)")
        )
      ))
    }
    
    # CAS 3 : sélection de 10 UA ou moins → pastilles individuelles habituelles
    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:4px;",
      lapply(sel, function(uid) {
        tags$span(
          style = paste0(
            "display:inline-flex; align-items:center; gap:4px;",
            "background:#2C5F4E; color:white; border-radius:12px;",
            "padding:2px 10px; font-size:0.78em; font-weight:600;"
          ),
          uid,
          tags$span(
            "\u00d7",
            style   = "cursor:pointer; opacity:0.75; font-size:1.1em;",
            onclick = paste0(
              "Shiny.setInputValue('fiche_remove_ua','", uid, "',{priority:'event'})"
            )
          )
        )
      })
    )
  })
  
  # fiche_clear_js : déclenché par le × des pastilles "Tout le Québec" et "> 10 UA"
  # Même effet que le bouton "Effacer" — vide toute la sélection
  observeEvent(input$fiche_clear_js, {
    ua_sel(character(0))
  })
  
  
  
  # ===========================================================================
  # 2. DONNÉES RÉACTIVES DES UA SÉLECTIONNÉES
  # ===========================================================================
  
  # reactive() : calcul qui se refait automatiquement quand ua_sel() change.
  # Filtre donnees_ua pour ne garder que les UA sélectionnées.
  # Retourne NULL si aucune UA n'est sélectionnée.
  df_sel <- reactive({
    ids <- ua_sel()
    if (length(ids) == 0) return(NULL)
    donnees_ua |> filter(ua %in% ids)
    # %in% : opérateur "est dans" — TRUE si ua est dans le vecteur ids
  })
  
  
  # ===========================================================================
  # 3. CONTENU PRINCIPAL DE LA FICHE UA
  # ===========================================================================
  # renderUI() génère toute l'interface sous la carte selon la sélection.
  # C'est une approche puissante : l'UI elle-même est réactive.
  # Inconvénient : les outputs Plotly et DT à l'intérieur (fiche_graph_ess, etc.)
  # doivent aussi être définis dans server.R comme des outputs classiques.
  
  output$fiche_contenu <- renderUI({
    d <- df_sel()
    n <- if (is.null(d)) 0L else nrow(d)
    
    # Message d'invite si aucune UA sélectionnée
    if (n == 0) {
      return(tags$div(
        style = "display:flex; flex-direction:column; align-items:center; justify-content:center; padding:56px; color:#bbb; gap:12px;",
        bsicons::bs_icon("hand-index-thumb", size = "2.5em"),
        tags$p(style = "font-size:0.95em; margin:0;",
               "Cliquez sur une ou plusieurs UA dans la carte pour afficher l'analyse.")
      ))
    }
    
    # Titre dynamique selon le nombre d'UA
    titre <- if (n == 1) {
      paste0("UA ", d$ua, " \u2014 Région ", d$region)
    } else {
      paste0(n, " UA sélectionnées : ", paste(d$ua, collapse = ", "))
    }
    
    # ------------------------------------------------------------------
    # Calcul des KPI (indicateurs résumés)
    # ------------------------------------------------------------------
    # Pour les sommes : on additionne les valeurs de toutes les UA (budget, ha, m³)
    # Pour les moyennes : on prend la moyenne des UA sélectionnées
    # Pour le % destiné à l'aménagement : moyenne PONDÉRÉE par la superficie totale
    #   → une grande UA pèse plus qu'une petite dans la moyenne
    #   → weighted.mean(valeurs, poids) fait ce calcul automatiquement
    
    pct_moy <- weighted.mean(d$pct_destinee_amenagement, d$sup_totale_ha, na.rm = TRUE)
    
    # Groupe TERRITOIRE : superficies et volume sur pied
    kpi_ter <- list(
      list("Superficie totale",          fmt_ha(sum(d$sup_totale_ha,               na.rm = TRUE)), ""),
      list("Superficie destinée amén.",  fmt_ha(sum(d$sup_destinee_amenagement_ha, na.rm = TRUE)), ""),
      list("% destinée à l'amén.",       paste0(round(pct_moy * 100, 1), "\u00a0%"),              "moy. pond."),
      list("Vol. sur pied total",        fmt_m3(sum(d$volume_sur_pied_m3,          na.rm = TRUE)), "brut")
    )
    
    # Groupe RÉCOLTE : possibilité et variables dendrométriques clés
    kpi_rec <- list(
      list("Possibilité totale",         fmt_m3(sum(d$Possibilite_totale_m3an, na.rm = TRUE)),        "/an"),
      list("SEPM",                       fmt_m3(sum(d$SEPM,                    na.rm = TRUE)),        "/an"),
      list("Taux de récolte moy.",       paste0(round(mean(d$taux_recolte_pct,       na.rm=TRUE),1), "\u00a0%"), "du vol. sur pied"),
      list("Productivité moy.",          paste0(round(mean(d$productivite_m3hanan,   na.rm=TRUE),2), "\u00a0m\u00b3/ha/an"), "")
    )
    
    # Groupe TRAVAUX SYLVICOLES ET BUDGET
    kpi_syl <- list(
      list("Coupes totales",  paste0(fmt_nb(sum(d$coupes_totales_haan, na.rm=TRUE)), "\u00a0ha/an"), ""),
      list("Plantations",     paste0(fmt_nb(sum(d$plantations_haan,    na.rm=TRUE)), "\u00a0ha/an"), ""),
      list("Budget total",    fmt_dol(sum(d$budget_an,   na.rm=TRUE)),                               "/an"),
      list("Budget ($/m\u00b3)", paste0(round(sum(d$budget_an, na.rm=TRUE) / sum(d$Possibilite_totale_m3an, na.rm=TRUE), 2), "\u00a0$/m\u00b3"), "total budget \u00f7 total possibilit\u00e9")
    )
    
    # ------------------------------------------------------------------
    # Fonction locale : générer une grille de KPI
    # ------------------------------------------------------------------
    # Définie ici (dans renderUI) car elle n'est utile que pour ce bloc.
    # grid-template-columns:repeat(auto-fill,minmax(175px,1fr)) = CSS Grid :
    # autant de colonnes que possible, chacune d'au moins 175px.
    kpi_bloc <- function(items) {
      tags$div(
        style = "display:grid; grid-template-columns:repeat(auto-fill,minmax(175px,1fr)); gap:10px;",
        lapply(items, function(x) {
          tags$div(
            style = "padding:10px 14px; border-left:3px solid #2C5F4E; background:#f8faf9; border-radius:0 4px 4px 0;",
            tags$div(style = "color:#888; font-size:0.78em; margin-bottom:2px;", x[[1]]),
            tags$div(style = "font-size:1.15em; font-weight:700; color:#2C5F4E; line-height:1.2;", x[[2]]),
            tags$div(style = "color:#aaa; font-size:0.75em;", x[[3]])
          )
        })
      )
    }
    
    # Fonction locale : en-tête de section avec icône
    sec <- function(icon_name, titre_txt) {
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin:20px 0 10px; border-bottom:1px solid #e0ece8; padding-bottom:6px;",
        bsicons::bs_icon(icon_name, style = "color:#2C5F4E;"),
        tags$span(titre_txt, style = "font-weight:600; color:#2C5F4E; font-size:1.02em;")
      )
    }
    
    # ------------------------------------------------------------------
    # Construction de l'interface complète
    # tagList() regroupe plusieurs éléments HTML en une seule liste
    # layout_columns() = grille de colonnes bslib (col_widths en unités sur 12)
    # ------------------------------------------------------------------
    tagList(
      
      # En-tête identifiant les UA analysées
      tags$div(
        style = "padding:10px 16px; background:#f0f4f2; border-left:4px solid #2C5F4E; border-radius:4px; margin-bottom:16px; font-weight:600; color:#2C5F4E;",
        bsicons::bs_icon("geo-alt-fill"), " ", titre
      ),
      
      # ---- SECTION TERRITOIRE ----
      sec("map", "Territoire"),
      kpi_bloc(kpi_ter),
      layout_columns(
        col_widths = c(7, 5),  # 7/12 pour le graphique, 5/12 pour le tableau
        card(
          card_header(tags$strong("Répartition des superficies (ha)")),
          # plotlyOutput est un "placeholder" — rempli par output$fiche_graph_sup
          plotlyOutput("fiche_graph_sup", height = "260px")
        ),
        card(
          card_header(tags$strong("Dendrométrie — moyennes")),
          # uiOutput rempli par output$fiche_dendro_table
          uiOutput("fiche_dendro_table")
        )
      ),
      
      
      # ---- SECTION RÉCOLTE PRÉVUE ----
      sec("tree", "Récolte prévue (possibilités forestières)"),
      kpi_bloc(kpi_rec),
      # layout_columns : côte à côte sur 12 colonnes Bootstrap
      # col_widths = c(8, 4) → graphique barre/donut à gauche, camembert à droite
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header(tags$strong("Possibilit\u00e9 par essence (m\u00b3/an)")),
          plotlyOutput("fiche_graph_ess", height = "300px")
        ),
        card(
          card_header(tags$strong("R\u00e9partition par essence (%)")),
          # Ce graphique agrège TOUJOURS toutes les UA en un seul camembert,
          # peu importe le nombre d'UA sélectionnées.
          plotlyOutput("fiche_pie_ess", height = "300px")
        )
      ),
      
      
      # ---- SECTION TRAVAUX SYLVICOLES ----
      sec("scissors", "Travaux sylvicoles"),
      kpi_bloc(kpi_syl),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(tags$strong("Traitements (ha/an)")),
          plotlyOutput("fiche_graph_syl", height = "260px")
        ),
        card(
          card_header(tags$strong("Budget sylvicole")),
          uiOutput("fiche_budget_table")
        )
      ),
      
      # ---- TABLEAU COMPARATIF (2+ UA seulement) ----
      if (n >= 2) {
        tagList(
          sec("table", "Tableau comparatif"),
          card(uiOutput("fiche_tableau_comp"))
        )
      },
      
      tags$div(style = "height:24px;")  # Marge inférieure
    )
  }) # fin renderUI fiche_contenu
  
  
  # ===========================================================================
  # 4. OUTPUTS GRAPHIQUES STABLES
  # ===========================================================================
  # Ces outputs ont des IDs FIXES (pas générés dynamiquement).
  # Même si le plotlyOutput est à l'intérieur d'un renderUI, le outputId doit
  # être défini ici — Shiny fait le lien automatiquement.
  #
  # df_sel() est la dépendance commune : quand la sélection change,
  # tous ces graphiques se recalculent.
  
  output$fiche_graph_sup    <- renderPlotly({ graph_superficies(df_sel()) })
  output$fiche_graph_ess    <- renderPlotly({ graph_essences(df_sel()) })
  output$fiche_graph_syl    <- renderPlotly({ graph_sylvicole(df_sel()) })

  # Camembert 1 : % de possibilité par essence, toutes UA agrégées
  # graph_pie_essences() est définie dans global.R
  output$fiche_pie_ess <- renderPlotly({ graph_pie_essences(df_sel()) })
  
  # Camembert 2 : part de la possibilité totale par UA (seulement si 2+ UA)
  # Montre le "poids" de chaque UA dans la sélection
  output$fiche_pie_ua  <- renderPlotly({ graph_pie_ua(df_sel()) })
  
  
  
  # ===========================================================================
  # 5. MINI-TABLEAUX D'INDICATEURS (renderUI car contenu textuel simple)
  # ===========================================================================
  
  # Tableau dendrométrie : liste de paires label / valeur
  output$fiche_dendro_table <- renderUI({
    d <- df_sel()
    if (is.null(d)) return(NULL)
    
    # mean() avec na.rm = TRUE : ignorer les NA dans le calcul
    rows <- list(
      list("Âge moy. de récolte",    paste0(round(mean(d$age_moyen_recolte_an,          na.rm=TRUE),1), " ans")),
      list("Vol. moy. récolté",      paste0(round(mean(d$volume_moyen_recolte_m3ha,     na.rm=TRUE),1), " m\u00b3/ha")),
      list("Surface terrière",       paste0(round(mean(d$surface_terriere_m2ha,          na.rm=TRUE),1), " m\u00b2/ha")),
      list("Délai entre interv.",    paste0(round(mean(d$delai_entre_interv_an,          na.rm=TRUE),1), " ans")),
      list("Prélèvement moy.",       paste0(round(mean(d$prelevement_moyen_cpf2025_m3ha,na.rm=TRUE),1), " m\u00b3/ha")),
      list("Dim. bois SEPM",         paste0(round(mean(d$dimension_bois_sepm_dcm3tige,  na.rm=TRUE),1), " dcm\u00b3/tige"))
    )
    
    tags$div(
      style = "padding:8px 4px;",
      lapply(rows, function(r) {
        tags$div(
          style = "display:flex; justify-content:space-between; padding:5px 10px; border-bottom:1px solid #f0f0f0; font-size:0.88em;",
          tags$span(style = "color:#666;", r[[1]]),
          tags$strong(style = "color:#2C5F4E;", r[[2]])
        )
      })
    )
  })
  
  # Tableau budget
  output$fiche_budget_table <- renderUI({
    d <- df_sel()
    if (is.null(d)) return(NULL)
    
    rows <- list(
      list("Budget total (/an)",      fmt_dol(sum(d$budget_an,                na.rm=TRUE))),
      list("$/ha (agr\u00e9g\u00e9)", paste0(round(sum(d$budget_an, na.rm=TRUE) / sum(d$sup_destinee_amenagement_ha, na.rm=TRUE), 2), " $/ha")),
      list("$/m\u00b3 (agr\u00e9g\u00e9)", paste0(round(sum(d$budget_an, na.rm=TRUE) / sum(d$Possibilite_totale_m3an, na.rm=TRUE), 2), " $/m\u00b3")),
      list("Plantations",             paste0(fmt_nb(sum(d$plantations_haan,   na.rm=TRUE)),   " ha/an")),
      list("Éducation",               paste0(fmt_nb(sum(d$education_haan,     na.rm=TRUE)),   " ha/an")),
      list("Préparation terrain",     paste0(fmt_nb(sum(d$preparation_terrain_haan, na.rm=TRUE)), " ha/an"))
    )
    
    tags$div(
      style = "padding:8px 4px;",
      lapply(rows, function(r) {
        tags$div(
          style = "display:flex; justify-content:space-between; padding:5px 10px; border-bottom:1px solid #f0f0f0; font-size:0.88em;",
          tags$span(style = "color:#666;", r[[1]]),
          tags$strong(style = "color:#2C5F4E;", r[[2]])
        )
      })
    )
  })
  
  
  # ===========================================================================
  # 6. TABLEAU COMPARATIF MULTI-UA
  # ===========================================================================
  # POURQUOI renderUI et non renderDT ?
  # renderDT (DT::datatable) ne fonctionne pas de manière fiable quand son
  # DTOutput est généré DYNAMIQUEMENT à l'intérieur d'un renderUI.
  # C'est une limitation connue de Shiny : les outputs JS complexes (DT, Plotly)
  # ont besoin d'être présents dans le DOM au chargement pour s'initialiser.
  # Solution : générer un tableau HTML simple avec tags$table — toujours fiable.
  #
  # Cette approche utilise du HTML pur via les fonctions tags$table, tags$tr,
  # tags$th (en-tête), tags$td (cellule). C'est l'équivalent de :
  # <table><tr><th>Colonne</th></tr><tr><td>Valeur</td></tr></table>
  
  output$fiche_tableau_comp <- renderUI({
    d <- df_sel()
    if (is.null(d) || nrow(d) < 2) return(NULL)
    
    # Définir les lignes du tableau : chaque élément = une variable à comparer.
    # Format : list(label affiché, expression R qui calcule la valeur par UA)
    # On crée une ligne par indicateur, avec une colonne par UA.
    indicateurs <- list(
      list("Région",                  function(x) x$region),
      list("Sup. totale (ha)",        function(x) fmt_nb(x$sup_totale_ha)),
      list("% destinée amén.",        function(x) paste0(round(x$pct_destinee_amenagement * 100, 1), "\u00a0%")),
      list("Possibilité totale (m³/an)", function(x) fmt_nb(x$Possibilite_totale_m3an)),
      list("SEPM (m³/an)",            function(x) fmt_nb(x$SEPM)),
      list("Peupliers (m³/an)",       function(x) fmt_nb(x$Peupliers)),
      list("Taux de récolte (%)",     function(x) paste0(round(x$taux_recolte_pct, 1), "\u00a0%")),
      list("Productivité (m³/ha/an)", function(x) round(x$productivite_m3hanan, 2)),
      list("Âge moy. récolte (an)",   function(x) round(x$age_moyen_recolte_an, 1)),
      list("Vol. sur pied (m³)",      function(x) fmt_nb(x$volume_sur_pied_m3)),
      list("Surface terrière (m²/ha)",function(x) round(x$surface_terriere_m2ha, 1)),
      list("Budget $/an",             function(x) fmt_dol(x$budget_an)),
      list("Budget $/ha",             function(x) paste0(round(x$budget_par_ha, 2), "\u00a0$/ha")),
      list("Budget $/m³",             function(x) paste0(round(x$budget_par_m3, 2), "\u00a0$/m\u00b3")),
      list("Coupes totales (ha/an)",  function(x) fmt_nb(x$coupes_totales_haan)),
      list("Plantations (ha/an)",     function(x) fmt_nb(x$plantations_haan))
    )
    
    # Style CSS partagé pour les cellules
    style_th <- "padding:7px 12px; background:#2C5F4E; color:white; font-weight:600; font-size:0.85em; white-space:nowrap; text-align:left;"
    style_td <- "padding:6px 12px; font-size:0.85em; border-bottom:1px solid #f0f0f0; white-space:nowrap;"
    style_label <- "padding:6px 12px; font-size:0.85em; color:#555; border-bottom:1px solid #f0f0f0; white-space:nowrap; background:#fafafa; font-weight:500;"
    
    # Construire le tableau HTML :
    # - En-tête : "Indicateur" + un code UA par colonne
    # - Corps : une ligne par indicateur, valeur calculée pour chaque UA
    tags$div(
      style = "overflow-x:auto;",  # Défilement horizontal si beaucoup d'UA
      tags$table(
        style = "border-collapse:collapse; width:100%; font-family:inherit;",
        
        # En-tête du tableau
        tags$thead(
          tags$tr(
            tags$th("Indicateur", style = style_th),
            # lapply sur chaque UA pour créer une colonne par UA
            lapply(d$ua, function(uid) {
              tags$th(paste0("UA\u00a0", uid), style = style_th)
            })
          )
        ),
        
        # Corps : une ligne par indicateur
        tags$tbody(
          lapply(indicateurs, function(ind) {
            label  <- ind[[1]]   # Étiquette de la ligne
            calcul <- ind[[2]]   # Fonction qui extrait la valeur
            
            tags$tr(
              tags$td(label, style = style_label),
              # Pour chaque UA, appliquer la fonction sur la ligne correspondante
              lapply(seq_len(nrow(d)), function(i) {
                valeur <- tryCatch(
                  as.character(calcul(d[i, ])),  # d[i, ] = ligne i du data frame
                  error = function(e) "\u2014"    # "\u2014" = tiret — si erreur
                )
                tags$td(valeur, style = style_td)
              })
            )
          })
        )
      )
    )
  })
  
  
  
  # ===========================================================================
  # 7. TABLEAU COMPLET (onglet Données)
  # ===========================================================================
  output$tableau_complet <- renderDT({
    datatable(
      # Convertir pct_destinee_amenagement de proportion en % pour l'affichage
      donnees_ua |>
        mutate(pct_destinee_amenagement = round(pct_destinee_amenagement * 100, 1)),
      rownames   = FALSE,
      filter     = "top",           # Filtres en haut de chaque colonne
      extensions = c("Scroller", "FixedColumns"),
      options    = list(
        dom          = "tip",
        scrollX      = TRUE,
        scrollY      = "calc(100vh - 320px)",  # Hauteur dynamique selon la fenêtre
        scroller     = TRUE,                   # Chargement à la demande (performance)
        fixedColumns = list(leftColumns = 2),  # Figer les 2 premières colonnes
        pageLength   = 50
      )
    )
  })
  
  
  # ===========================================================================
  # 8. TÉLÉCHARGEMENT CSV
  # ===========================================================================
  # downloadHandler() : gère le téléchargement de fichiers.
  # filename : nom du fichier proposé à l'usager (fonction pour être dynamique)
  # content  : fonction qui écrit le contenu dans un fichier temporaire
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("possibilites-forestieres-UA_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(
        donnees_ua |>
          mutate(pct_destinee_amenagement = round(pct_destinee_amenagement * 100, 1)),
        file,
        row.names     = FALSE,
        fileEncoding  = "UTF-8"
      )
    }
  )
  
} # fin server()
# =============================================================================
# RAPPEL : rien ne doit être écrit après cette ligne dans server.R
# =============================================================================