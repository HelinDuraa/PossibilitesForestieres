# =============================================================================
# server.R — Possibilités forestières du Québec (SYN-00624)
# Rappel : server.R ne doit contenir QUE la fonction server().
# Toutes les fonctions auxiliaires sont dans global.R.
# =============================================================================

# Opérateur utilitaire : a %||% b retourne b si a est NULL ou vide
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


server <- function(input, output, session) {

  # ===========================================================================
  # SÉLECTION D'UA
  # ===========================================================================
  ua_sel <- reactiveVal(character(0))

  # Carte de base (créée une seule fois)
  output$carte_selection_fiche <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 4)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -72, lat = 50, zoom = 5)

    if (!is.null(ua_sf) && nrow(ua_sf) > 0) {
      m <- m |>
        addPolygons(
          data        = ua_sf,
          fillColor   = "#A8B5A0",
          fillOpacity = 0.45,
          color       = "white",
          weight      = 0.6,
          layerId     = ~ua,
          label       = ~paste0("UA ", ua, " — Région ", region),
          highlightOptions = highlightOptions(weight = 2.5, color = "#1a3a30",
                                              bringToFront = TRUE)
        )
    }
    m
  })

  # Recoloration selon la sélection
  observe({
    if (is.null(ua_sf) || nrow(ua_sf) == 0) return()
    sel <- ua_sel()
    leafletProxy("carte_selection_fiche", data = ua_sf) |>
      clearShapes() |>
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

  # Clic sur un polygone : ajouter ou retirer (bascule)
  observeEvent(input$carte_selection_fiche_shape_click, {
    uid <- input$carte_selection_fiche_shape_click$id
    if (is.null(uid)) return()
    sel <- ua_sel()
    if (uid %in% sel) ua_sel(sel[sel != uid]) else ua_sel(c(sel, uid))
  })

  # Boutons
  observeEvent(input$fiche_tout,  { ua_sel(sort(unique(donnees_ua$ua))) })
  observeEvent(input$fiche_clear, { ua_sel(character(0)) })
  observeEvent(input$fiche_remove_ua, {
    uid <- input$fiche_remove_ua
    ua_sel(ua_sel()[ua_sel() != uid])
  })

  # Pastilles des UA sélectionnées
  output$fiche_ua_tags <- renderUI({
    sel     <- ua_sel()
    n_sel   <- length(sel)
    n_total <- nrow(donnees_ua)

    if (n_sel == 0) {
      return(tags$span(style = "color:#bbb; font-size:0.8em;",
                       "Aucune UA sélectionnée"))
    }

    if (n_sel >= n_total) {
      return(tags$span(
        style = paste0("display:inline-flex; align-items:center; gap:6px;",
                       "background:#2C5F4E; color:white; border-radius:12px;",
                       "padding:3px 12px; font-size:0.82em; font-weight:600;"),
        paste0("Tout le Qu\u00e9bec (", n_sel, "\u00a0UA)"),
        tags$span("\u00d7",
          style   = "cursor:pointer; opacity:0.8; font-size:1.2em; line-height:1;",
          onclick = "Shiny.setInputValue('fiche_clear_js', Date.now(), {priority:'event'})")
      ))
    }

    if (n_sel > 10) {
      return(tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        tags$span(
          style = paste0("display:inline-flex; align-items:center; gap:6px;",
                         "background:#2C5F4E; color:white; border-radius:12px;",
                         "padding:3px 12px; font-size:0.82em; font-weight:600;"),
          paste0(n_sel, "\u00a0UA s\u00e9lectionn\u00e9es"),
          tags$span("\u00d7",
            style   = "cursor:pointer; opacity:0.8; font-size:1.2em; line-height:1;",
            onclick = "Shiny.setInputValue('fiche_clear_js', Date.now(), {priority:'event'})")
        ),
        tags$span(style = "font-size:0.78em; color:#888;",
                  paste0("(", paste(head(sel, 3), collapse = ", "), "\u2026)"))
      ))
    }

    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:4px;",
      lapply(sel, function(uid) {
        tags$span(
          style = paste0("display:inline-flex; align-items:center; gap:4px;",
                         "background:#2C5F4E; color:white; border-radius:12px;",
                         "padding:2px 10px; font-size:0.78em; font-weight:600;"),
          uid,
          tags$span("\u00d7",
            style   = "cursor:pointer; opacity:0.75; font-size:1.1em;",
            onclick = paste0("Shiny.setInputValue('fiche_remove_ua','", uid,
                             "',{priority:'event'})"))
        )
      })
    )
  })

  observeEvent(input$fiche_clear_js, { ua_sel(character(0)) })


  # ===========================================================================
  # DONNÉES DES UA SÉLECTIONNÉES
  # ===========================================================================
  df_sel <- reactive({
    ids <- ua_sel()
    if (length(ids) == 0) return(NULL)
    donnees_ua |> filter(ua %in% ids)
  })


  # ===========================================================================
  # CONTENU DE LA FICHE
  # ===========================================================================
  output$fiche_contenu <- renderUI({
    d <- df_sel()
    n <- if (is.null(d)) 0L else nrow(d)

    if (n == 0) {
      return(tags$div(
        style = "display:flex; flex-direction:column; align-items:center; justify-content:center; padding:56px; color:#bbb; gap:12px;",
        bsicons::bs_icon("hand-index-thumb", size = "2.5em"),
        tags$p(style = "font-size:0.95em; margin:0;",
               "Cliquez sur une ou plusieurs UA dans la carte pour afficher l'analyse.")
      ))
    }

    titre <- if (n == 1) {
      paste0("UA ", d$ua, " \u2014 Région ", d$region)
    } else {
      paste0(n, " UA sélectionnées : ", paste(d$ua, collapse = ", "))
    }

    # ---- Calcul des indicateurs (sans détails de calcul affichés) ----
    pct_moy <- weighted.mean(d$pct_destinee_amenagement, d$sup_totale_ha, na.rm = TRUE)

    # Volume sur pied : afficher NA plutôt que 0 si tout est manquant
    vsp <- if (all(is.na(d$volume_sur_pied_m3))) NA_real_
           else sum(d$volume_sur_pied_m3, na.rm = TRUE)

    kpi_ter <- list(
      list("Superficie totale",         fmt_ha(sum(d$sup_totale_ha, na.rm = TRUE))),
      list("Superficie destinée amén.", fmt_ha(sum(d$sup_destinee_amenagement_ha, na.rm = TRUE))),
      list("% destinée à l'amén.",      paste0(fmt_dec(pct_moy * 100, 1), "\u00a0%")),
      list("Vol. sur pied total",       if (is.na(vsp)) "NA" else fmt_m3(vsp))
    )

    kpi_rec <- list(
      list("Possibilité totale",      fmt_m3(sum(d$Possibilite_totale_m3an, na.rm = TRUE))),
      list("SEPM",                    fmt_m3(sum(d$SEPM, na.rm = TRUE))),
      list("Taux de récolte moy.",    paste0(fmt_dec(mean(d$taux_recolte_pct, na.rm = TRUE), 1), "\u00a0%")),
      list("Productivité moy.",       paste0(fmt_dec(mean(d$productivite_m3hanan, na.rm = TRUE), 2), "\u00a0m\u00b3/ha/an")),
      list("Dimension moy. des bois", paste0(fmt_dec(mean(d$dimension_bois_sepm_dcm3tige, na.rm = TRUE), 1), "\u00a0dcm\u00b3/tige"))
    )

    kpi_syl <- list(
      list("Coupes totales", paste0(fmt_nb(sum(d$coupes_totales_haan, na.rm = TRUE)), "\u00a0ha/an")),
      list("Plantations",    paste0(fmt_nb(sum(d$plantations_haan, na.rm = TRUE)), "\u00a0ha/an")),
      list("Budget total",   fmt_dol(sum(d$budget_an, na.rm = TRUE))),
      list("Budget ($/m\u00b3)", paste0(fmt_dec(sum(d$budget_an, na.rm = TRUE) / sum(d$Possibilite_totale_m3an, na.rm = TRUE), 2), "\u00a0$/m\u00b3"))
    )

    # Grille de KPI (label + valeur, sans ligne de détail de calcul)
    kpi_bloc <- function(items) {
      tags$div(
        style = "display:grid; grid-template-columns:repeat(auto-fill,minmax(175px,1fr)); gap:10px;",
        lapply(items, function(x) {
          tags$div(
            style = "padding:10px 14px; border-left:3px solid #2C5F4E; background:#f8faf9; border-radius:0 4px 4px 0;",
            tags$div(style = "color:#888; font-size:0.78em; margin-bottom:2px;", x[[1]]),
            tags$div(style = "font-size:1.15em; font-weight:700; color:#2C5F4E; line-height:1.2;", x[[2]])
          )
        })
      )
    }

    sec <- function(icon_name, titre_txt) {
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin:20px 0 10px; border-bottom:1px solid #e0ece8; padding-bottom:6px;",
        bsicons::bs_icon(icon_name, style = "color:#2C5F4E;"),
        tags$span(titre_txt, style = "font-weight:600; color:#2C5F4E; font-size:1.02em;")
      )
    }

    tagList(

      tags$div(
        style = "padding:10px 16px; background:#f0f4f2; border-left:4px solid #2C5F4E; border-radius:4px; margin-bottom:16px; font-weight:600; color:#2C5F4E;",
        bsicons::bs_icon("geo-alt-fill"), " ", titre
      ),

      # ---- TERRITOIRE ----
      sec("map", "Territoire"),
      kpi_bloc(kpi_ter),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(tags$strong("Répartition des superficies (ha)")),
          plotlyOutput("fiche_graph_sup", height = "260px")
        ),
        card(
          card_header(tags$strong("Dendrométrie — moyennes")),
          uiOutput("fiche_dendro_table")
        )
      ),

      # ---- RÉCOLTE PRÉVUE ----
      sec("tree", "Récolte prévue (possibilités forestières)"),
      kpi_bloc(kpi_rec),
      card(
        card_header(tags$strong("Possibilit\u00e9 par essence (m\u00b3/an)")),
        plotlyOutput("fiche_graph_ess", height = "300px")
      ),

      # ---- COMPARAISON ENTRE UA (2+ UA) ----
      if (n >= 2) {
        tagList(
          sec("bar-chart-line", "Comparaison entre UA"),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(tags$strong("Part de la possibilit\u00e9 totale par UA (%)")),
              plotlyOutput("fiche_pie_ua", height = "300px")
            )
          )
        )
      },

      # ---- TRAVAUX ET BUDGET SYLVICOLE ----
      sec("scissors", "Travaux et budget sylvicole"),
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

      # ---- TABLEAU COMPARATIF (2+ UA) ----
      if (n >= 2) {
        tagList(
          sec("table", "Tableau comparatif"),
          card(uiOutput("fiche_tableau_comp"))
        )
      },

      tags$div(style = "height:24px;")
    )
  })


  # ===========================================================================
  # OUTPUTS GRAPHIQUES
  # ===========================================================================
  output$fiche_graph_sup <- renderPlotly({ graph_superficies(df_sel()) })
  output$fiche_graph_ess <- renderPlotly({ graph_essences(df_sel()) })
  output$fiche_graph_syl <- renderPlotly({ graph_sylvicole(df_sel()) })
  output$fiche_pie_ua    <- renderPlotly({ graph_pie_ua(df_sel()) })


  # ===========================================================================
  # MINI-TABLEAUX
  # ===========================================================================
  output$fiche_dendro_table <- renderUI({
    d <- df_sel()
    if (is.null(d)) return(NULL)

    rows <- list(
      list("Âge moy. de récolte", paste0(fmt_dec(mean(d$age_moyen_recolte_an,        na.rm=TRUE), 1), " ans")),
      list("Vol. moy. récolté",   paste0(fmt_dec(mean(d$volume_moyen_recolte_m3ha,   na.rm=TRUE), 1), " m\u00b3/ha")),
      list("Surface terrière",    paste0(fmt_dec(mean(d$surface_terriere_m2ha,        na.rm=TRUE), 1), " m\u00b2/ha")),
      list("Délai entre interv.", paste0(fmt_dec(mean(d$delai_entre_interv_an,        na.rm=TRUE), 1), " ans")),
      list("Prélèvement moy.",    paste0(fmt_dec(mean(d$prelevement_moyen_cpf2025_m3ha, na.rm=TRUE), 1), " m\u00b3/ha"))
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

  output$fiche_budget_table <- renderUI({
    d <- df_sel()
    if (is.null(d)) return(NULL)

    rows <- list(
      list("Budget total (/an)", fmt_dol(sum(d$budget_an, na.rm=TRUE))),
      list("Budget ($/ha)",      paste0(fmt_dec(sum(d$budget_an, na.rm=TRUE) / sum(d$sup_destinee_amenagement_ha, na.rm=TRUE), 2), " $/ha")),
      list("Budget ($/m\u00b3)", paste0(fmt_dec(sum(d$budget_an, na.rm=TRUE) / sum(d$Possibilite_totale_m3an, na.rm=TRUE), 2), " $/m\u00b3")),
      list("Plantations",        paste0(fmt_nb(sum(d$plantations_haan,         na.rm=TRUE)), " ha/an")),
      list("Éducation",          paste0(fmt_nb(sum(d$education_haan,           na.rm=TRUE)), " ha/an")),
      list("Préparation terrain",paste0(fmt_nb(sum(d$preparation_terrain_haan, na.rm=TRUE)), " ha/an"))
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
  # TABLEAU COMPARATIF MULTI-UA
  # ===========================================================================
  output$fiche_tableau_comp <- renderUI({
    d <- df_sel()
    if (is.null(d) || nrow(d) < 2) return(NULL)

    indicateurs <- list(
      list("Région",                    function(x) x$region),
      list("Sup. totale (ha)",          function(x) fmt_nb(x$sup_totale_ha)),
      list("% destinée amén.",          function(x) paste0(fmt_dec(x$pct_destinee_amenagement * 100, 1), "\u00a0%")),
      list("Possibilité totale (m³/an)",function(x) fmt_nb(x$Possibilite_totale_m3an)),
      list("SEPM (m³/an)",              function(x) fmt_nb(x$SEPM)),
      list("Peupliers (m³/an)",         function(x) fmt_nb(x$Peupliers)),
      list("Taux de récolte (%)",       function(x) paste0(fmt_dec(x$taux_recolte_pct, 1), "\u00a0%")),
      list("Productivité (m³/ha/an)",   function(x) fmt_dec(x$productivite_m3hanan, 2)),
      list("Âge moy. récolte (an)",     function(x) fmt_dec(x$age_moyen_recolte_an, 1)),
      list("Dimension moy. bois (dcm³/tige)", function(x) fmt_dec(x$dimension_bois_sepm_dcm3tige, 1)),
      list("Vol. sur pied (m³)",        function(x) fmt_nb(x$volume_sur_pied_m3)),
      list("Surface terrière (m²/ha)",  function(x) fmt_dec(x$surface_terriere_m2ha, 1)),
      list("Budget $/an",               function(x) fmt_dol(x$budget_an)),
      list("Budget $/ha",               function(x) paste0(fmt_dec(x$budget_par_ha, 2), "\u00a0$/ha")),
      list("Budget $/m³",               function(x) paste0(fmt_dec(x$budget_par_m3, 2), "\u00a0$/m\u00b3")),
      list("Coupes totales (ha/an)",    function(x) fmt_nb(x$coupes_totales_haan)),
      list("Plantations (ha/an)",       function(x) fmt_nb(x$plantations_haan))
    )

    style_th    <- "padding:7px 12px; background:#2C5F4E; color:white; font-weight:600; font-size:0.85em; white-space:nowrap; text-align:left;"
    style_td    <- "padding:6px 12px; font-size:0.85em; border-bottom:1px solid #f0f0f0; white-space:nowrap;"
    style_label <- "padding:6px 12px; font-size:0.85em; color:#555; border-bottom:1px solid #f0f0f0; white-space:nowrap; background:#fafafa; font-weight:500;"

    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        style = "border-collapse:collapse; width:100%; font-family:inherit;",
        tags$thead(
          tags$tr(
            tags$th("Indicateur", style = style_th),
            lapply(d$ua, function(uid) tags$th(paste0("UA\u00a0", uid), style = style_th))
          )
        ),
        tags$tbody(
          lapply(indicateurs, function(ind) {
            label  <- ind[[1]]
            calcul <- ind[[2]]
            tags$tr(
              tags$td(label, style = style_label),
              lapply(seq_len(nrow(d)), function(i) {
                valeur <- tryCatch(as.character(calcul(d[i, ])),
                                   error = function(e) "\u2014")
                tags$td(valeur, style = style_td)
              })
            )
          })
        )
      )
    )
  })


  # ===========================================================================
  # TABLEAU COMPLET (onglet Données)
  # ===========================================================================
  output$tableau_complet <- renderDT({
    datatable(
      donnees_ua |>
        mutate(pct_destinee_amenagement = round(pct_destinee_amenagement * 100, 1)),
      rownames   = FALSE,
      filter     = "top",
      extensions = c("Scroller", "FixedColumns"),
      options    = list(
        dom          = "tip",
        scrollX      = TRUE,
        scrollY      = "calc(100vh - 320px)",
        scroller     = TRUE,
        fixedColumns = list(leftColumns = 2),
        pageLength   = 50
      )
    )
  })


  # ===========================================================================
  # TÉLÉCHARGEMENT CSV
  # ===========================================================================
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("possibilites-forestieres-UA_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(
        donnees_ua |>
          mutate(pct_destinee_amenagement = round(pct_destinee_amenagement * 100, 1)),
        file, row.names = FALSE, fileEncoding = "UTF-8"
      )
    }
  )

}
