# =============================================================================
# ui.R
# Possibilités forestières du Québec — SYN-00624
# =============================================================================
#
# RÔLE DE CE FICHIER
# -------------------
# ui.R décrit ce que l'usager VOIT : la mise en page, les onglets, les boutons,
# les espaces réservés pour les graphiques et les tableaux.
#
# Ce fichier ne contient PAS de logique — il indique seulement QUÉ afficher
# et OÙ. Le COMMENT est géré dans server.R.
#
# STRUCTURE DE LA PAGE
# ---------------------
# page_navbar() crée une page avec une barre de navigation en haut.
# Chaque nav_panel() est un onglet.
# La sidebar est commune à tous les onglets.
#
# CONCEPTS CLÉS DE L'UI SHINY
# ----------------------------
# - Inputs  : éléments que l'usager contrôle (ex. actionButton, selectInput)
#             Chaque input a un inputId unique → accessible dans server.R via input$id
# - Outputs : espaces réservés pour du contenu généré par le serveur
#             (ex. plotlyOutput, DTOutput, leafletOutput, uiOutput)
#             Chaque output a un outputId unique → rempli dans server.R via output$id
# - uiOutput : cas spécial — l'interface elle-même est générée dynamiquement
#              par le serveur (renderUI). Utile quand le contenu dépend des données.


ui <- page_navbar(
  
  # Titre dans la barre de navigation
  title = tags$div(
    style = "display:flex; align-items:center; gap:10px;",
    bsicons::bs_icon("tree-fill", size = "1.3em"),
    tags$span("Possibilités forestières du Québec — SYN-00624",
              style = "font-weight:600;")
  ),
  
  theme    = theme_app,  # Défini dans global.R
  fillable = TRUE,       # Les onglets occupent toute la hauteur de la fenêtre
  bg       = "#2C5F4E",  # Couleur de fond de la barre de navigation
  inverse  = TRUE,       # Texte blanc dans la barre (pour contraste sur fond sombre)
  
  
  # ===========================================================================
  # ONGLET 1 : FICHE UA
  # Sélection d'UA sur une carte Leaflet → analyse agrégée sous la carte
  # ===========================================================================
  nav_panel(
    title = "Fiche UA",
    icon  = bsicons::bs_icon("file-earmark-bar-graph"),
    
    # -------------------------------------------------------------------------
    # CARTE DE SÉLECTION
    # card() = conteneur visuel avec ombre et bordure (composant bslib)
    # card_header() = titre de la carte
    # card_body() = contenu de la carte
    # -------------------------------------------------------------------------
    card(
      fill  = FALSE,  # Ne pas laisser la carte s'étirer pour remplir l'espace
      style = "margin-bottom:12px;",
      
      card_header(
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:8px;",
          
          # Titre de la section
          tags$span(
            bsicons::bs_icon("map-fill", style = "color:#2C5F4E; margin-right:6px;"),
            tags$strong("Sélectionner des unités d'aménagement"),
            style = "font-size:0.95em;"
          ),
          
          # Zone droite : pastilles des UA sélectionnées + bouton effacer
          tags$div(
            style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
            
            # uiOutput : espace réservé — rempli dynamiquement par output$fiche_ua_tags
            # dans server.R. Le contenu change selon les UA sélectionnées.
            uiOutput("fiche_ua_tags"),
            
            
            # Bouton : sélectionner toutes les UA du Québec forestier d'un coup.
            # Déclenche observeEvent(input$fiche_tout, ...) dans server.R.
            # Tous les codes UA de donnees_ua seront mis dans ua_sel().
            actionButton(
              inputId = "fiche_tout",
              label   = "Sélection complète",
              icon    = icon("check-double"),
              class   = "btn-sm btn-outline-primary"
            ),
            
            # actionButton : déclenche observeEvent(input$fiche_clear, ...) dans server.R
            actionButton(
              inputId = "fiche_clear",        # ID pour y accéder dans server.R
              label   = "Effacer",
              icon    = icon("xmark"),
              class   = "btn-sm btn-outline-secondary"
            )
          )
        )
      ),
      
      card_body(
        padding = "0",  # Pas de marges internes — la carte Leaflet remplit tout
        
        # Instruction contextuelle
        tags$div(
          style = "padding:4px 12px; font-size:0.78em; color:#999; border-bottom:1px solid #eee;",
          "Cliquez sur une UA pour l'ajouter ou la retirer de l'analyse."
        ),
        
        # leafletOutput : espace réservé pour la carte Leaflet
        # Rempli par output$carte_selection_fiche dans server.R
        # height fixe en pixels — la carte n'est pas interactive pour la hauteur
        leafletOutput("carte_selection_fiche", height = "320px", width = "100%")
      )
    ), # fin card carte de sélection
    
    # -------------------------------------------------------------------------
    # CONTENU DYNAMIQUE
    # uiOutput("fiche_contenu") est rempli par renderUI() dans server.R.
    # Quand aucune UA n'est sélectionnée → message d'invite
    # Quand 1+ UA sont sélectionnées → KPI, graphiques, tableaux
    # -------------------------------------------------------------------------
    uiOutput("fiche_contenu")
    
  ), # fin nav_panel Fiche UA
  
  
  # ===========================================================================
  # ONGLET 2 : DONNÉES
  # Tableau complet de toutes les variables pour toutes les UA + export CSV
  # ===========================================================================
  nav_panel(
    title = "Données",
    icon  = bsicons::bs_icon("table"),
    
    card(
      full_screen = TRUE,  # Bouton pour agrandir en plein écran (bslib)
      card_header(
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          tags$span("Tableau intégral — toutes les variables (2025-2028)",
                    style = "font-weight:600;"),
          # downloadButton : déclenche output$dl_csv dans server.R
          downloadButton(
            outputId = "dl_csv",
            label    = "Télécharger CSV",
            class    = "btn-sm btn-outline-primary",
            icon     = icon("download")
          )
        )
      ),
      # DTOutput : espace réservé pour le tableau interactif DT
      # Rempli par output$tableau_complet (renderDT) dans server.R
      DTOutput("tableau_complet", height = "100%")
    )
  ), # fin nav_panel Données
  
  
  # ===========================================================================
  # ONGLET 3 : À PROPOS
  # Texte statique — pas d'interaction, pas d'outputs réactifs
  # ===========================================================================
  nav_panel(
    title = "À propos",
    icon  = bsicons::bs_icon("info-circle"),
    
    card(
      max_height = "90vh",
      card_body(
        style = "padding:32px; max-width:820px; line-height:1.7;",
        
        tags$h3("Possibilités forestières du Québec — SYN-00624",
                style = "color:#2C5F4E; margin-bottom:6px;"),
        tags$p(style = "color:#888; font-size:0.9em; margin-bottom:24px;",
               "Outil exploratoire — période 2025-2028"),
        
        tags$hr(style = "margin-bottom:24px;"),
        
        tags$h5("Description", style = "color:#2C5F4E;"),
        tags$p("Outil de consultation des variables forestières associées au calcul ",
               "des possibilités forestières (CPF) 2025-2028, à l'échelle des unités ",
               "d'aménagement (UA) du Québec. Sélectionnez une ou plusieurs UA sur la ",
               "carte pour afficher l'analyse comparative."),
        
        tags$h5("Contenu de l'analyse par UA", style = "color:#2C5F4E; margin-top:20px;"),
        tags$ul(style = "padding-left:20px;",
                tags$li(tags$strong("Territoire : "),
                        "superficies par catégorie, % destiné à l'aménagement"),
                tags$li(tags$strong("Récolte prévue : "),
                        "possibilité totale et par essence (m³/an)"),
                tags$li(tags$strong("Dendrométrie : "),
                        "âge de récolte, volume sur pied, taux de récolte, productivité"),
                tags$li(tags$strong("Travaux sylvicoles : "),
                        "coupes, plantations, éducation (ha/an)"),
                tags$li(tags$strong("Budget sylvicole : "),
                        "investissement annuel ($/an, $/ha, $/m³)")
        ),
        
        tags$h5("Source des données", style = "color:#2C5F4E; margin-top:20px;"),
        tags$p("Bureau du forestier en chef, gouvernement du Québec. ",
               tags$em("Possibilités forestières 2025-2028 — Principales variables ",
                       "forestières associées au calcul"),
               " (SYN-00624). Mise à jour du 10 décembre 2024."),
        
        tags$h5("Avertissement", style = "color:#2C5F4E; margin-top:20px;"),
        tags$p(style = "color:#666;",
               "Prototype exploratoire. Pour toute utilisation officielle ou ",
               "réglementaire, se référer aux publications du Bureau du forestier en chef."),
        
        tags$hr(style = "margin-top:24px; margin-bottom:16px;"),
        tags$p(style = "font-size:0.8em; color:#aaa;",
               "Développé au Bureau du forestier en chef — Québec")
      )
    )
  ) # fin nav_panel À propos
  
) # fin page_navbar