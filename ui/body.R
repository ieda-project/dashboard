
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "overview",
      fluidRow(
        valueBox(GL_n_consults, "Consultations", icon = icon("stethoscope")),
        valueBox("TODO", "Patients", icon = icon("child"))
      ),

      fluidRow(
        valueBox(GL_n_districts, "Districts", icon = icon("map-marker"), color = "purple"),
        valueBox(GL_n_mobile_users, "Centres de santé", icon = icon("hospital-o"), color = "purple"),
        valueBox(GL_n_health_workers, "Agents de santé", icon = icon("user-md"), color = "purple")
      )
    ),

    tabItem(tabName = "rec_usage_who",
      fluidRow(
        box(title = "Nombre de consultations par type d'agent", htmlOutput("rec_usage_who_line"), width = 12, status = "primary")
      ),
      fluidRow(
        box(title = "Répartition du nombre de consultations par type d'agent", htmlOutput("rec_usage_who_pie"), width = 8, status = "primary"),
        box(title = "Nombre d'agents", dataTableOutput("rec_usage_who_table"), width = 4, status = "primary")
      )
    ),
    
    tabItem(tabName = "rec_usage_profile",
      fluidRow(
        box(title = "Utilisation du REC selon l'heure de la journée et le jour de la semaine", plotOutput("rec_usage_profile_hm"), width = 12, status = "primary")
      )
    ),
    
    tabItem(tabName = "rec_usage_sync_lag",
      fluidRow(
        box(title = "Délai de synchronisation par CSPS", htmlOutput("rec_usage_sync_lag_bar"), width = 8, status = "primary"),
        box(title = "CSPS avec délai sup. à la moyenne", htmlOutput("rec_usage_sync_lag_pie"), width = 4, status = "primary")
      ),
      fluidRow(
        valueBoxOutput("rec_usage_sync_lag_avg"),
        valueBoxOutput("rec_usage_sync_lag_median"),
        valueBoxOutput("rec_usage_sync_lag_n_above_avg")
      )
    ),
    
    tabItem(tabName = "consults_n_consults",
      fluidRow(
        box(title = "Nombre de consultations", htmlOutput("consults_n_consults_bar"), width = 12, status = "primary")
      ),
      fluidRow(
        shiny::column(width = 4,
          valueBoxOutput("consults_n_consults_n", width = NULL),
          valueBoxOutput("consults_n_consults_average", width = NULL),
          valueBoxOutput("consults_n_consults_min", width = NULL),
          valueBoxOutput("consults_n_consults_max", width = NULL)
        ),
        shiny::column(width = 8,
          box(title = "Nombre de consultations par CSPS", htmlOutput("consults_n_consults_tree"), width = NULL, status = "primary")
        )
      )
    ),
    
    tabItem(tabName = "consults_age_sex",
      fluidRow(
        box(title = "Nombre de consultations par tranche d'âge et sexe", htmlOutput("consults_age_sex_range_bar"), width = 8, status = "primary"),
        box(title = "Répartition par tranche d'âge", htmlOutput("consults_n_consults_range_pie"), width = 4, status = "primary")
      ),
      fluidRow(
        box(title = "Nombre de consultations par d'âge", htmlOutput("consults_age_sex_age_bar"), width = 12, status = "primary")
      )
    ),
    
    tabItem(tabName = "epi_profile_classifications",
      fluidRow(
        box(title = "Fréquences des classifications", dataTableOutput("epi_profile_classifications_table"), width = 12, status = "primary")
      )
    ),
    
    tabItem(tabName = "epi_profile_combinations",
      fluidRow(
        box(title = "Fréquence des combinaisons de classifications", dataTableOutput("epi_profile_combinations_table"), width = 12, status = "primary")
      )
    ),
    
    tabItem(tabName = "data_entry",
      fluidRow(
        box(title = "Durée moyenne de saisie des formulaires", htmlOutput("data_entry_average"), width = 6, status = "primary"),
        box(title = "Durée moyenne de saisie ~ Qualification de l'agent", htmlOutput("data_entry_qualification"), width = 6, status = "primary")
      ),
      fluidRow(
        box(title = "Durée moyenne de saisie ~ Nombre de classifications", htmlOutput("data_entry_n_classifications_eval"), width = 6, status = "primary"),
        box(title = "Durée moyenne de saisie ~ Nombre de classifications", htmlOutput("data_entry_n_classifications_treat"), width = 6, status = "primary")
      ),
      fluidRow(
        valueBoxOutput("data_entry_n_minutes", width = 3),
        valueBoxOutput("data_entry_n_hours", width = 3),
        valueBoxOutput("data_entry_n_days", width = 3),
        valueBoxOutput("data_entry_average_agent", width = 3)
      )
    )
  )
)