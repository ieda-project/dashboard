library(shiny)
library(leaflet)
library(googleVis)
library(ggplot2)

# Nombre de nouveaux infirmiers sur une période pour avoir une idée du turnover du staff

function(input, output, session) {
  
  # Filter the dataset
  enroll_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d")
    hour(s) <- 23; minute(s) <- 59; second(s) <- 59;
    hour(e) <- 23; minute(e) <- 59; second(e) <- 59;
    d <- filter(enroll_data, started_on >= s, started_on <= e)
    if (input$district != "Tous") {
      d <- filter(d, district == input$district)
    }
    d
  })
  
  visit_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d")
    hour(s) <- 23; minute(s) <- 59; second(s) <- 59;
    hour(e) <- 23; minute(e) <- 59; second(e) <- 59;
    d <- filter(visit_data, started_on >= s, started_on <= e)
    if (input$district != "Tous") {
      d <- filter(d, district == input$district)
    }
    d
  })
  
  treatment_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d")
    hour(s) <- 23; minute(s) <- 59; second(s) <- 59;
    hour(e) <- 23; minute(e) <- 59; second(e) <- 59;
    d <- filter(visit_data, started_on >= s, started_on <= e)
    d <- filter(treatment_data, started_on >= s, started_on <= e)
    if (input$district != "Tous") {
      d <- filter(d, district == input$district)
    }
    d
  })
  
  sync_lag_data <- reactive({ sync_lag(visit_data_f(), "mobile_user") })
  
  sync_lag_summary <- reactive({ summary(sync_lag_data()) })
  
  n_consults_csps <- reactive({
    f <- factor(visit_data_f()$mobile_user)
    table(f)
  })
  
  sum_data_entry <- reactive({sum(visit_data_f()$duration) + sum(treatment_data_f()$duration) + sum(enroll_data_f()$duration)})
  
  geo_data <- reactive({
    data <- visit_data_f()
    data$site_code <- factor(data$site_code)
    
    # Sync lag
    d <- data.frame(sync_lag(data, "site_code"))
    d <- cbind(rownames(d), d)
    colnames(d) <- c("site_code", "sync_lag")
    d <- mutate(d, sync_lag = round(as.numeric(sync_lag), 2))
    d <- arrange(d, site_code)

    # Consults
    consults <- data.frame(table(data$site_code))
    colnames(consults) <- c("site_code", "n_consults")
    consults <- arrange(consults, site_code)
    d <- mutate(d, n_consults = consults$n_consults)
    
    # Coordinates
    d <- merge(d, locations_data, by.x = "site_code", by.y = "site_code")
    d <- filter(d, !is.na(latitude))
    d
  })
  
  mapp <- createLeafletMap(session, "map")
  
  session$onFlushed(once = TRUE, function() {
    paintObs <- observe({
      data <- geo_data()
      
      mapp$clearShapes()
      mapp$clearMarkers()
      
      if (input$geo_data == "position") {
        mapp$addMarker(data$latitude, data$longitude)
      } else if (input$geo_data == "n_consults") {
        radius <- data$n_consults
        mapp$addCircle(data$latitude, data$longitude, radius * 5, data$site_code, list(stroke = F, fill = T, fillOpacity = 0.4))
      } else {
        radius <- data$sync_lag
        mapp$addCircle(data$latitude, data$longitude, radius * 150, data$site_code, list(stroke = F, fill = T, fillOpacity = 0.4))
      }
    })
    
    session$onSessionEnded(paintObs$suspend)
  })
  
  showInfoPopup <- function(site_code, lat, lng) {
    content <- paste("CSPS :", site_code)
    mapp$showPopup(lat, lng, content, site_code)
  }
  
  clickObs <- observe({
    mapp$clearPopups()
    event <- input$mapp_shape_click
    if (is.null(event)) {
      print("-- NULL")
      return()
    }
    isolate({
      showInfoPopup(event$id, event$lat, event$lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  output$rec_usage_who_line <- renderGvis({
    qual <- as.data.frame.matrix(t(table(visit_data_f()$qualification, visit_data_f()$started_year_month)))
    qual <- cbind(rownames(qual), qual)

    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Mois de l\\\'année'}",
                    vAxis = "{title:'Nombre de consultations'}")
    
    gvisLineChart(qual, options = options)
  })
  
  output$rec_usage_who_pie <- renderGvis({  
    all_qual <- as.data.frame(table(visit_data_f()$qualification))
    
    options <- list(height = GL_chart_h)
    
    gvisPieChart(all_qual, options = options)
  })
  
  output$rec_usage_who_table <- renderDataTable({
    data <- consulting_health_workers(visit_data_f())
    data <- table(data$qualification)
    data <- data.frame(data)
  }, options = list(paging = F, searching = F, columns = list(list(title = "Qualification"), list(title = "Nombre"))))
  
  output$rec_usage_profile_hm <- renderPlot({
    data <- form_activity(visit_data_f(), "started")
    
    p <- ggplot(data, aes(x = hour, y = weekday, fill = count))
    p + geom_tile() + labs(x = "Heure de la journée", y = "Jour de la semaine", fill = "Qté")
  })

  output$rec_usage_sync_lag_bar <- renderGvis({
    summary <- sync_lag_summary()
    data <- data.frame(sync_lag_data())
    data <- cbind(rownames(data), data)
    colnames(data) <- c("csps", "delay")
    data <- mutate(data, average = summary[4])
    data <- mutate(data, delay = round(as.numeric(delay), 2))
    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Centre de santé'}",
                    vAxis = "{title:'Délai de synchronisation (jours)'}",
                    legend = "{position:'none'}",
                    seriesType = "bars", 
                    series = "{1:{type:'line'}}")
    
    gvisComboChart(data, options = options)
  })
  
  output$rec_usage_sync_lag_pie <- renderGvis({
    summary <- sync_lag_summary()
    data <- round(sync_lag_data(), 2)
    data <- table(data > summary[4])
    names(data) <- c("Faux", "Vrai")
    data <- data.frame(data) 
    
    options = list(height = GL_chart_h, legend = "{position:'bottom'}")
    
    gvisPieChart(data, options = options)
  })
  
  output$rec_usage_sync_lag_avg <- renderValueBox({
    data <- sync_lag_summary()[4]
    valueBox(data, "Moyenne (jours)", icon = icon("info-circle"), color = "aqua")
  })
  
  output$rec_usage_sync_lag_n_above_avg <- renderValueBox({
    data <- sum(sync_lag_data() > sync_lag_summary()[4])
    valueBox(data, "Nb.CSPS avec un délai > moyenne", icon = icon("exclamation-circle"), color = "red")
  })
  
  output$rec_usage_sync_lag_median <- renderValueBox({
    data <- sync_lag_summary()[3]
    valueBox(data, "Médianne (jours)", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_bar <- renderGvis({
    options <- list(height = GL_chart_h,
      hAxis = "{title:'Mois de l\\\'année'}",
      vAxis = "{title:'Nombre de consultations'}")
    
    if (input$district != "Tous") {
      data <- table(visit_data_f()$started_year_month)
      data <- data.frame(data)
      colnames(data) <- c("month", input$district)
      options$seriesType = "bars"
      gvisColumnChart(data, options = options)
    } else {
      data <- as.data.frame.matrix(t(table(visit_data_f()$district, visit_data_f()$started_year_month)))
      names <- rownames(data)
      data <- mutate(data, total = apply(data, 1, sum))
      data <- mutate(data, total.annotation = as.character(total))
      data <- cbind(names, data)
      colnames(data)[1] <- "month"
      yvar <- colnames(data)[2:ncol(data)]
      options$series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = "")
      options$seriesType = "bars"
      options$isStacked = T
      gvisComboChart(data, xvar = "month", yvar = yvar, options = options)
    }
  })
  
  output$consults_n_consults_n <- renderValueBox({
    data <- format_number(nrow(visit_data_f()))
    valueBox(data, "Total sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_average <- renderValueBox ({
    data <- trunc(mean(n_consults_csps()))
    valueBox(data, "Moyenne par CSPS sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_min <- renderValueBox ({
    data <- min(n_consults_csps())
    valueBox(data, "Minimum sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_max <- renderValueBox ({
    data <- max(n_consults_csps())
    valueBox(data, "Maximum sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_tree <- renderGvis({
    data <- summarise(group_by(visit_data_f(), mobile_user, district), n_consults = length(age))
    data$color <- data$n_consults
    data$mobile_user <- as.character(data$mobile_user)    
    data$district <- as.character(data$district)
    data$district <- paste("District", data$district) # avoid duplicates with mobile_user (ex. seguenega)
    
    u <- unique(data$district)
    ds <- data.frame(cbind(u, rep("Burkina Faso", length(u)), rep(0, length(u)), rep(0, length(u))))
    colnames(ds) <- colnames(data)
    ds$n_consults <- as.numeric(levels(ds$n_consults))[ds$n_consults] # convert from factor
    ds$color <- ds$n_consults
    ds$mobile_user <- as.character(ds$mobile_user)
    ds$district <- as.character(ds$district)
    
    data <- rbind(ds, data)
    data <- rbind(c("Burkina Faso", NA, 0, 0), data)
    
    options <- list(height = GL_chart_h,
      showScale = T,
      minColor = "#fc8d59",
      midColor = "#ffffbf", 
      maxColor = "#2b83ba")
    
    gvisTreeMap(data, idvar = "mobile_user", parentvar = "district",
      sizevar = "n_consults", colorvar = "color", options = options)
  })
  
  output$consults_age_sex_range_bar <- renderGvis ({
    data <- as.data.frame.matrix(t(table(visit_data_f()$sex, visit_data_f()$age_range)))
    names <- rownames(data)
    data <- mutate(data, total = apply(data, 1, sum))
    data <- mutate(data, total.annotation = as.character(total))
    data <- cbind(names, data)
    colnames(data)[1] <- "range"
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
      hAxis = "{title:'Tranche d\\\'âge (mois)'}",
      vAxis = "{title:'Nombre de consultations'}",
      series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
      seriesType = "bars",
      isStacked = T)
    
    gvisComboChart(data, xvar = "range", yvar = yvar, options = options)
  })
  
  output$consults_n_consults_range_pie <- renderGvis ({
    data <- data.frame(table(visit_data_f()$age_range))
    
    options <- list(height = GL_chart_h,
      legend = "{position:'bottom'}")
    
    gvisPieChart(data, options = options)
  })
  
  output$consults_age_sex_age_bar <- renderGvis({
    data <- as.data.frame.matrix(t(table(visit_data_f()$sex, visit_data_f()$age)))
    data <- cbind(rownames(data), data)
    
    options <- list(height = GL_chart_h,
      hAxis = "{title:'Age (mois)'}",
      vAxis = "{title:'Nombre de consultations'}",
      isStacked = T)
    
    gvisColumnChart(data, options = options)
  })
  
  output$epi_profile_classifications_table <- renderDataTable({
    clean <- F
    if (input$class_freq == "illness") { clean <- T }
    
    classifications_frequency(visit_data_f(), clean)
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  
  output$epi_profile_combinations_table <- renderDataTable({
    clean <- F
    if (input$class_comb == "illness") { clean <- T }
    
    classifications_combinations(visit_data_f(), clean)
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10, scrollX = T))

  output$data_entry_average <- renderGvis({
    e <- enroll_data_f()
    v <- visit_data_f()
    t <- treatment_data_f()
    data <- data.frame(c(mean(e$duration), mean(v$duration), mean(t$duration)))
    data <- cbind(c("Enregistrement", "Evaluation", "Traitement"), data)
    colnames(data) <- c("form", "average")
    data$average <- round(data$average, 2)
    
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    options <- list(height = GL_chart_h,
      legend = "{position:'none'}",
      hAxis = "{title:'Formulaire'}",
      vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:0}",
      series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
      seriesType = "bars")
    
    gvisComboChart(data, xvar = "form", yvar = yvar, options = options)
  })
  
  output$data_entry_qualification <- renderGvis({
    data <- data.frame(tapply(visit_data_f()$duration, visit_data_f()$qualification, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("qualification", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]

    options <- list(height = GL_chart_h,
      title = "Formulaire d'évaluation",
      legend = "{position:'none'}",
      hAxis = "{title:'Qualification'}",
      vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:1}",
      series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
      seriesType = "bars")
    
    gvisColumnChart(data, xvar = "qualification", yvar = yvar, options = options)
  })
  
  output$data_entry_n_classifications_eval <- renderGvis({
    data <- data.frame(tapply(visit_data_f()$duration, visit_data_f()$n_classifications, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("n_classifications", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
      title = "Formulaire d'évaluation",
      legend = "{position:'none'}",
      hAxis = "{title:'Nombre de classifications'}",
      vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:1}",
      series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
      seriesType = "bars")
    
    gvisColumnChart(data, xvar = "n_classifications", yvar = yvar, options = options)
  })
  
  output$data_entry_n_classifications_treat <- renderGvis({
    data <- data.frame(tapply(treatment_data_f()$duration, treatment_data_f()$n_classifications, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("n_classifications", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
      title = "Formulaire de traitement",
      legend = "{position:'none'}",
      hAxis = "{title:'Nombre de classifications'}",
      vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:0}",
      series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
      seriesType = "bars")
    
    gvisColumnChart(data, xvar = "n_classifications", yvar = yvar, options = options)
  })
  
  output$data_entry_n_minutes <- renderValueBox ({
    data <- format_number(sum_data_entry())
    valueBox(data, "Nombre de minutes de saisie", icon = icon("clock-o"), color = "light-blue")
  })
  
  output$data_entry_n_hours <- renderValueBox ({
    data <- format_number(trunc(sum_data_entry() / 60))
    valueBox(data, "Nombre d'heures de saisie", icon = icon("clock-o"), color = "green")
  })
  
  output$data_entry_n_days <- renderValueBox ({
    data <- format_number(trunc(sum_data_entry() / 60 / 24))
    valueBox(data, "Nombre de jours de saisie", icon = icon("clock-o"), color = "aqua")
  })
  
  output$data_entry_average_agent <- renderValueBox ({
    n <- length(unique(visit_data_f()$health_worker_id))
    data <- format_number(trunc(sum_data_entry() / 60 / n))
    valueBox(data, "Moyenne par agent (heures)", icon = icon("clock-o"), color = "yellow")
  })
}