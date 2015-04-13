library(lubridate)
library(plyr)
library(dplyr)

# Clean - Rename factors for treatments
# Graph - Frequency of classifications
  # Save default par state before each graph and restore after. 
  # axis(1, las=2)
  # par(mar = c(w, x, y, z))

weekdays_vector <- function() {
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
}

tablet_users_load_data <- function(filename) {
  users <- read.csv(filename)
  users <- select(users, info.case_id, qualification, user_type)
  users <- dplyr::rename(users, health_worker_id = info.case_id)
  users <- filter(users, qualification != "ibe")
  users$qualification <- factor(users$qualification)
  users$qualification <- revalue(users$qualification, c("ab" = "AB", "accoucheuse" = "Accoucheuse", "ais" = "AIS", "fe_me" = "Sage femme", "ib" = "IB", "ide" = "IDE", "other" = "Autre"))
  users$user_type <- revalue(users$user_type, c("admin" = "ICP", "regular" = "Agent"))
  users
}

mobile_users_load_data <- function(filename) {
  users <- read.csv(filename)
  colnames(users) <- c("mobile_user", "district")
  users
}

enroll_child_load_data <- function(filename, mobile_users) {
  # Read data
  data <- read.csv(filename)
  
  # Format data
  colnames(data) <- c("started_on", "completed_on", "received_on", "dob_known", "mobile_user", "sex")
  data$started_on <- as.POSIXct(data$started_on, format = "%Y-%m-%d %H:%M")
  data$completed_on <- as.POSIXct(data$completed_on, format = "%Y-%m-%d %H:%M")
  data$received_on <- as.POSIXct(data$received_on, format = "%Y-%m-%d %H:%M")
  data <- filter(data, !grepl("test|formation|demo", mobile_user))
  data$mobile_user <- factor(data$mobile_user)
  
  data <- merge(data, mobile_users, by.x = "mobile_user", by.y = "mobile_user")
  data$district <- factor(data$district)
  
  data$sex <- mapvalues(data$sex, from = c("female", "male"), to = c("Fille", "Garçon"))
  
  # Add weekday & time columns
  data$started_hour <- factor(hour(data$started_on))
  data$started_weekday <- ordered(weekdays(data$started_on), levels = weekdays_vector())
  data$started_month <- month(data$started_on, label = T, abbr = F)
  data$started_year <- year(data$started_on)
  
  data$completed_hour <- factor(hour(data$completed_on))
  data$completed_weekday <- ordered(weekdays(data$completed_on), levels = weekdays_vector())
  data$completed_month <- month(data$completed_on, label = T, abbr = F)
  data$completed_year <- year(data$completed_on)
  
  data$received_hour <- factor(hour(data$received_on))
  data$received_weekday <- ordered(weekdays(data$received_on), levels = weekdays_vector())
  data$received_month <- month(data$received_on, label = T, abbr = F)
  data$received_year <- year(data$received_on)
  
  # Add duration colummn
  data$duration <- as.numeric(difftime(data$completed_on, data$started_on, units = "mins"))
  data$sync_lag <- as.numeric(difftime(data$received_on, data$completed_on, units = "mins"))
  
  data
}

child_visit_load_data <- function(filename, tablet_users, mobile_users) {
  # Read data
  data <- read.csv(filename)
  
  # Format data
  colnames(data) <- c("started_on", "completed_on", "received_on", "health_worker_id", "mobile_user", "case_id",
                      "height", "weight", "temperature", "muac",
                      "classification_oreille", "classification_diahree", "classification_vih", "classification_paludisme",
                      "classification_deshydratation", "classification_pneumonie", "classification_malnutrition",
                      "classification_rougeole", "classification_anemie", "classification_dysenterie",
                      "visit_date", "dob", "sex", "visit_type",
                      "l_wfa", "m_wfa", "s_wfa", "zscore_wfa",
                      "l_wfh", "m_wfh", "s_wfh", "zscore_wfh",
                      "l_hfa", "m_hfa", "s_hfa", "zscore_hfa")
  data <- filter(data, !grepl("test|formation|demo", mobile_user))
  data$mobile_user <- factor(data$mobile_user)
  data <- merge(data, tablet_users, by.x = "health_worker_id", by.y = "health_worker_id")
  data <- merge(data, mobile_users, by.x = "mobile_user", by.y = "mobile_user")
  data$district <- factor(data$district)
  
  data$started_on <- as.POSIXct(data$started_on, format = "%Y-%m-%d %H:%M")
  data$completed_on <- as.POSIXct(data$completed_on, format = "%Y-%m-%d %H:%M")
  data$received_on <- as.POSIXct(data$received_on, format = "%Y-%m-%d %H:%M")
  data$visit_date <- as.POSIXct(data$visit_date, format = "%Y-%m-%d")
  data$dob <- as.POSIXct(data$dob, format = "%Y-%m-%d")
  
  data$muac <- as.numeric(levels(data$muac))[data$muac] # Convert MUAC to numeric
  
  data$sex <- mapvalues(data$sex, from = c("female", "male"), to = c("Fille", "Garçon"))
  
  data$classification_oreille <- revalue_classifications(data$classification_oreille, "oreille")
  data$classification_diahree <- revalue_classifications(data$classification_diahree, "diahree")
  data$classification_vih <- revalue_classifications(data$classification_vih, "vih")
  data$classification_paludisme <- revalue_classifications(data$classification_paludisme, "paludisme")
  data$classification_deshydratation <- revalue_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_pneumonie <- revalue_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_malnutrition <- revalue_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_rougeole <- revalue_classifications(data$classification_rougeole, "rougeole")
  data$classification_anemie <- revalue_classifications(data$classification_anemie, "anemie")
  data$classification_dysenterie <- revalue_classifications(data$classification_dysenterie, "dysenterie")
  data$qualification <- revalue(data$qualification, c("ab" = "AB", "accoucheuse" = "Accoucheuse",
                                                      "ais" = "AIS", "fe_me" = "Sage femme", "ib" = "IB",
                                                      "ibe" = "IDE", "ibe" = "IDE", "other" = "Autre"))
  data$n_classifications <- apply(data, 1, count_classifications)
  
  # Add weekday & time columns
  data$started_hour <- factor(hour(data$started_on))
  data$started_weekday <- ordered(weekdays(data$started_on), levels = weekdays_vector())
  data$started_month <- month(data$started_on, label = T, abbr = F)
  data$started_year <- year(data$started_on)
  data$started_year_month <- strftime(data$started_on, format = "%Y-%m")
  
  data$completed_hour <- factor(hour(data$completed_on))
  data$completed_weekday <- ordered(weekdays(data$completed_on), levels = weekdays_vector())
  data$completed_month <- month(data$completed_on, label = T, abbr = F)
  data$completed_year <- year(data$completed_on)
  data$completed_year_month <- strftime(data$completed_on, format = "%Y-%m")
  
  data$received_hour <- factor(hour(data$received_on))
  data$received_weekday <- ordered(weekdays(data$received_on), levels = weekdays_vector())
  data$received_month <- month(data$received_on, label = T, abbr = F)
  data$received_year <- year(data$received_on)
  data$received_year_month <- strftime(data$received_on, format = "%Y-%m")
  
  # Add duration colummn
  data$duration <- as.numeric(difftime(data$completed_on, data$started_on, units = "mins"))
  data$sync_lag <- as.numeric(difftime(data$received_on, data$completed_on, units = "mins"))
  
  # Add age related columns in months
  data$age <- floor(as.numeric(difftime(data$started_on, data$dob, units = "weeks")) / 4) # in months
  data$age_range <- to_age_range_month(data$age)
  
  data
}

child_treatment_load_data <- function(filename, mobile_users) {
  # Read data
  data <- read.csv(filename)
  
  # Format data
  colnames(data) <- c("started_on", "completed_on", "received_on", "mobile_user", "child_case_id",
                      "treatment_pneumonie_grave", "treatment_deshydratation_severe_grave",
                      "treatment_diahree_persistante_severe_grave", "treatment_paludisme_grave",
                      "med_perfusion_p1_a", "med_perfusion_p1_b", "med_perfusion_p2_a", "med_perfusion_p2_b",
                      "med_deparasitage", "med_artemether", "med_vitamine_a", "med_antibio",
                      "classification_oreille", "classification_diahree", "classification_vih", "classification_paludisme",
                      "classification_deshydratation", "classification_pneumonie", "classification_malnutrition",
                      "classification_rougeole", "classification_anemie", "classification_dysenterie",
                      "age", "weight")
  data$started_on <- as.POSIXct(data$started_on, format = "%Y-%m-%d %H:%M")
  data$completed_on <- as.POSIXct(data$completed_on, format = "%Y-%m-%d %H:%M")
  data$received_on <- as.POSIXct(data$received_on, format = "%Y-%m-%d %H:%M")
  data <- filter(data, !grepl("test|formation|demo", mobile_user))
  data$mobile_user <- factor(data$mobile_user)
  
  data <- merge(data, mobile_users, by.x = "mobile_user", by.y = "mobile_user")
  data$district <- factor(data$district)
  
  data$classification_oreille <- revalue_classifications(data$classification_oreille, "oreille")
  data$classification_diahree <- revalue_classifications(data$classification_diahree, "diahree")
  data$classification_vih <- revalue_classifications(data$classification_vih, "vih")
  data$classification_paludisme <- revalue_classifications(data$classification_paludisme, "paludisme")
  data$classification_deshydratation <- revalue_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_pneumonie <- revalue_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_malnutrition <- revalue_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_rougeole <- revalue_classifications(data$classification_rougeole, "rougeole")
  data$classification_anemie <- revalue_classifications(data$classification_anemie, "anemie")
  data$classification_dysenterie <- revalue_classifications(data$classification_dysenterie, "dysenterie")
  data$n_classifications <- apply(data, 1, count_classifications)
  
  # Add weekday & time columns
  data$started_hour <- factor(hour(data$started_on))
  data$started_weekday <- ordered(weekdays(data$started_on), levels = weekdays_vector())
  data$started_month <- month(data$started_on, label = T, abbr = F)
  data$started_year <- year(data$started_on)
  
  data$completed_hour <- factor(hour(data$completed_on))
  data$completed_weekday <- ordered(weekdays(data$completed_on), levels = weekdays_vector())
  data$completed_month <- month(data$completed_on, label = T, abbr = F)
  data$completed_year <- year(data$completed_on)
  
  data$received_hour <- factor(hour(data$received_on))
  data$received_weekday <- ordered(weekdays(data$received_on), levels = weekdays_vector())
  data$received_month <- month(data$received_on, label = T, abbr = F)
  data$received_year <- year(data$received_on)
  
  # Add duration colummn
  data$duration <- as.numeric(difftime(data$completed_on, data$started_on, units = "mins"))
  data$sync_lag <- as.numeric(difftime(data$received_on, data$completed_on, units = "mins"))
  
  data
}

revalue_classifications <- function(var, classification) {
  if (classification == "oreille") {
    var <- mapvalues(var,
            from = c("infection_aigue_oreille", "infection_chronique_oreille", "mastoidite", ""),
            to = c("Infection aigüe", "Infection chronique", "Mastoïdite", "---"))
  } else if (classification == "diahree") {
    var <- mapvalues(var,
            from = c("diahree_persistante", ""),
            to = c("Diahree persistente", "---"))
  } else if (classification == "vih") {
    var <- mapvalues(var,
            from = c("vih_confirmee", "vih_pas", "vih_peu_probable", "vih_possible", "vih_symp_confirmee", "vih_symp_probable", "vih_symp_suspecte", ""),
            to = c("VIH confirmé", "Pas de VIH", "VIH peu probable", "VIH possible", "VIH symp. comfirmé", "VIH symp. probable", "VIH symp. suspecte", "---"))    
  } else if (classification == "paludisme") {
    var <- mapvalues(var,
            from = c("paludisme_grave", "paludisme_grave_sans_tdr", "paludisme_simple", "pas_paludisme", ""),
            to = c("Paludisme Grave", "Paludisme grave - TDR-", "Paludisme simple", "Pas de paludisme", "---"))
  } else if (classification == "deshydratation") {
    var <- mapvalues(var,
            from = c("deshydratation_severe_grave", "pas_deshydratation", "signes_deshydratation", ""),
            to = c("Deshy. grave", "Pas de deshy.", "Signes évidents de deshy.", "---"))
  } else if (classification == "pneumonie") {
    var <- mapvalues(var,
            from = c("pas_pneumonie", "pneumonie", "pneumonie_grave", ""),
            to = c("Pas de pneumonie", "Pneumonie", "Pneumonie grave", "---"))
  } else if (classification == "malnutrition") {
    var <- mapvalues(var,
            from = c("mam", "masc", "mass", "pas_malnutrition", ""),
            to = c("MAM", "MASc", "MASs", "Pas de malnutrition", "---"))
  } else if (classification == "rougeole") {
    var <- mapvalues(var,
            from = c("antecedent_rougeole", "rougeole", "rougeole_complications", "rougeole_compliquee", ""),
            to = c("Antécédents", "Rougeole", "Complications yeux/bouche", "Rougeole compliquée", "---"))
  } else if (classification == "anemie") {
    var <- mapvalues(var,
            from = c("anemie", "anemie_grave", ""),
            to = c("Anémie", "Anémie grave", "---"))
  } else if (classification == "dysenterie") {
    var <- mapvalues(var,
            from = c("dysenterie", ""),
            to = c("Dysenterie", "---"))
  }
  var
}

count_classifications <- function (consult) {
  (consult["classification_oreille"] != "---") + (consult["classification_diahree"] != "---") +
    (consult["classification_vih"] != "---") + (consult["classification_paludisme"] != "---") +
    (consult["classification_deshydratation"] != "---") + (consult["classification_pneumonie"] != "---") +
    (consult["classification_malnutrition"] != "---") + (consult["classification_rougeole"] != "---") +
    (consult["classification_anemie"] != "---") + (consult["classification_dysenterie"] != "---")
}

to_age_range_month <- function (data) {
  data <- findInterval(data, c(0, 2, 6, 12, 24, 36, 48, 60, 100))
  data <- factor(data)
  data <- mapvalues(data,
                   from = c("1", "2", "3", "4", "5", "6", "7", "8"),
                   to = c("0-2", "2-6", "6-12", "12-24", "24-36", "36-48", "48-60", ">=60"))
  data
}

# Analyser les durées négatives pour faire ressortir les problèmes d'heure sur les tablettes
# nombre de formulaire par csps
# Temps moyen de saisie du formulaire de consulation ou traitement en fonction du nombre de classifications trouvées
# Temps moyen de saise du formulaire (consult ou traitement) en fonction du nombre de classifications graves
# nombre de consultations par agent par CSPS