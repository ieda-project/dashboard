source("data-scripts/load-data.R")
source("data-scripts/graph-par.R")
source("data-scripts/transforms.R")

tablet_users_data <- tablet_users_load_data("data/tablet-users.csv")
mobile_users_data <- mobile_users_load_data("data/mobile-users.csv")
enroll_data <- enroll_child_load_data("data/enroll-child.csv", mobile_users_data)
visit_data <- child_visit_load_data("data/child-visit.csv", tablet_users_data, mobile_users_data)
treatment_data <- child_treatment_load_data("data/child-treatment.csv", mobile_users_data)

GL_n_consults <- format_number(nrow(visit_data))
GL_n_districts <- as.character(length(levels(visit_data$district)))
GL_n_mobile_users <- as.character(length(levels(visit_data$mobile_user)))
GL_n_health_workers <- as.character(nrow(consulting_health_workers(visit_data)))

GL_chart_w = "100%"
GL_chart_h = 400

GL_fist_consult_date = min(visit_data$started_on)
GL_last_consult_date = max(visit_data$started_on)