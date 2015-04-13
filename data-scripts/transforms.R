# Heatmap with bubble with googleCharts
# form_activity <- function(data, col_prefix) {
#   col_hour <- paste(col_prefix, "_hour", sep = "")
#   col_weekday <- paste(col_prefix, "_weekday", sep = "")
#   d <- data.frame(table(data[,col_hour], data[,col_weekday]))
#   d <- rename(d, hour = Var1, weekday = Var2, count = Freq)
#   d <- mutate(d, cat = count)
#   d <- mutate(d, id = paste(hour, weekday))
#   d <- mutate(d, hour = unclass(hour) - 1, weekday = unclass(weekday))
#   d <- select(d, id, hour, weekday, cat, count)
#   d
# }

form_activity <- function(data, col_prefix) {
  col_hour <- paste(col_prefix, "_hour", sep = "")
  col_weekday <- paste(col_prefix, "_weekday", sep = "")
  d <- data.frame(table(data[,col_hour], data[,col_weekday]))
  d <- dplyr::rename(d, hour = Var1, weekday = Var2, count = Freq)
  d
}

activity_duration <- function(data) {
  m <- as.data.frame(tapply(data$duration, data$mobile_user, mean))
  f <- plyr::count(data$mobile_user)
  r <- cbind(f, m[,1])
  colnames(r) <- c("mobile_user", "n_forms", "mean")
  r
}

hour_duration <- function(data) {
  tapply(data$duration, data$started_hour, mean)
}

classifications_combinations <- function (data) {
  d <- aggregate(data$n_classifications,
            by=list(oreille = data$classification_oreille, diahree = data$classification_diahree,
                    paludisme = data$classification_paludisme, vih = data$classification_vih,
                    deshydratation = data$classification_deshydratation, pneumonie = data$classification_pneumonie,
                    malnutrition = data$classification_malnutrition, rougeole = data$classification_rougeole,
                    anemie = data$classification_anemie, dysenterie = data$classification_dysenterie),
            FUN=length)
  d <- arrange(d, desc(x))
  d <- dplyr::rename(d, frequency = x)
  d
}

classifications_frequency <- function (data) {
  d <- select(data, starts_with("classification_"))
  d <- lapply(d, function(x) as.data.frame(table(x)))
  n <- sum(d[[1]]$Freq)
  d <- do.call("rbind", d)
  d <- filter(d, x != "---")
  colnames(d) <- c("classification", "frequence")
  d <- mutate(d, pourcentage = round(frequence / n * 100, digits = 2))
  d <- arrange(d, desc(frequence))
  d
}

consulting_health_workers <- function(data) {
  data <- distinct(data, health_worker_id)
  data <- select(data, qualification, district, mobile_user)
  data
}

sync_lag <- function (data, group_by) {
  data <- tapply(data$sync_lag, data[, group_by], mean)
  data <- data / 60 / 24 # Duration in days
  data <- sort(data)
  data
}

format_number <- function(number) {
  format(number, digits = 9, decimal.mark = "'", big.mark = "'",small.mark = ".", , small.interval = 3)
}