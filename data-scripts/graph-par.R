color_above_value <- function(data, value) {
  cc <- rainbow(2)
  col <- c()
  for (i in 1:length(data)) {
    if(data[i] > value) {
      col <- c(col, cc[2])
    } else {
      col <- c(col, cc[1])
    }
  }
  col
}

color_min_max <- function(data) {
  mn <- min(data)
  mx <- max(data)
  col <- c()
  for (i in 1:length(data)) {
    if(data[i] == mn) {
      col <- c(col, "#09A129")
    } else if (data[i] == mx) {
      col <- c(col, "#ff0000")
    } else {
      col <- c(col, "#cccccc")
    }
  }
  col
}