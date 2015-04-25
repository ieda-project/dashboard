library(shiny)
library(leaflet)
library(shinydashboard)

source("ui/header.R")
source("ui/sidebar.R")
source("ui/body.R")

dashboardPage(header, sidebar, body)