## app.R ##

# Loading libraries

library(shinycustomloader)

library(shiny)
library(readxl)
library(readr)
library(ggpubr)
library(here)
library(scales)
library(RColorBrewer)
library(tidyverse)
# library(lubridate)
library(shinydashboard)
library(plotly)
library(DT)
library(msaR)
library(Biostrings)
library(seqinr)
library(glue)
library(reticulate)
library(fs)
library(report)


# Initializing and preparation #####

source(here::here("initialize.R"), local = TRUE)

source(here::here("prepare_01.R"), local = TRUE)

# Sourcing custom functions #### 

source(here::here("Functions", "ep.map.vis.R"))

# source(here::here("Functions", "find.single.coverages.R"))

# Computing derived constants ####

NUM_GENOMES = length(Acc_IDs)
LATEST <- metadf %>%
  filter(date == metadf$date %>% max()) %>%
  select(`Accession ID`, Country) %>% head(1)
# LATEST_DATE <- lubridate::make_date(year = 2020, month = 3, day = 11)
NUM_B_EPITOPES = dim(df_Bepitopes_REF)[1]
NUM_T_EPITOPES = dim(df_TMHCepitopes_REF)[1]

# titleName <- c("COVIDep: Up-to-date identification of potential vaccine targets for the COVID-19 coronavirus (SARS-CoV-2)",
                    # href='http://mycompanyishere.com',
                    # tags$img(src='UST_L3.png'))
  
# title = tags$a(
#   "COVIDep: Up-to-date identification of potential vaccine targets for the COVID-19 coronavirus (SARS-CoV-2)",
#   img(src='LOGO.png', height = "45px"))


# Header content ####
header <- dashboardHeader(
                      title =  "COVIDep", 
                      titleWidth = 250,
                      # Set height of dashboardHeader
                      # tags$li(class = "dropdown",
                      #         tags$style(".main-header {max-height: 50px}"),
                      #         tags$style(".main-header .logo {height: 50px}")
                      # ),
                          disable = FALSE
                      #, 
                          # titleWidth = "92%",
                # dropdownMenu(type = "notifications",
                #              notificationItem(
                #                text = paste0("Sequences updated on ", UPDATE_DATE),
                #                href = "https://www.gisaid.org/CoV2020/",
                #                icon("database"))
                # )
                )

# Sidebar content ####

source(here::here("sideBar.R"), local = TRUE)

# Body content ####

source(here::here("body.R"), local = TRUE)

# Server #####

source(here::here("server_file.R"), local = TRUE)

ui <- dashboardPage(
  # skin = "black",
  header,
  sidebar,
  body
)

shinyApp(ui, server)