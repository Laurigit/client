#options are prod, test, dev
options(shiny.trace = FALSE)
GLOBAL_test_mode <- "prod"
options(shiny.fullstacktrace = FALSE)
if(!GLOBAL_test_mode %in% c("test", "prod", "dev")) {
  stop()
}

dir.create("./external_files/", showWarnings = FALSE)
dir.create("./download_folder/", showWarnings = FALSE)
dir.create("./upload_folder/", showWarnings = FALSE)
dir.create("./all_data_test_upload/", showWarnings = FALSE)
dir.create("./temporary_files/", showWarnings = FALSE)
dir.create("./www/", showWarnings = FALSE)
file.create("./www/favicon.ico")
dir.create("./save_deck_here_from_mtg/", showWarnings = FALSE)
if (!dir.exists("./dmg_turn_files/")) {
  dir.create("./dmg_turn_files/")
}


#library(config)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(lubridate)
library(DT)
library(reshape2)
library(jsonlite)
library(zoo)
library(rvest)
library(curl)
library(stringr)
library(ggplot2)
library(ggthemes)
library(shinyalert)
library(anytime)
library(readxl)
library(readtext)
library(qdapRegex)
library(httr)
library(V8)
library(shinydashboardPlus)
library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
library(beepr)
#library(extendShinyjs)
#library(glob2rx)
#library(shinythemes)
#options(shiny.error=browser)
options(max.print=1000000)
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
Sys.setenv(TZ='EET')
#setwd("C:/Users/laurilepisto/Documents/R/shiny/r2")
#setwd("C:/Users/Lauri/Documents/R/mstat2/code")
#setwd("E:/Pikkuohjelmat/mstat/mstat/code")
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# read it back with readRDS

# Then pass the token to each drop_ function
#drop_acc(dtoken = token)






jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
sourcelist[, rivi := seq_len(.N)]
sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi]
sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

input_kansio_list <- c("utility",
                       "solution_functions",
                       "UID")
for(input_kansio in input_kansio_list) {
  dir_list <- sourcelist[kansio == input_kansio, polku]
  for(filename in dir_list) {
    result = tryCatch({
      print(paste0("sourcing ", filename))
      source(paste0("./scripts/", filename), local = TRUE)
      print(paste0("sourced ", filename))
    }, error = function(e) {
      print(paste0("error in loading file: ", filename))
    })
  }
}



print("Global.R valmis")
