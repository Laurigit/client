#SRC_TEMP_DATA_STORAGE


required_functions("rc")
if (file.exists("../common_data/temp_data_storage.csv")) {
readtds <- rc("temp_data_storage.csv")
} else {
  muuttujat<-c(
    "Aloitus_DT",
    "Laurin_mulligan",
    "Martin_mulligan",
    "Peli_ID")
  arvot <- c(
    as.character(now()),
    0,
    0,
    1)
  tempData <- data.table(muuttuja = muuttujat, arvo = arvot)




  wc(tempData, "../common_data/", "temp_data_storage" )
}

SRC_TEMP_DATA_STORAGE <- readtds
