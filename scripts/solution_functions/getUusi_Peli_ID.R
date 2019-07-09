#getUusi_Peli_ID
#required_data("ADM_PELIT")
# PakkaLeft<-1
# PakkaRight <-11
# getUusi_Peli_ID(ADM_PELIT, PakkaLeft, PakkaRight)
getUusi_Peli_ID <- function(input_STG_PELISTATSIT, PakkaLeft, PakkaRight) {


  #otetaan pienin pelaamaton peli
  peli_min <- suppressWarnings(input_STG_PELISTATSIT[is.na(Voittaja) &

                         Pakka_ID == PakkaLeft &
                         Vastustajan_Pakka_ID == PakkaRight, min(Peli_ID)])
 # op("peli_min")
  #jos ei oo, niin otetaan suurin pelattu
  peli_max <- suppressWarnings(input_STG_PELISTATSIT[!is.na(Voittaja) &
                          Pakka_ID == PakkaLeft &
                          Vastustajan_Pakka_ID == PakkaRight, max(Peli_ID)])
 # op("peli_max")
  result_peli <- ifelse(is.infinite(peli_min), peli_max, peli_min)
  #jos ei oo ikinä pelannu eikä myöskään oo ohjelmassa, niin palauta NA
  result_peli_valid <- ifelse(is.infinite(result_peli), NA, result_peli)
 return(result_peli_valid)
  # op("result_peli_valid")
  #print(result_peli_valid)
}


