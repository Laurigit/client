#getDeckStats
#Omistaja1 <- "Lauri"
#required_data("UID_UUSI_PELI")
getVSStatsHtml <- function(UID_UUSI_PELI, Omistaja1, input_Peli_ID) {

  stat <- UID_UUSI_PELI[Peli_ID == input_Peli_ID, .(Prediction = round(Prediction, 2) * 100,
                                                           Games = round(Pelit_ABS_VS, 1),
                                  'Winpct' = round(Voitto_PCT_VS, 2) * 100,
                                  'WinpctMA' = round(Voitto_PCT_MA_VS, 2) * 100,
                                  Voitto_PCT_MA_VS_rank,
                                  Voitto_PCT_VS_rank,
                                  Putki_VS_rank,
                                  'Streak' = Putki_VS,
                                  Putki_VS_rank,
                                  Pelit_ABS_VS_rank), by = Omistaja_NM]
  text_stats_temp <- stat[, .(idvar ="idvari",

                                'Win%' = paste0("(", stat[Omistaja_NM == "Lauri", Voitto_PCT_VS_rank], ") ",
                                                stat[Omistaja_NM == "Lauri", Winpct], "-", stat[Omistaja_NM == "Martti", Winpct],
                                                " (", stat[Omistaja_NM == "Martti", Voitto_PCT_VS_rank], ")"),
                                'Win%-MA%' = paste0("(", stat[Omistaja_NM == "Lauri", Voitto_PCT_MA_VS_rank], ") ",
                                         stat[Omistaja_NM == "Lauri", WinpctMA], "-", stat[Omistaja_NM == "Martti", WinpctMA],
                                         " (", stat[Omistaja_NM == "Martti", Voitto_PCT_MA_VS_rank], ")"),
                                Streak = ifelse(Streak > 0,
                                                paste0("(", stat[Omistaja_NM == "Lauri", Putki_VS_rank], ") ", Streak, "-0"),
                                                paste0("0-", -Streak, " (", stat[Omistaja_NM == "Martti", Putki_VS_rank], ")")),
                                Games = paste0(Games, " (", Pelit_ABS_VS_rank, ")"),
                                Prediction = paste0(Prediction, "-" , 100 - Prediction),
                         Omistaja_NM
                                )]
  text_stats_temp <- text_stats_temp[Omistaja_NM =="Lauri"][, Omistaja_NM := NULL]

  aloittaja <- UID_UUSI_PELI[Omistaja_NM == Omistaja1 & Peli_ID == input_Peli_ID, Aloittaja]
  divari_input <- UID_UUSI_PELI[Omistaja_NM == Omistaja1 & Peli_ID == input_Peli_ID, Divari]
  get_aloittaja_image <- aloittaja_image(aloittaja,
                                         divari_input)

  getCardImg(get_aloittaja_image)


  tulos <-suppressWarnings(melt.data.table(text_stats_temp, id.vars = "idvar")[,idvar := NULL])
  # tulos[variable == "Win_pct", color :=  ifelse(value > 50, "purple", "yellow")]
  # tulos[variable == "Win_pct-MA", color :=  ifelse(value > 50, "purple", "yellow")]
  # tulos[variable == "Streak", color :=  ifelse(value > 0, "purple", "yellow")]
  # tulos[variable == "Games", color :=  "blue"]
  #

  tulos[, riviteksti := paste0("<h4>", variable, ": <b>", value, "</b><br>")]
  #  "<h4>", variable, ":<b> ", value, "<b><br/>")]

  ottelu_ID <- UID_UUSI_PELI[Omistaja_NM == Omistaja1 & Peli_ID == input_Peli_ID, Ottelu_ID]
  tilanne <- paste0(UID_UUSI_PELI[Omistaja_NM == "Lauri" & Ottelu_ID == ottelu_ID , sum(Voittaja, na.rm = TRUE)],
                    "-",
                    UID_UUSI_PELI[Omistaja_NM == "Martti" & Ottelu_ID == ottelu_ID, sum(Voittaja, na.rm = TRUE)])
 peleja_jaljella <- nrow(UID_UUSI_PELI[Omistaja_NM == "Lauri" & is.na(Voittaja) & Ottelu_ID == ottelu_ID]) > 0
  # tilanne <- paste0(UID_UUSI_PELI[Omistaja_NM == Omistaja1, Tilanne], "-", UID_UUSI_PELI[!Omistaja_NM == Omistaja1, Tilanne])
  etuliite <- ifelse(UID_UUSI_PELI[Omistaja_NM == Omistaja1  & Peli_ID == input_Peli_ID, Aloittaja] == 1, "*", "")
  takaliite <- ifelse(UID_UUSI_PELI[Omistaja_NM == Omistaja1  & Peli_ID == input_Peli_ID, Aloittaja] == 0, "*", "")
  otsikko <- ifelse(peleja_jaljella == TRUE,
                    paste0(etuliite, tilanne, takaliite), tilanne)
  hmtlout <- paste0("<h3>", otsikko, "<br>", paste0(tulos[, riviteksti], collapse =""))

  text_stats <- cbind(text_stats_temp, get_aloittaja_image, otsikko)

  #hmtlout <- paste0("<h2>",Pakkanimi, "<h2/><br/>", paste0(tulos[, riviteksti], collapse =""))
  #kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
  #                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]

  #kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
  #                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]

  result <- NULL
  result$html <- hmtlout
  result$data <- text_stats

  return(result)
}
