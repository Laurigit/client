


#jos alotetaan life counter game, niin nollaa temp_data_storage
observeEvent(input$start_life_counter, {
print("input$start_life_counter painettu")
   Aloitus_DT <- now(tz = "EET")
  Peli_ID  <- eR_Peli_ID()

  req(  input$slider_laurin_mulligan,
        input$slider_martin_mulligan)
  laurin_mull <- input$slider_laurin_mulligan
  martin_mull <- input$slider_martin_mulligan

  muuttujat<-c(
    "Aloitus_DT",
    "Laurin_mulligan",
    "Martin_mulligan",
    "Peli_ID",
    "Aloittaja")
  arvot <- c(
    as.character(Aloitus_DT),
    laurin_mull,
    martin_mull,
    Peli_ID,
    eR_Peli_Aloittaja$a)
  tempData <- data.table(muuttuja = muuttujat, arvo = arvot)

wc(tempData, "../common_data/", "temp_data_storage" )
    required_data("ADM_DI_HIERARKIA")
    dbWriteTable(con, "TEMP_DATA_STORAGE", tempData, append = FALSE, overwrite = TRUE)
    updateData("SRC_TEMP_DATA_STORAGE", ADM_DI_HIERARKIA, input_env = globalenv())

    #nollaa tempdatalaskuri
    tempDataLehtysLaskuri$a <- 0
    start_life_counter_button$value <- 2
}, ignoreInit = TRUE, ignoreNULL = TRUE)




#arvopeli
observeEvent(input$arvo_peli,{
#input$divariRadio <- 1
  #required_data("STG_PELISTATSIT")
 # browser()
  # input <- NULL
  # input$divariRadio <-1
  arvottu_peli_id <- getRandomPeli_ID(STG_PELISTATSIT$data, input$divariRadio)

  paivitaSliderit(arvottu_peli_id,session, STG_PELISTATSIT$data)
})




eR_Peli_Aloittaja <- reactiveValues(a = -1, b = -4)
observe({
#required_data("STG_PELISTATSIT")
req(eR_Peli_ID())

  pelidata <- STG_PELISTATSIT$data[1 != 0]
  result <- pelidata[Peli_ID == eR_Peli_ID(), .(Aloittaja, Omistaja_ID)][Omistaja_ID == "M", Aloittaja]

  eR_Peli_Aloittaja$a <- result
  #0 = Lauri, 1 = martti
} )


eR_UID_UUSI_PELI <- reactive({

  # input$numeric_MA_valinta <- 7
  # input$radio_bo_mode<- FALSE
  # input$radio_pfi_mode <- FALSE
  #create dependency
 # required_data(c("ADM_PELIT", "INT_PFI", "STG_PAKAT", "STG_OMISTAJA", "STAT_VOITTOENNUSTE", "STAT_CURRENT_PAKKA"))
#required_functions("UID_UUSI_PELI")
  #required_data("STG_PELISTATSIT")

tulos <- STG_PELISTATSIT$data
  return(tulos)
})

eR_UID_PAKKA <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
required_functions("UID_PAKKA")
required_data(c("INT_PFI"))
result <-  UID_PAKKA(STG_PELISTATSIT$data,
                                                        INT_PFI,
                                                        input_MA_length = input$numeric_MA_valinta,
                                                        input_BO_mode  = input$radio_bo_mode,
                                                        input_pfi_mode = input$radio_pfi_mode)
return(result)
})

eR_UID_PAKKA_VS <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
                                  required_functions("UID_PAKKA_VS")
                                  result <-  UID_PAKKA_VS(STG_PELISTATSIT$data,
                                                       INT_PFI,
                                                       input_MA_length = input$numeric_MA_valinta,
                                                       input_BO_mode  = input$radio_bo_mode,
                                                       input_pfi_mode = input$radio_pfi_mode)
                                  return(result)
 })

eR_UID_TURNAUS_EV <- eventReactive(tallenna_tulos_ui_update$value, {
  required_data(c("STAT_VOITTOENNUSTE"))
  required_functions("UID_TURNAUS_EV")
  results <- UID_TURNAUS_EV(STG_PELISTATSIT$data, STAT_VOITTOENNUSTE)
  return(results)
}, ignoreNULL = FALSE, ignoreInit = FALSE)

eV_UID_MALLI_KOMPONENTIT <- reactive( {
  required_data("STAT_VOITTOENNUSTE")

  tulos <- UID_MALLI_KOMPONENTIT(STAT_VOITTOENNUSTE,
                                 eR_Peli_ID())
  return(tulos)
})



observeEvent(input$tasuriPeli, {

  uusPeliID <- getTasuriPeli(STG_PELISTATSIT$data)
  paivitaSliderit(uusPeliID, session, STG_PELISTATSIT$data)
})

observeEvent(input$havioPeli, {

  uusPeliID <- getTasuriPeli(STG_PELISTATSIT$data, reverse = TRUE)
  paivitaSliderit(uusPeliID, session, STG_PELISTATSIT$data)
})




tempDataLehtysLaskuri <- reactiveValues(a = 0)



#KOLME UI KOMPONENTTIA KOPIPASTETTY. TEE MUUTOKSET MOLEMPIIN
output$PakkaLeftBox <- renderUI({

  if (!is.na(eR_Peli_ID())) {
    result <- getDeckStats("Lauri", eR_UID_UUSI_PELI(), eR_Peli_ID())
    result_data <- result$data
    image_file_names$Lauri <-  paste0(result_data$Most_same_card, ".jpg")
  #  print("output$PakkaLeftBox")

    #luo riippuvuus
    #print(eR_UID_UUSI_PELI())
    ############
    box(
      solidHeader = FALSE,
      collapsible = FALSE,
      width = NULL,
      boxProfile(
        src = paste0(result_data$Most_same_card, ".jpg"),
        title = result_data$Deck,
        boxProfileItemList(
          bordered = TRUE,
          boxProfileItem(
            title = "Win%",
            description = result_data$`Win%`
          ),
          boxProfileItem(
            title = "Win%-MA",
            description = result_data$`Win%-MA`
          ),
          boxProfileItem(
            title = "Streak",
            description = result_data$Streak
          ),
          boxProfileItem(
            title = "Cards",
            description = result_data$Cards
          ),
          boxProfileItem(
            title = "Shuffle8",
            description = result_data$Shuffle8
          )
        )
      )
     )
  } else {
    "Not scheduled"
  }
})


image_file_names <- reactiveValues(Lauri = NULL, Martti = NULL)

output$PakkaRightBox <- renderUI({
  #luo riippuvuus
  eR_UID_UUSI_PELI()
  if (!is.na(eR_Peli_ID())) {
 # browser()
  ############
  result <- getDeckStats("Martti", eR_UID_UUSI_PELI(), eR_Peli_ID())
  result_data <- result$data
  image_file_names$Martti <-  paste0(result_data$Most_same_card, ".jpg")
  box(
    tags$head(tags$style(HTML('
      .boxProfileItem {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
}
  '))),
    solidHeader = FALSE,
    collapsible = FALSE,
    width = NULL,
    boxProfile(
      src = paste0(result_data$Most_same_card, ".jpg"),
      title = result_data$Deck,
      boxProfileItemList(
        bordered = TRUE,
        boxProfileItem(
          title = "Win%",
          description = result_data$`Win%`
        ),
        boxProfileItem(
          title = "Win%-MA",
          description = result_data$`Win%-MA`
        ),
        boxProfileItem(
          title = "Streak",
          description = result_data$Streak
        ),
        boxProfileItem(
          title = "Cards",
          description = result_data$Cards
        ),
        boxProfileItem(
          title = "Shuffle8",
          description = result_data$Shuffle8
        )
      )
    )
  )
  } else {
    "Not scheduled"
  }

})


output$PakkaVSBox <- renderUI({
  #required_data("UID_UUSI_PELI", TRUE)
  #rm(eR_UID_UUSI_PELI)

  if (!is.na(eR_Peli_ID())) {
  #luo riippuvuus
  (eR_UID_UUSI_PELI())
  ############
  #eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
  result <- getVSStatsHtml(eR_UID_UUSI_PELI(), "Lauri", eR_Peli_ID())
  result_data <- result$data
  #box(HTML(result), background = "aqua", width = NULL, align = "middle")
  box(

    solidHeader = FALSE,
    collapsible = FALSE,
    width = NULL,
    boxProfile(
      src = paste0(result_data$get_aloittaja_image, ".jpg"),
      title = result_data$otsikko,
      boxProfileItemList(
        bordered = TRUE,
        boxProfileItem(
          title = "Win%",
          description = result_data$`Win%`
        ),
        boxProfileItem(
          title = "Win%-MA",
          description = result_data$`Win%-MA`
        ),
        boxProfileItem(
          title = "Streak",
          description = result_data$Streak
        ),
        boxProfileItem(
          title = "Games",
          description = result_data$Games
        ),
        boxProfileItem(
          title = "Prediction",
          description = result_data$Prediction
        )
      )
    )
  )
  } else {
    "Not scheduled"
  }
})


output$peliKesto <- renderText({ aika_text_reactive$aika


})
aika_text_reactive = reactiveValues(aika = 0, i = 0)
observe({
  required_data("ADM_TEMP_DATA_STORAGE")
  tempData <- ADM_TEMP_DATA_STORAGE

  invalidateLater(1000, session)
  pelialkuAika <- tempData[muuttuja == "Aloitus_DT", as.POSIXct(arvo, tz = "EET")]
  aikaNyt <- now(tz = "EET")
  sekunnit_yht<- as.integer(difftime(aikaNyt, pelialkuAika, units = c("secs")))
  minuutit_yht<-floor(sekunnit_yht/60)
  sekunnit<-sekunnit_yht-60*minuutit_yht
  tunnit <- floor(minuutit_yht / 60)
  minuutit <- minuutit_yht - 60 * tunnit
  sekunnit_fix <- str_pad(sekunnit, 2, pad = "0")
  minuutit_fix <- str_pad(minuutit, 2, pad = "0")
  tunnit_text <- ifelse(tunnit > 0,
                        paste0(str_pad(tunnit, 2, pad = "0"),":"),
                        "")

  isolate(tempDataLehtysLaskuri$a <- tempDataLehtysLaskuri$a + 1)
  if ( tempDataLehtysLaskuri$a == 10) {

    shinyjs::addClass(selector = "body", class = "sidebar-collapse")

  }
  if ( tempDataLehtysLaskuri$a == 240) {
    # js$collapse("uusipeli_box")
  }





  aika_text_reactive$aika <- paste0(tunnit_text, minuutit_fix,":",sekunnit_fix)
})

#################### KOPIPASTETTU. TEE MUUTOKSET MOLEMPIIN#
output$EV_plot <- renderPlot({

  melttaa_aggr <-  eR_UID_TURNAUS_EV()
  # required_data(c("ADM_PELIT", "STAT_VOITTOENNUSTE"))
  # melttaa_aggr <- UID_TURNAUS_EV(ADM_PELIT, STAT_VOITTOENNUSTE)
  plot <-ggplot(melttaa_aggr, aes(x = ottelu_id, y = Martin_johto, colour = variable)) + geom_line(size = 1.5) +
    theme_calc() + scale_color_calc()
  plot+ theme(legend.title=element_blank(),
              legend.position = c(0.12, 0.1),
              legend.background = element_rect(color = "black",
                                               fill = "transparent", size = 1, linetype = "solid"))

})
output$EV_plot_ovelary <- renderPlot({

  melttaa_aggr <-  eR_UID_TURNAUS_EV()
  # required_data(c("ADM_PELIT", "STAT_VOITTOENNUSTE"))
  # melttaa_aggr <- UID_TURNAUS_EV(ADM_PELIT, STAT_VOITTOENNUSTE)
  plot <-ggplot(melttaa_aggr, aes(x = ottelu_id, y = Martin_johto, colour = variable)) + geom_line(size = 1.5) +
    theme_calc() + scale_color_calc()
  plot+ theme(legend.title=element_blank(),
              legend.position = c(0.12, 0.1),
              legend.background = element_rect(color = "black",
                                               fill = "transparent", size = 1, linetype = "solid"))

})
#################### KOPIPASTETTU. TEE MUUTOKSET MOLEMPIIN#

output$win_distribution <- renderPlot({
  if (!is.na(eR_Peli_ID())) {
  melttaa <- eV_UID_MALLI_KOMPONENTIT()
  graphs_breaks <- melttaa[, Turnaus_NO]
  plot <- ggplot(melttaa, aes(x = (Turnaus_NO), y = Martin_etu, colour = variable)) + geom_line(size = 1.5) +
    theme_calc() + scale_color_calc()

  plot+ theme(legend.title=element_blank(),
              legend.position = c(0.12, 0.1),
              legend.background = element_rect(color = "black",
                                               fill = "transparent", size = 1, linetype = "solid")) +
    scale_x_continuous(name = "Turnaus_NO",
                       breaks = graphs_breaks) +
    ylim(-0.5,0.50)
  }
})


#select_laurin_pakka
observeEvent(input$select_laurin_pakka,{
    select_laurin_pakka$value <- input$select_laurin_pakka
}, ignoreNULL = TRUE, ignoreInit = TRUE)

 observe({
   updateSelectInput(session,
                     inputId = "select_laurin_pakka", selected = (select_laurin_pakka$value))
 })

 #select_martin_pakka
 observeEvent(input$select_martin_pakka,{
   select_martin_pakka$value <- input$select_martin_pakka
 }, ignoreNULL = TRUE, ignoreInit = TRUE)

 observe({
   updateSelectInput(session,
                     inputId = "select_martin_pakka", selected = (select_martin_pakka$value))
 })



 #toiminnot, kun painetaan nappulaa, mikä käynnistää life_counterin

 observe({

   print("start_life_counter_button$value  triggered")
   print(start_life_counter_button$value )
  # print(start_life_counter_button$value)
   required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_DMG"))
   if (session$user != "overlay" & start_life_counter_button$value > 0) {
     updateTabItems(session,"sidebarmenu", "tab_LifeCounter")
     addClass(selector = "body", class = "sidebar-collapse")
     start_life_counter_button$value <-  isolate(start_life_counter_button$value - 1)

  if (start_life_counter_button$value == 1) {
   write.table(x = ADM_CURRENT_DMG[1 == 0],
               file = paste0("../common_data/", "current_dmg.csv"),
               sep = ";",
               row.names = FALSE,
               dec = ",")
   write.table(x = ADM_CURRENT_TURN[1 == 0],
               file = paste0("../common_data/", "current_turn.csv"),
               sep = ";",
               row.names = FALSE,
               dec = ",")
  }
   required_data("ADM_DI_HIERARKIA")
   updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
   updateData("SRC_CURRENT_TURN", ADM_DI_HIERARKIA, globalenv())
   life_totals$data <-  calc_life_totals(ADM_CURRENT_DMG)
   damage_data$data <- ADM_CURRENT_DMG
   turnData$turn <- 1


  }

 })

