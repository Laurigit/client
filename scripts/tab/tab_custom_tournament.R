#tab_custom_tournament

paivittyva_custom_tournament <- my_reactivePoll(session, "CUSTOM_TOURNAMENT", "SELECT * FROM CUSTOM_TOURNAMENT", 2500, con)

STG_CUSTOM_TOURNAMENT <- reactiveValues(data = data.table(dbSelectAll("CUSTOM_TOURNAMENT", con)))
observe({
  STG_CUSTOM_TOURNAMENT$data <- paivittyva_custom_tournament()

})


observeEvent(input$aloita_custom,{
  req(STG_CUSTOM_TOURNAMENT$data)
click("start_life_counter")
  updateRadioButtons(session,"voittaja_custom",selected = NA)
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  seuraava_peli_rivi <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli]
  stamppi <-  as.character(now(tz = "EET"))

  dbFetch(dbSendQuery(con, paste0('UPDATE CUSTOM_TOURNAMENT SET timestamp = "', stamppi, '" WHERE game_id = ', seuraava_peli_rivi[, game_id])))

})


output$matchup_text <- renderText({

seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
seuraava_peli_rivi <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli]
boksiteksti <- paste0(seuraava_peli_rivi[, vasen], "-", seuraava_peli_rivi[, oikea])
# box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
#                 boksiteksti,
#                                              '</b></font></div>')),
#                                  background = "blue",
#                                  width = NULL)

boksiteksti
})

observeEvent(input$tallenna_tulos_voittaja,{
  req(STG_CUSTOM_TOURNAMENT$data)

  req(input$voittaja_custom)
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  seuraava_peli_rivi <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli]

  dbFetch(dbSendQuery(con, paste0("UPDATE CUSTOM_TOURNAMENT SET voittaja = ", input$voittaja_custom, " WHERE game_id = ", seuraava_peli_rivi[, game_id])))
  updateRadioButtons(session, "aloittaja_custom", selected = -1)

})


output$matchup_tilanne <- renderUI({
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli, match_id]
  matchup_data <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  vas_name <- matchup_data[, .N, by = vasen][, vasen]
  oik_name <- matchup_data[, .N, by = oikea][, oikea]
  vas_voitot <- matchup_data[voittaja == -1, .N]
  oik_voitot <- matchup_data[voittaja == 1, .N]
  tasurit <- matchup_data[voittaja == 0, .N] / 2
  boksiteksti <- paste0(vas_voitot + tasurit, "-", oik_voitot + tasurit)

  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  boksiteksti,
                                               '</b></font></div>')),
                                   background = "purple",
                                   width = NULL)



})

observe({
  #minä vahdin, mitä nappuloita saa painaa
  #eti peli, jossa timestamp, muttei tulosta
  peleja_kesken <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja) & timestamp != "", .N]

if (input$activate_custom == 1) {
  if (peleja_kesken == 1) {
    shinyjs::disable("aloita_custom")
    shinyjs::enable("tallenna_tulos_voittaja")
  } else {
    alottaja_valittu <- input$aloittaja_custom != -1
    if ( alottaja_valittu == TRUE) {
      shinyjs::enable("aloita_custom")
    } else {
      shinyjs::disable("aloita_custom")
    }
    shinyjs::disable("tallenna_tulos_voittaja")
  }
} else {
  shinyjs::disable("aloita_custom")
  shinyjs::disable("tallenna_tulos_voittaja")
}
})

observe({
  #tämä vaan tuhoo 2-0:n jälkeen ylimääräsen rivin
  #1. onko keskenerästä peliä
  #edellinen pelattu peli

  #DEPEND
  input$tallenna_tulos_voittaja
  ##############

  edellinen_pleattu <-   STG_CUSTOM_TOURNAMENT$data[!is.na(voittaja), max(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == edellinen_pleattu, match_id]
  matchupdata <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  peleja_pelattu <- matchupdata[!is.na(voittaja), .N]

  if (peleja_pelattu == 2) {
     if (abs(matchupdata[!is.na(voittaja), sum(voittaja)]) == 2) {
       #rivi tuhotaan. Mikä rivi?
       tuhoa_game_id <- matchupdata[nth_game_of_match == 3, game_id]

      if (length(tuhoa_game_id) > 0) {
         #find to be deleted game_id
        # delete_game_id <- matchupdata[, max(game_id)]
         dbFetch(dbSendQuery(con, paste0("DELETE FROM CUSTOM_TOURNAMENT WHERE game_id = ", tuhoa_game_id)))
      }
     }
  }

})


sarjataulukko <- reactive({


  if (nrow(STG_CUSTOM_TOURNAMENT$data) > 1) {

  aggr_over_bo3_raw <- copy(STG_CUSTOM_TOURNAMENT$data)[!is.na(voittaja), .(pelatut_pelit = .N, sum_voitot = sum(voittaja)), by = .(vasen, oikea, match_id)]
  aggr_over_bo3_raw[, BO_voittaja := ifelse(sum_voitot < 0, -1,
                                        ifelse(sum_voitot > 0, 1, 0))]
  aggr_over_bo3_raw[, is_completed := ifelse((abs(sum_voitot) == 2 & pelatut_pelit == 2) |  pelatut_pelit == 3, 1, 0)]
  aggr_over_bo3 <- aggr_over_bo3_raw[is_completed == TRUE]
 vasurivoitot <- aggr_over_bo3[BO_voittaja == -1, .(Wins = .N), by = .(Player = vasen)]
 oikee_voitot <- aggr_over_bo3[BO_voittaja == 1, .(Wins = .N), by = .(Player = oikea)]
 tasurit <-  aggr_over_bo3[BO_voittaja == 0, .(Draw = .N), by = .(Player = oikea)]
 tasurit_vasen <-  aggr_over_bo3[BO_voittaja == 0, .(Draw = .N), by = .(Player = vasen)]
 vasuritappio <- aggr_over_bo3[BO_voittaja == 1, .(Lost = .N), by = .(Player = vasen)]
 oikee_tappio <- aggr_over_bo3[BO_voittaja == -1, .(Lost = .N), by = .(Player = oikea)]
 bindwin <- rbind(vasurivoitot, oikee_voitot)[, .(type = "Wins", sum = sum(Wins)), by = Player]
 bindlost <- rbind(vasuritappio, oikee_tappio)[, .(type = "Lost", sum = sum(Lost)), by = Player]
 binddraw <- rbind(tasurit, tasurit_vasen)[, .(type = "Draw", sum = sum(Draw)), by = Player]
 tasuridummy <- data.table(Player = "Tasuri", type = "Draw", sum = 1)
 bindall <- rbind(bindwin, bindlost, binddraw, tasuridummy)
 dcasti <- dcast.data.table(bindall, Player ~ type, fun.aggregate = sum, value.var = "sum")
 dcasti[is.na(dcasti)] <- 0
 remove_dummy <- dcasti[Player != "Tasuri", .(Player, Score = Wins * 3 + Draw, Wins, Lost, Draw, Stats = paste0(Wins, "-", Lost, "-", Draw))][order(-Score)]
 remove_dummy
  }
})


#monitoroi nappuoloita
observeEvent(input$activate_custom, {
  isolate(react_custom_tournament$data <- input$activate_custom)

}, ignoreNULL = TRUE)
observe({
  updateRadioButtons(session, "activate_custom", selected = react_custom_tournament$data)

})



observeEvent(input$voittaja_custom, {
isolate(react_custom_tournament$voittaja <- input$voittaja_custom)

}, ignoreNULL = TRUE)

observe({

#  print(react_custom_tournament$voittaja)
  updateRadioButtons(session, "voittaja_custom", selected = react_custom_tournament$voittaja)

})



observeEvent(input$aloittaja_custom, {
  isolate(react_custom_tournament$aloittaja <- input$aloittaja_custom)
  if (react_custom_tournament$aloittaja != -1) {
  #etsi matchuppi, jossa aloittaja oikein

  copi <- copy(STG_PELISTATSIT$data[, .(Omistaja_ID, Peli_ID, Aloittaja, Pakka_NM, Ottelu_ID)][order(Ottelu_ID, Peli_ID, Omistaja_ID)])
  copi[, eka_peli_id := min(Peli_ID), by = Ottelu_ID]
  keep_min <- copi[eka_peli_id == Peli_ID]
  vasen_alottaa_peli_id <- keep_min[Omistaja_ID  == "M" & Aloittaja == 0][1, Peli_ID]
  oikea_aloittaa_peli_id <-  keep_min[Omistaja_ID  == "M" & Aloittaja == 1][1, Peli_ID]
  #keep_min[Peli_ID %in% c(vasen_alottaa_peli_id, oikea_aloittaa_peli_id)]

  if (react_custom_tournament$aloittaja == 0) {
    vasen_pakka <- STG_PAKAT[Pakka_NM == keep_min[Peli_ID == oikea_aloittaa_peli_id & Omistaja_ID == "L", Pakka_NM], Pakka_ID]
    oikea_pakka <- STG_PAKAT[Pakka_NM == keep_min[Peli_ID == oikea_aloittaa_peli_id & Omistaja_ID == "M", Pakka_NM], Pakka_ID]
  } else if (react_custom_tournament$aloittaja == 1) {
    vasen_pakka <- STG_PAKAT[Pakka_NM == keep_min[Peli_ID == vasen_alottaa_peli_id & Omistaja_ID == "L", Pakka_NM], Pakka_ID]
    oikea_pakka <- STG_PAKAT[Pakka_NM == keep_min[Peli_ID == vasen_alottaa_peli_id & Omistaja_ID == "M", Pakka_NM], Pakka_ID]
  }

  updateSelectInput(session,
                    inputId = "select_laurin_pakka", selected = vasen_pakka)
  updateSelectInput(session,
                    inputId = "select_martin_pakka", selected = oikea_pakka)

}

}, ignoreNULL = TRUE)
observe({
  updateRadioButtons(session, "aloittaja_custom", selected = react_custom_tournament$aloittaja)

})



output$vasen_pelaaja_custom <- renderUI({
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli, match_id]
  matchup_data <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  vas_name <- matchup_data[, .N, by = vasen][, vasen]

  vas_stats <- sarjataulukko()[Player == vas_name, Stats]


  boksiteksti <- paste0(vas_name, " ", vas_stats)

  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  boksiteksti,
                  '</b></font></div>')),
      background = "purple",
      width = NULL)

})
output$oikea_pelaaja_custom <- renderUI({
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli, match_id]
  matchup_data <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  oik_name <- matchup_data[, .N, by = oikea][, oikea]

  oik_stats <- sarjataulukko()[Player == oik_name, Stats]


  boksiteksti <- paste0(oik_name, " ", oik_stats)

  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  boksiteksti,
                  '</b></font></div>')),
      background = "purple",
      width = NULL)

})
output$matchuptimer <- renderUI({
  invalidateLater(1000, session)
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli, match_id]
  matchup_data <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]

  if ((matchup_data[nth_game_of_match == 1, timestamp] != "")) {
    matchup_alku <-   matchup_data[nth_game_of_match == 1, as.POSIXct(timestamp, tz = "EET")]
    aikaNyt <- now(tz = "EET")
    sekunnit_yht <- as.integer(difftime(aikaNyt, matchup_alku, units = c("secs")))
    minuutit_yht <- floor(sekunnit_yht / 60)
    sekunnit <- sekunnit_yht - 60 * minuutit_yht
    tunnit <- floor(minuutit_yht / 60)
    minuutit <- minuutit_yht - 60 * tunnit
    sekunnit_fix <- str_pad(sekunnit, 2, pad = "0")
    minuutit_fix <- str_pad(minuutit, 2, pad = "0")
    tunnit_text <- ifelse(tunnit > 0,
                          paste0(str_pad(tunnit, 2, pad = "0"),":"),
                          "")
    mu_teksti <- paste0(tunnit_text, minuutit_fix,":",sekunnit_fix)
  } else {
    mu_teksti <- "Not started"
  }



  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  mu_teksti,
                  '</b></font></div>')),
      background = "purple",
      width = NULL)

})

output$sarjataulukko <- renderDataTable({
  #DEPENDENCY
  input$tallenna_tulos_voittaja
  ########################
  sarjataulukko()[, .(Player, Score, Stats)]

}, options = list(
  searching = FALSE,
  #scrollY = "400px",
  scrollX = FALSE,
  lengthChange = FALSE,
  paging = FALSE,
  bInfo =  FALSE
),
rownames = FALSE
)
