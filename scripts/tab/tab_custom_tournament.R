#tab_custom_tournament

paivittyva_custom_tournament <- my_reactivePoll(session, "CUSTOM_TOURNAMENT", "SELECT * FROM CUSTOM_TOURNAMENT", 2500, con)

STG_CUSTOM_TOURNAMENT <- reactiveValues(data = data.table(dbSelectAll("CUSTOM_TOURNAMENT", con)))
observe({
  STG_CUSTOM_TOURNAMENT$data <- paivittyva_custom_tournament()

})


observeEvent(input$aloita_custom,{
click("start_life_counter")
  updateRadioButtons(session,"voittaja_custom",selected = -1)
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
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  seuraava_peli_rivi <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli]
  dbFetch(dbSendQuery(con, paste0("UPDATE CUSTOM_TOURNAMENT SET voittaja = ", input$voittaja_custom, " WHERE game_id = ", seuraava_peli_rivi[, game_id])))

})


output$matchup_tilanne <- renderUI({
  seuraava_peli <-   STG_CUSTOM_TOURNAMENT$data[is.na(voittaja), min(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == seuraava_peli, match_id]
  matchup_data <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  vas_name <- matchup_data[, .N, by = vasen][, vasen]
  oik_name <- matchup_data[, .N, by = oikea][, oikea]
 # vas_voitot <- matchup_data[voittaja == -1, .N]
#  oik_voitot <- matchup_data[voittaja == 1, .N]
  boksiteksti <- paste0(vas_voitot, "-", oik_voitot)

  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  boksiteksti,
                                               '</b></font></div>')),
                                   background = "purple",
                                   width = NULL)



})

observe({
  #tämä vaan tuhoo 2-0:n jälkeen ylimääräsen rivin
  #1. onko keskenerästä peliä
  #edellinen pelattu peli
  edellinen_pleattu <-   STG_CUSTOM_TOURNAMENT$data[!is.na(voittaja), max(game_id)]
  nykyinen_match_up <- STG_CUSTOM_TOURNAMENT$data[game_id == edellinen_pleattu, match_id]
  matchupdata <- STG_CUSTOM_TOURNAMENT$data[match_id == nykyinen_match_up]
  peleja_pelattu <- matchupdata[, .N]

  if (peleja_pelattu == 2) {
     if (abs(matchupdata[, sum(voittaja)]) == 2) {
       #rivi tuhotaan. Mikä rivi?
       tuhoa_game_id <- matchupdata[nth_game_of_match == 3, game_id]

         #find to be deleted game_id
         delete_game_id <- matchup_data[, max(game_id)]
         dbFetch(dbSendQuery(con, paste0("DELETE FROM CUSTOM_TOURNAMENT WHERE game_id = ", delete_game_id)))

     }
  }

})


sarjataulukko <- reactive({



  if (nrow(STG_CUSTOM_TOURNAMENT$data) > 1) {
  aggr_over_bo3 <- copy(STG_CUSTOM_TOURNAMENT$data)[!is.na(voittaja), .(pelatut_pelit = .N, sum_voitot = sum(voittaja)), by = .(vasen, oikea, match_id)]
  aggr_over_bo3[, BO_voittaja := ifelse(sum_voitot < 0, -1,
                                        ifelse(sum_voitot > 0, 1, 0))]

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
 remove_dummy <- dcasti[Player != "Tasuri", .(Player, Wins, Lost, Draw, Stats = paste0(Wins, "-", Lost, "-", Draw))]
 remove_dummy
  }
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
