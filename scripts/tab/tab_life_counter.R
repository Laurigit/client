#tab_lifecounter

combat_DMG_reactive <- reactiveValues()
combat_DMG_reactive$combat_dmg <- TRUE
lifegain_DMG_reactive <- reactiveValues("Lifegain" = FALSE)
reverse_DMG_reacive <- reactiveValues("Reverse_DMG" = FALSE)
amount_DMG_reactive <- reactiveValues("dmg" = NULL, "opp" = TRUE)
waiting_opponent_input <- reactiveValues(waiting = FALSE)
opponent_waiting_my_input <- reactiveValues(waiting = FALSE)
inputLife <- reactiveValues(amount = "")
#if user clicked to add previous life
fix_life <- reactiveValues(enabled = FALSE)
#if user has selected to input turn instead of life
turn_overwrite <- reactiveValues(enabled = FALSE, value = "")
#one variable to hold keypad input.
complex_input <- reactiveValues(value = "")
#local variable to keep track turn and only write it once to file
local_turn <- reactiveValues(value = 0)



#reactiveValues to keep up with the status of enabled/disabled actionbuttons
actButtonStatus <- reactiveValues(isEndStep = FALSE,
                                  isMyTurn = FALSE,
                                  editTurnOrLife = FALSE,
                                 ab_Vaihda_vuoro = TRUE,
                                 ab_pakita_endille = TRUE)

setActionButtonStatus <- function(Status, button) {


  if (Status == "enable") {
    shinyjs::enable(button)
    actButtonStatus[[button]] <- TRUE
     #isolate(print(paste0(button, " = ", actButtonStatus[[button]]," ", session$user)))
   } else {
     shinyjs::disable(button)
     actButtonStatus[[button]] <- FALSE
   #  isolate(print(paste0(button, " = ", actButtonStatus[[button]]," ", session$user)))
  }


}

#one time init settings for UI
# shinyjs::disable("isEndStep")
# shinyjs::disable("isMyTurn")
# shinyjs::disable("editTurnOrLife")
 setActionButtonStatus("disable", "isEndStep")
 setActionButtonStatus("disable", "isMyTurn")
 setActionButtonStatus("disable", "editTurnOrLife")
# print("STATSTUSTUAST")
# isolate(print(actButtonStatus[[editTurnOrLife]]))

#trying to be
#observeEvent(input$dmg_settings,{
observe({
listz <- input$dmg_settings
takeReact <-  damage_data$data
#print(session$user)

if ("Lifegain" %in% listz) {
  lifegain_DMG_reactive$Lifegain <- TRUE
} else {
  lifegain_DMG_reactive$Lifegain <- FALSE
}

if ("Non-combat damage" %in% listz) {
  combat_DMG_reactive$combat_dmg <- FALSE
} else {
  combat_DMG_reactive$combat_dmg <- TRUE
}


if ("Reverse Source" %in% listz) {
  reverse_DMG_reacive$Reverse_DMG <- TRUE
  combat_DMG_reactive$combat_dmg <- FALSE
} else {
  reverse_DMG_reacive$Reverse_DMG <- FALSE
}

})
#session <- NULL
#session$user <- "Lauri"
#message(amount_DMG_reactive$dmg, tablifecounter)
observeEvent(input$Lose_1, {
 amount_DMG_reactive$dmg <- 1
 amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_9, {
  updateTabsetPanel(session, "lifeBox", selected = "NinePlusPanel")
  amount_DMG_reactive$opp <- FALSE
})


observeEvent(input$Deal_1, {
  amount_DMG_reactive$dmg <- 1
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_9, {
  updateTabsetPanel(session, "lifeBox", selected = "NinePlusPanel")
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Edit_1, {

  complex_input$value <- paste0(complex_input$value, "1")
})

observeEvent(input$Edit_2, {
   complex_input$value<- paste0(complex_input$value, "2")
})

observeEvent(input$Edit_3, {
   complex_input$value<- paste0(complex_input$value, "3")
})

observeEvent(input$Edit_4, {
   complex_input$value<- paste0(complex_input$value, "4")
})

observeEvent(input$Edit_5, {
   complex_input$value<- paste0(complex_input$value, "5")
})

observeEvent(input$Edit_6, {
   complex_input$value<- paste0(complex_input$value, "6")
})

observeEvent(input$Edit_7, {
   complex_input$value<- paste0(complex_input$value, "7")
})

observeEvent(input$Edit_8, {
   complex_input$value<- paste0(complex_input$value, "8")
})

observeEvent(input$Edit_9, {
   complex_input$value<- paste0(complex_input$value, "9")
})

observeEvent(input$Edit_0, {
   complex_input$value <- paste0(complex_input$value, "0")
})

observeEvent(input$backSpace, {
   complex_input$value <- str_sub(complex_input$value, 1, -2)
})


#jos on vihun vuoro ja combat damage, non-lifelink päällä, niin disabloi dealit

observe({
  req( eR_Peli_ID())
  Aloittaja_all <- getAloittaja(STG_PELISTATSIT$data, eR_Peli_ID())
  Aloittaja <- Aloittaja_all$Aloittaja_NM
  if (Aloittaja == session$user) {
    I_start <- TRUE
  } else {
    I_start <- FALSE
  }

is_my_turn <- I_start == ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn]
  if (is_my_turn == FALSE & lifegain_DMG_reactive$Lifegain == FALSE & combat_DMG_reactive$combat_dmg == TRUE) {
  #disabloi dealit
  shinyjs::disable("Deal_1")
    shinyjs::disable("Deal_2")
    shinyjs::disable("Deal_3")
    shinyjs::disable("Deal_4")
    shinyjs::disable("Deal_5")
    shinyjs::disable("Deal_6")
    shinyjs::disable("Deal_7")
    shinyjs::disable("Deal_8")
    shinyjs::disable("Deal_9")
  } else {

    shinyjs::enable("Deal_1")
    shinyjs::enable("Deal_2")
    shinyjs::enable("Deal_3")
    shinyjs::enable("Deal_4")
    shinyjs::enable("Deal_5")
    shinyjs::enable("Deal_6")
    shinyjs::enable("Deal_7")
    shinyjs::enable("Deal_8")
    shinyjs::enable("Deal_9")


}
})


observe({
  #Amount damage should be the only thing triggering this

  req(amount_DMG_reactive$dmg)
  req(eR_Peli_ID())
  input_TSID <- isolate(turnData$turn)
  isolate(
    if(lifegain_DMG_reactive$Lifegain == TRUE) {
      life_kerroin <- -1
    } else {
      life_kerroin <- 1
    }
  )

  extract_pelidata <- isolate(eR_UID_UUSI_PELI())[Peli_ID == eR_Peli_ID()]

  #jos on menossa damagen erityisfiksaus, niin ota turni inputeista
  if (fix_life$enabled == TRUE) {
    #check who started

    Aloittaja_all <- getAloittaja(STG_PELISTATSIT$data, eR_Peli_ID())
    Aloittaja <- Aloittaja_all$Aloittaja_NM
  #  print(session$user)
    if (Aloittaja == session$user) {
      I_start <- TRUE
    } else {
      I_start <- FALSE
    }
   # print("I_start")
  #  print(paste0(I_start))
    Stareters_turn_boo <- I_start == input$isMyTurn
   # print("Stareters_turn_boo")
  # print(Stareters_turn_boo)
    required_data("ADM_TURN_SEQ")
    turnValue <-  ADM_TURN_SEQ[End_phase == input$isEndStep &
                                 Starters_turn == Stareters_turn_boo &
                                 Turn == as.numeric(turn_overwrite$value), TSID]
  #  print("turnValue")
   # print(turnValue)
    #cleanup
    fix_life$enabled  <- FALSE
    turn_overwrite$value <- ""
    setActionButtonStatus("disable", "isEndStep")
    setActionButtonStatus("disable", "isMyTurn")
    setActionButtonStatus("disable", "editTurnOrLife")
    updateRadioGroupButtons(session,
                            "editTurnOrLife",
                            selected = "Life")

  } else {
    turnValue <- isolate(turnData$turn)
  }



  #uuspeli <- data.table(Omistaja_NM = c("Lauri", "Martti"), Peli_ID_input = 1033)
  #testitulos <- mark_damage(3, "Lauri", 1, TRUE, "Lauri", 1, ADM_CURRENT_DMG, uuspeli)
  tulos <- mark_damage(Amount = amount_DMG_reactive$dmg * life_kerroin,
                       Opponent_target = isolate(amount_DMG_reactive$opp),
                       Combat_dmg =  isolate(combat_DMG_reactive$combat_dmg),
                       Reverse_source = isolate(reverse_DMG_reacive$Reverse_DMG),
                       input_session_user = session$user,
                       input_TSID = turnValue,
                       current_dmg = isolate(damage_data$data),
                       input_UID_UUSI_PELI = extract_pelidata
  )
 # print(tulos)

 isolate(amount_DMG_reactive$dmg <- NULL)


# print(input$dmg_settings)
  damage_data$data <- tulos
  js$beep()


})


#observeEvent damage_data$data
#handles changed in damagedate
observe({

  print("dmgdata triggereds")
  #jos muuttuu, niin validoi ja kirjota, jos validi
 # print(damage_data$data)
  templife <- calc_life_totals(damage_data$data)
  # print(templife)
  #validate input
  if(isolate(templife$count_missing_rows == 0)){
print("isolate(templife$count_missing_rows")
    print(isolate(templife$count_missing_rows))
   # print("kirjotetaan csv")
    #print(damage_data$data)
    #write to csv


    wc(damage_data$data, folder = "../common_data/", "current_dmg")
    peli_id_to_db <- isolate(eR_Peli_ID())

    if (is.na(peli_id_to_db)) {
      peli_id <- 0
    } else {
      peli_id <- peli_id_to_db
    }

    dbFetch(dbSendQuery(con, paste0("DELETE FROM CURRENT_DAMAGE WHERE Peli_ID = ", peli_id)))
    dbWriteTable (con, "CURRENT_DAMAGE", damage_data$data, overwrite = FALSE, append = TRUE, row.names = FALSE)
    # write.table(x = damage_data$data,
    #             file = paste0("./dmg_turn_files/", "current_dmg.csv"),
    #             sep = ";",
    #             row.names = FALSE,
    #             dec = ",")
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())


    life_totals$data <- templife
    input_error$error <- FALSE
    waiting_opponent_input$waiting <- FALSE
    opponent_waiting_my_input$waiting <- FALSE
    updateTabsetPanel(session, "lifeBox", selected = "life_input")
    updateTabsetPanel(session, "lifes_or_waiting", selected = "chartti2")
  } else if (isolate(templife$count_missing_rows == 2)) {
    #error
    #show both players input and let them choose the correct.
    input_error$error <- TRUE
    #message("input error",  input_error$error)
    waiting_opponent_input$waiting <- FALSE
    opponent_waiting_my_input$waiting <- FALSE

  } else if (tail(damage_data$data, 1)[, Input_Omistaja_NM] == session$user ) {
    #if I did the input, then I am waiting opponent
    waiting_opponent_input$waiting <- TRUE
    opponent_waiting_my_input$waiting <- FALSE
  } else if (tail(damage_data$data, 1)[, Input_Omistaja_NM] != session$user ) {
    #if I did not do the input, then opponent is waiting mine
    waiting_opponent_input$waiting <- FALSE
    opponent_waiting_my_input$waiting <- TRUE
  } else if (isolate(templife$count_missing_rows > 2)) {
    input_error$error <- FALSE
    waiting_opponent_input <- FALSE
    opponent_waiting_my_input$waiting <- FALSE
    damage_data$data <- templife$accepted_rows
    print("käytiin debugissa, missä kojrattiin tilanne, että oli 3 tai enemmän hyväksymätöntä damageinputtia")

  }


})

#obseveEVent input_error$error
observe({
  if (session$user != "overlay") {
   if (input_error$error == TRUE) {
    #calc error
    #debug
    #damage_data <- NULL
    #damage_data$data <- ADM_CURRENT_DMG
   # print("dmg data")
 #  print(damage_data$data)
   # choose_input <- calc_life_totals(isolate(damage_data$data))$input_error
  #  print("choose_input")
  # print(choose_input)

    lifetdata <- calc_life_totals(isolate(damage_data$data))
    accRows <- lifetdata$accepted_rows
    damage_data$data <- accRows
    input_error$error <- FALSE

    js$toot()
       shinyalert(
                 title = "Difference in damage input",
                  text = "Input has been deleted",
                  type = "warning",
                  closeOnClickOutside = FALSE,
                  closeOnEsc = FALSE,
                  showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                  confirmButtonText = "Ok, we will both input again!")
    }

    #updateTabsetPanel(session, "lifeBox", selected = "dmg_rows_panel")

    #print(damage_data_for_observe)
   # print(input_error_response$response)
  #   shinyalert(callbackR = function(x) {
  #     new_row <- data.table(user = session$user, response = x, done = FALSE)
  #     input_error_response$response <- (rbind(input_error_response$response , new_row))
  #   },
  #             title = "Difference in damage input",
  #              text = paste0(choose_input[, text], collapse = "\n"),
  #              type = "warning",
  #              closeOnClickOutside = FALSE,
  #              closeOnEsc = FALSE,
  #              showCancelButton = TRUE,
  #              showConfirmButton = TRUE,
  #              confirmButtonText = "Accept opponent input",
  #              cancelButtonText = "My input is correct")
   }
 # print( input_error_response$response)
})


    #tää jäi loooppiin, pitäs ehkä siirtää niin, että input$shiny alert on omassa observessa ja se muuttaa input_errorresponsen arvoja
    #print( input_error_response$response )
  observe({
    req(input_error_response$response)
    damage_data_for_observe <- isolate(damage_data$data)[DID > 0]



                 if (nrow(input_error_response$response) == 2) {
                   if (input_error_response$response[1, response] != input_error_response$response[2, response]
                       ) {
                     print("TRUEN puoelella")
                     #case inputit meni oikein eli oltiin samaa mieltä virheestä
                     ##tallenna valittu inputti
                     ###tuhoa oma viimeisin ja lisää sinne vihun asetukset omalla inputilla

                     my_response <- input_error_response$response[user  == session$user, response]
                     if (my_response == TRUE) {
                     #  "Accept opponent input"
                       # damage_data_for_observe <- ADM_CURRENT_DMG
                       # session <- NULL
                       # session$user <- "Lauri"
                       damage_undone <- isolate(undo_damage(damage_data_for_observe, session$user))

                       row_to_copy <- damage_undone[which.max(DID)]
                       row_to_copy[, ':=' (Input_Omistaja_NM = session$user,
                                           DID = max(DID) + 1)]
                       damage_data$data <- rbind(damage_undone, row_to_copy)

                     }
                     #debug
                     #input_error_response <- NULL
                     #input_error_response$response <- data.table(user = c("Lauri", "Martti"), response = c(TRUE, FALSE))

                     # tuhottava <- damage_data_for_observe[Input_Omistaja_NM ==
                     #                    input_error_response$response[response ==
                     #                                                    FALSE,
                     #                                                  user], .(DID = max(DID)), by = .(Input_Omistaja_NM)]
                     # message("Tuhottava rivi", tuhottava)


                     # print(data_mihin_lisatty)
                     # life_totals$data <- calc_life_totals(data_mihin_lisatty)

                   } else {

                     #case inputit meni väärin
                     #tuhoa oma viimeisin input ja ilmoita pelaajalle
                   #  print("elsen puolella")
                     tuhottava <- damage_data_for_observe[Input_Omistaja_NM == session$user, .(DID = max(DID))]
                #   print(tuhottava)
                    damage_data$data <- damage_data_for_observe[!DID %in% tuhottava[, DID]]
                   # print(damage_data$data)
                   #  life_totals$data <- isolate(calc_life_totals(damage_data$data))
                     #print( life_totals$data)
                     shinyalert(title = "Damage disagreement",
                                text = "Both inputs have been deleted. Please input damage again",
                                type = "error",
                                closeOnClickOutside = TRUE,
                                closeOnEsc = TRUE,
                                showCancelButton = TRUE,
                                showConfirmButton = TRUE,
                                confirmButtonText = "Sorry, my bad",
                                cancelButtonText = "He's a drunk idiot")
                   }
                   #when second user confirms, delete global var row

                   if (input_error_response$response[1, done ] == FALSE) {
                      isolate(input_error_response$response[1, done := TRUE ])
                   } else {
                     input_error_response$response <- NULL
                   }
                 }


})


observe({
  if (waiting_opponent_input$waiting  == FALSE) {
    updateTabsetPanel(session, "lifeBox", selected = "life_input")
  }
})

#observe lifetotals$data


output$life_total_row <- renderUI({
  req(life_totals$data)


 # print(session$user)
# print(life_totals$data)
  lifedata <- life_totals$data$Lifetotal
  lifetext <- life_totals$data$dmg_text
  #tags$style(HTML('#hello2 {font-family:"Courier",Georgia,Serif; background-color:pink}'))

  fluidRow(column(width = 4,
                  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                            lifedata[Omistaja_NM == session$user, Life_total],
                            '</b></font></div>')),
                                         background = "black",
                      width = '100%'
                      )
                 ),
           column(4,


            #  height: 100%;  background-color: #000080;
           # column(4,
                  actionButton("ab_pakita_endille", HTML("Reject turn"),
                               width = '100%', style='font-size:150%;
                               color: #fff; padding:4px;
                               font-size: 6;
                               height: 87px;
                               background-color: #000080;'
           #)
           )),
           column(4,
                  (box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                   lifedata[Omistaja_NM != session$user, Life_total],
                                   '</b></font></div>')),
                       background = "black",
                       width = '100%')
                 )
            )
           )


                 # valueBox( lifedata[Omistaja_NM == session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)),
          # column(5, offset = 2,
           #       valueBox( lifedata[Omistaja_NM != session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)))
})

output$dynamic_turn_box <- renderUI({
  required_data("ADM_TURN_SEQ")
  if ( turnData$turn > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == turnData$turn, Turn_text]
    if (isolate(eR_Peli_Aloittaja$a) == 0) {
      Aloittaja <- "L"
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    vuoroTeksti <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  } else {
    vuoroTeksti <- "Not started"
  }

  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                  vuoroTeksti,
                  '</b></font></div>')),
      background = "maroon",
      width = "100%")

})


# output$pass_turn_row <- renderUI({
#
#
#   fluidRow(
#
#     column(4,
#
#              actionButton(inputId = "ab_Vaihda_vuoro_virhe",
#                           label = HTML("End turn <br> add mistake"),
#                           style = "font-size:150%; color: #fff; background-color: #000080; border-color: #2e6da4; height: 87px;",
#                           width = '100%')
#            ),
#
#
#
#            )
#     )
#   )
#
# })

observeEvent(input$ab_Vaihda_vuoro, {
  js$no()
  turnData$turn <- ADM_TURN_SEQ[TSID == turnData$turn, Next_turn_TSID]
  local_turn$value <- local_turn$value + 1
})

observeEvent(input$ab_pakita_endille, {
  turnData$turn <- turnData$turn - 1
  local_turn$value <- local_turn$value + 1

})


#observe localTurn
observe({
  required_data("ADM_CURRENT_TURN")
  print("localturn triggered")

  req(eR_Peli_ID())
take_dep <-  local_turn$value
#print("writing to turn csv")
  new_row <- data.table(TSID = isolate(turnData$turn),
                        Peli_ID = isolate(eR_Peli_ID()),
                        time_stamp =  as.character(now(tz = "EET")))

  #  print(str(new_row))
   # print(str(ADM_CURRENT_TURN))
   # print("rbinding")
    #print(rbind(ADM_CURRENT_TURN, new_row))
    #print("done")
  new_data <- rbind(ADM_CURRENT_TURN, new_row)
  wc(new_data, "../common_data/", "current_turn")
  # write.table(x = new_data,
  #             file = paste0("./dmg_turn_files/", "current_turn.csv"),
  #             sep = ";",
  #             row.names = FALSE,
  #             dec = ",")
  required_data("ADM_DI_HIERARKIA")
  peli_id_to_db <- isolate(eR_Peli_ID())
  if (is.na(peli_id_to_db)) {
    peli_id <- 0
  } else {
    peli_id <- peli_id_to_db
  }

  dbFetch(dbSendQuery(con, paste0("DELETE FROM CURRENT_TURN WHERE Peli_ID = ", peli_id)))
  dbWriteTable (con, "CURRENT_TURN", new_data, overwrite = FALSE, append = TRUE, row.names = FALSE)
  updateData("SRC_CURRENT_TURN", ADM_DI_HIERARKIA, globalenv())

}, priority = -100)


waiting_timer <- reactiveValues(data = FALSE, counter = 0)

observeEvent(waiting_timer$counter, {

  templife <- calc_life_totals(damage_data$data)
  if (templife$count_missing_rows == 1 & opponent_waiting_my_input$waiting == TRUE) {
    updateTabsetPanel(session, "lifes_or_waiting", selected = "waiting2")
  }
}, ignoreInit = TRUE)

output$waiting_image <- renderUI({



  if (session$user == "Lauri") {
    kuva <-     image_file_names$Lauri
  } else {
    kuva <-     image_file_names$Martti
  }
  if (is.null(kuva)) {kuva <- "Scion of the Wild.jpg"}
  fluidRow(img(src= kuva, align = "middle"))

})

observeEvent(opponent_waiting_my_input$waiting, {
  req(opponent_waiting_my_input$waiting)


print("alussa")

  if (opponent_waiting_my_input$waiting == TRUE) {

       delay(3000, waiting_timer$counter <- waiting_timer$counter + 1)

    }

}, ignoreInit = TRUE)

#handle ui status based on turndata and life_input
observe({
  required_data("ADM_TURN_SEQ")
  print("turndata triggered")
if(turnData$turn > 0) {
  print("Käyty sörkkimässä UITA.  Waiting = ")
  #print(waiting_opponent_input$waiting )
  #print("turndata$turn")
 # print(turnData$turn)
  if (waiting_opponent_input$waiting == TRUE) {
    updateTabsetPanel(session, "lifeBox", selected = "waiting_panel")
    setActionButtonStatus("disable", "ab_Vaihda_vuoro")
    setActionButtonStatus("disable", "ab_pakita_endille")
  #  shinyjs::disable("ab_Vaihda_vuoro_virhe")
  } else {
     Aloittaja_all <- getAloittaja(STG_PELISTATSIT$data, eR_Peli_ID())
     Aloittaja <- Aloittaja_all$Aloittaja_NM
    if (Aloittaja == session$user) {
      I_start <- TRUE
    } else {
      I_start <- FALSE
    }
    print("turndata$turn, jos 0 niin kaatuu")
    print(turnData$turn)

  my_turn <- I_start == ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn]
  end_phase <-  ADM_TURN_SEQ[TSID == turnData$turn, End_phase]
  if (my_turn == TRUE & end_phase == FALSE) {
    #print("my_turn == TRUE & end_phase == FALSE")
    setActionButtonStatus("enable", "ab_Vaihda_vuoro")
  #  shinyjs::enable("ab_Vaihda_vuoro_virhe")
    setActionButtonStatus("enable", "ab_pakita_endille")
  } else if  (my_turn == TRUE & end_phase == TRUE) {
   # print("my_turn == TRUE & end_phase == TRUE")
    setActionButtonStatus("enable", "ab_Vaihda_vuoro")
 #   shinyjs::enable("ab_Vaihda_vuoro_virhe")
    setActionButtonStatus("disable", "ab_pakita_endille")

  } else {
  #  print("ELSE")
    setActionButtonStatus("disable", "ab_Vaihda_vuoro")
    setActionButtonStatus("disable", "ab_pakita_endille")
 #   shinyjs::disable("ab_Vaihda_vuoro_virhe")
  }
  if (end_phase == TRUE) {
    updateCheckboxGroupButtons(session,
                               "dmg_settings",
                               selected = c("Non-combat damage"))
    #älä säädä ennen ku inputit hyväksytty.
  } else if (waiting_opponent_input$waiting == FALSE &  opponent_waiting_my_input$waiting == FALSE) {
    #check if on current turn, combat damage has been done
    #turnData <- NULL
    #turnData$turn <- 10
    current_turn <- ADM_TURN_SEQ[TSID == turnData$turn, Turn]
    is_starters_turn <- ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn]
    #one value in total coming as a result
    get_current_turn_TSID <- ADM_TURN_SEQ[Turn == current_turn & Starters_turn == is_starters_turn & End_phase == FALSE, TSID]
    #damage_data <- NULL
    #damage_data$data <- ADM_CURRENT_DMG
    #check if in current turn, combat damage is done

    aggr_dmg <-   damage_data$data[Amount >= 0 , .(Combat_dmg = max(Combat_dmg), rivit = .N), by = .(TSID)]

   # print(aggr_dmg)
    #combat damage non lifegain rows

    cmbt_damage_rows <- aggr_dmg[rivit > 1 & TSID == get_current_turn_TSID & Combat_dmg == 1]

   # print(cmbt_damage_rows)
    if (nrow(cmbt_damage_rows) > 0) {
      cmbt_dmg_done <- TRUE
    } else {
      cmbt_dmg_done <- FALSE
    }
    if (cmbt_dmg_done == TRUE) {

    updateCheckboxGroupButtons(session,
                               "dmg_settings",
                               selected = c("Non-combat damage"))
    } else {
      updateCheckboxGroupButtons(session,
                                 "dmg_settings",
                                 selected = c(""))
    }
  }




  }
  }
}, priority = -10)


observeEvent(input$ab_Undo,{
  damage_data$data <- undo_damage(damage_data$data, session$user)
})

observeEvent(input$ab_Vaihda_vuoro_virhe, {
  if (session$user == "Lauri") {
   # input$laurin_virhe <- input$laurin_virhe + 1
    click("laurin_virhe")
  } else {
    click("martin_virhe")
   #input$martin_virhe <- input$martin_virhe + 1
  }
  # turnData$turn <- ADM_TURN_SEQ[TSID == turnData$turn, Next_turn_TSID]
  # local_turn$value <- local_turn$value + 1
})

observeEvent(input$save_9_damage, {

  amount_DMG_reactive$dmg <- as.numeric(inputLife$amount)
  complex_input$value <- ""
  inputLife$amount <- ""


})
output$value_type_life <- renderUI({
  inputValue <- inputLife$amount
  if (fix_life$enabled == TRUE) {
    turnValue <- turn_overwrite$value
  } else {
    required_data("ADM_TURN_SEQ")
    turnValue <- ADM_TURN_SEQ[TSID == turnData$turn, Turn]
  }

Tot_text <- HTML(paste0('<font size="3"', inputValue, ' T:', turnValue, '</font>'))
Tot_text <- HTML(paste0('<div align="center"><font size="6" color="white"> <b>',
                        inputValue, ' T:', turnValue,
                  '</b></font></div>'))
#Tot_text <- "test"
#TurnText <- paste0("Turn: ", turnValue)

  box(Tot_text,
          # subtitle = NULL,
           background = "aqua",
           width = 12,
      height = "60px",
      collapsible = FALSE)
})

output$damage_rows_dt <- renderDataTable({

#  required_data("ADM_CURRENT_DMG")
  damage_data$data[, .N, by = .(Amount,
                       Target_player,
                       Dmg_source,
                       Combat_dmg,
                       TSID)][, N := NULL]
}, options = list(
  searching = FALSE,
  #scrollY = "400px",
  scrollX = FALSE,
  lengthChange = FALSE,
  paging = TRUE,
  bInfo =  FALSE
  ),
rownames = FALSE
)

observeEvent(input$Delete_dmg_row,{
  required_functions("delete_damage")
  #times too as the visible data has been aggregated
  damage_data$data <- delete_damage(input$damage_rows_dt_rows_selected * 2,
                                    damage_data$data)
})

observeEvent(input$ab_fix_lifes, {
  fix_life$enabled <- TRUE
  setActionButtonStatus("enable", "isEndStep")
  setActionButtonStatus("enable", "isMyTurn")
  setActionButtonStatus("enable", "editTurnOrLife")
  updateTabsetPanel(session, "lifeBox", selected = "NinePlusPanel")

  updateRadioGroupButtons(session,
                          inputId = "isMyTurn",
                          selected = TRUE)

  updateRadioGroupButtons(session,
                          inputId = "isEndStep",
                          selected = FALSE)

    updateRadioGroupButtons(session,
                          inputId = "editTurnOrLife",
                          selected = "Turn")
})


#observeEvent complex_input
observe({
  if (complex_input$value == "") {
    use_value <- 0
  } else {
    use_value <- complex_input$value
  }
  if (isolate(input$editTurnOrLife == "Turn")) {
 #   print("write turns")
    turn_overwrite$value <- use_value
  } else {
#    print("write life")
   inputLife$amount <-   use_value
  }

})
#
#
#   input$isEndStep
#   input$isMyTurn
#
#   turn_overwrite$enabled <-



observeEvent(input$editTurnOrLife, {


  if (input$editTurnOrLife == "Turn") {
    turn_overwrite$enabled <- TRUE
    isolate(complex_input$value <- turn_overwrite$value )
  } else {
    turn_overwrite$enabled <- FALSE
    isolate(complex_input$value <-   inputLife$amount )
  }

}, ignoreInit = TRUE, ignoreNULL = TRUE)




###########################
#RENDER LIFE CHART
###########################

output$lifeChart <- renderPlot({
  required_data(c("ADM_TURN_SEQ", "ADM_CURRENT_DMG"))
  graphInput <- isolate(damage_data$data)
  # session <- NULL
  # session$user <- "Lauri"
  #graphInput <- ADM_CURRENT_DMG
  only_my_input <- graphInput[Input_Omistaja_NM == session$user]

 take_dependency <- life_totals$data
splitL <- only_my_input[Target_player == "Lauri"]
splitM <- only_my_input[Target_player == "Martti"]
dtLCurr_Turn <- splitL[ADM_TURN_SEQ, on = "TSID"][, Target_player := ifelse(is.na(Target_player), "Lauri", Target_player)]
dtMCurr_Turn <- splitM[ADM_TURN_SEQ, on = "TSID"][, Target_player := ifelse(is.na(Target_player), "Martti", Target_player)]
appendForProcessing <- rbind(dtLCurr_Turn, dtMCurr_Turn)
appendForProcessing[, Amount := ifelse(is.na(Amount), 0, Amount)]
#aloittajaNo <- "Lauri"
aloittajaNo <- isolate(eR_Peli_Aloittaja$a)
if(aloittajaNo == 0) {
  Aloittaja <- "Lauri"
} else if (aloittajaNo == 1){
  Aloittaja <- "Martti"
} else {
  Aloittaja <- "RIKKI"
}


starting_life <-  20

# tse <- ADM_TURN_SEQ
# join_tse <- graphInput[ADM_TURN_SEQ, on = "TSID"]
# join_tse[,':=' (Amount = ifelse(is.na(Amount), 0, Amount))]
#only for simul
join_tse <- appendForProcessing
join_tse[, Combat_dmg := ifelse(End_phase == TRUE, 0, Combat_dmg)]
####
join_tse[, Target_Player_Turn := (Target_player == Aloittaja) == Starters_turn]
aggr_dmg <- join_tse[, .(Amount = sum(Amount, na.rm =TRUE), TSID = min(TSID)
),
by = .(

  Target_player,
  # Dmg_source,
  # Combat_damage,

  Turn,
  Target_Player_Turn,Starters_turn)]
aggr_dmg[, lifegain := ifelse(Amount < 0, TRUE, FALSE)]
aggr_dmg[, half_turn := (Turn  + ifelse(Starters_turn == TRUE, 0, 0.5))]
aggr_dmg[, cum_dmg := cumsum(Amount), by = Target_player]
aggr_dmg[, cum_lifetotl := starting_life - cum_dmg]
aggr_dmg[, cum_life_start_of_turn := cum_lifetotl + Amount]
#aggr_dmg <- aggr_dmg[order(TSID)]



aggr_dmg[, ':=' (ymax = cum_life_start_of_turn,
                 xmin = half_turn,
                 xmax = half_turn + 0.5,
                 ymin = cum_lifetotl,
                 value = Amount)]

#
# turnData <- NULL
# turnData$turn <- 50


aggr_dmg_cut <- aggr_dmg[TSID <= (isolate(turnData$turn) + 2)]
#aggr_dmg_cut[, bg_color_group := ifelse()]


aggr_dmg_me <- aggr_dmg_cut[Target_player == isolate(session$user)]
aggr_dmg_opponent <- aggr_dmg_cut[Target_player  != isolate(session$user)]
plot_ymax <- max(aggr_dmg_me[, max(ymax)], 20)
plot_xmax <- aggr_dmg_me[, max(half_turn)]
plot_ymin <- min(aggr_dmg_opponent[, min(-ymax)], -20)


# fill=paste0(Starters_turn, "-") sen takia, että muuten TRUE ja FALSE mäppääytyy samaks väriks.
aggr_dmg_me %>%
  arrange(half_turn) %>%
  ggplot() +
  geom_rect(aes(ymin=plot_ymin, ymax=plot_ymax, xmin=xmin,
                                    xmax=xmax, fill=paste0(Starters_turn, "-")), alpha =0.3) +# scale_fill_manual(values = c("red", "green4",  "white", "blue"))+
  geom_rect(aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                fill =  factor(lifegain, levels = c("TRUE", "FALSE", "FALSE-", "TRUE-"))), show.legend = FALSE) +# scale_fill_manual(values = c("red",  "white", "green4", "blue")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(plot_ymin, plot_ymax),
                     breaks = c(seq(0, plot_ymin, by = -5), seq(0, plot_ymax, by = 5)),
                     minor_breaks = NULL) +#c(seq(0, plot_ymin, by = -1), seq(0, plot_ymax, by = 1))) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, plot_xmax) , breaks = seq(1:plot_xmax)) +
  # theme(panel.grid.major = element_line(color = "red")) +
  # ylim(min = 0, max = 20) +
  # geom_segment(aes(x = xArrow, y = ymax, xend = xArrow, yend = ymin),
  #               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  geom_line(aes(half_turn, (cum_life_start_of_turn)), col = "dodgerblue4", size = 1)  -> p1

#p2 <- p1 +  expand_limits(x=c(1,10), y=c(0,20))
colors <- c("red", "green4", "grey" , "white")
names(colors) = c("FALSE", "TRUE", "FALSE-", "TRUE-")

p3 <- p1 +  geom_hline(yintercept = 0) +
  geom_rect(data = aggr_dmg_opponent, aes(xmin = xmin,
                                          xmax = xmax,
                                          ymin = -ymin,
                                          ymax = -ymax,
                                          fill =  factor(lifegain)), show.legend = FALSE) + scale_fill_manual(values = colors) +
  geom_line(data = aggr_dmg_opponent, aes(half_turn, (-cum_life_start_of_turn)), col = "dodgerblue4", size = 1) +
   theme(legend.position = "none") +
 guides(fill=FALSE) +
  theme(axis.title.x=element_blank()) +
theme(axis.title.y=element_blank())
p3


})


#observaa, jos joku voittaa
observe({
 # life_totals <- NULL
 # life_totals$data <- calc_life_totals(ADM_CURRENT_DMG)
  required_data("ADM_TURN_SEQ")
  minLife <- life_totals$data$Lifetotal[, min(Life_total)]

  if (minLife <= 0) {
    aloittajaNo <- isolate(eR_Peli_Aloittaja$a)

    if (aloittajaNo == 0) {
      mulligan_lkm  <- isolate(input$slider_laurin_mulligan)
      # print(paste0(input$slider_vuoroarvio, " + ", input$slider_laurin_mulligan, " - 6 = ", vuoroarviolasku))

    } else {
      mulligan_lkm  <- isolate(input$slider_martin_mulligan)
      # print(paste0(input$slider_vuoroarvio, " + ", input$slider_martin_mulligan, " - 6 = ", vuoroarviolasku))
    }


    #print("turn")
  #  print(isolate(turnData$turn))
  #  print("mulligan")
   # print(mulligan_lkm)

    update_value <- ADM_TURN_SEQ[TSID == isolate(turnData$turn), Turn] + 6 - mulligan_lkm


    #print("peli tallentuu, mika arvo")
   # isolate(print(slider_vuoroarvio$value))
    slider_laurin_lifet$value <- life_totals$data$Lifetotal[Omistaja_NM == "Lauri", Life_total]
    slider_martin_lifet$value <-   life_totals$data$Lifetotal[Omistaja_NM == "Martti", Life_total]
    if (life_totals$data$Lifetotal[which.min(Life_total), Omistaja_NM] == "Lauri") {

     # click("martti_voitti")
      react_martti_voitti$value <- isolate(react_martti_voitti$value + 1)
      slider_vuoroarvio$value <- update_value
     # updateSliderInput(session,
    #                    inputId = "slider_vuoroarvio", value = update_value)
    } else {
      #click("lauri_voitti")
      react_lauri_voitti$value <- isolate(react_lauri_voitti$value + 1)
      slider_vuoroarvio$value <- update_value
    #  updateSliderInput(session,
   #                     inputId = "slider_vuoroarvio", value = update_value)
    }
  }
})
