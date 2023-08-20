



################
#Life counter data
#turn = TSID, Atual trn = turn in magic game.
turnData <- reactiveValues(turn = 1)

required_data("ADM_DI_HIERARKIA")
updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
required_data("ADM_CURRENT_DMG")
life_totals <- reactiveValues(data = calc_life_totals(ADM_CURRENT_DMG))
damage_data <- reactiveValues(data = ADM_CURRENT_DMG)
input_error <- reactiveValues(error = FALSE)
input_error_response <- reactiveValues(response = NULL)

################GLOBAL UI control
slider_laurin_mulligan <-  reactiveValues(value = 0)
slider_martin_mulligan <-  reactiveValues(value = 0, muuttaja = 0)
slider_laurin_virhe <-  reactiveValues(value = 1)
slider_martin_virhe <-  reactiveValues(value = 1)
slider_laurin_landit <-  reactiveValues(value = 0)
slider_martin_landit <-  reactiveValues(value = 0)
slider_laurin_lifet <-  reactiveValues(value = 0)
slider_martin_lifet <-  reactiveValues(value = 0)
slider_vuoroarvio <-  reactiveValues(value = 0)
slider_laurin_kasikortit <-  reactiveValues(value = -1)
slider_martin_kasikorit <-  reactiveValues(value = -1)
tallenna_tulos_ui_update <-  reactiveValues(value = 0)
tallenna_tulos_ui_launch <- reactiveValues(value = 0)
start_life_counter_button <- reactiveValues(value = 0)


select_laurin_pakka <- reactiveValues(value = NULL)
select_martin_pakka <- reactiveValues(value = NULL)
react_lauri_voitti<- reactiveValues(value = 0)
react_martti_voitti <- reactiveValues(value = 0)

#CUSTOM TOUR
react_custom_tournament <- reactiveValues(value = 0, aloittaja = -1, voittaja = NA)


keymap <- reactiveValues(data  = data.table(Nappain	= c("a", "b"),
                                            set_env = c("normal", "normal"),
                                            env = c("normal", "normal"),
                                            button_id = c("nope", "ei"),
                                            valid_pair = c("e", "b"),
                                            PainoAika = now(),
                                            Painaja = c("Lauri", "Martti"),
                                            uft_nappi = c("a", "b"),
                                            type = "",
                                            sub_id = ""))
last_simulated_click <- reactiveValues(time = now())
###############

user_logged <- reactiveValues(count = 0)


shinyServer(function(input, output, session) {
  paivittyva_statsi <- my_reactivePoll(session, "UID_UUSI_PELI", "SELECT * FROM UID_UUSI_PELI", 2500, con)

  STG_PELISTATSIT <- reactiveValues(data = data.table(dbSelectAll("UID_UUSI_PELI", con)))
  observe({
    STG_PELISTATSIT$data <- paivittyva_statsi()

  })

  required_data("STAT_VOITTOENNUSTE", force_update = TRUE)
  updatedTempData<- reactiveValues(a = 0)
  eR_Peli_ID <- reactive({

    #DEPENDENCY
    select_laurin_pakka$value
    select_martin_pakka$value
    updatedTempData$a

    input$luo_peleja
    #############

    if (!is.null(select_laurin_pakka$value) & !is.null(select_martin_pakka$value)) {

      #select_laurin_pakka <- NULL
      #select_martin_pakka <- NULL
      # select_laurin_pakka$value <- 1
      # select_martin_pakka$value <-9
      required_functions("getUusi_Peli_ID")
      #required_data(c("STG_PELISTATSIT"))

      normiToiminto <- getUusi_Peli_ID(STG_PELISTATSIT$data,
                                       select_laurin_pakka$value,
                                       select_martin_pakka$value)
      wc(data.table(Peli_ID = normiToiminto), "../common_data/", "next_game_ID" )

      # message("palautettu uusi peli id ", normiToiminto)
      return(normiToiminto)
    } else {
      NApalautus <- NA
      return(NApalautus)
    }

  })

  #load_scripts.R


  inputLoop <- reactiveValues(timeStamp = now(),
                              allow_change = TRUE,
                              which_input_changed = "",
                              input_data = NULL)
  observe({
    req(               slider_martin_mulligan$value,
                         slider_laurin_virhe$value,
                        slider_martin_virhe$value,
                       slider_laurin_landit$value,
                   slider_martin_landit$value,
                      slider_laurin_lifet$value,
                       slider_martin_lifet$value,
                        slider_vuoroarvio$value,
                       slider_laurin_kasikortit$value,
                         slider_martin_kasikorit$value)
    # vanhat_arvot <- data.table(versio = "vanha",
    #                            slider_laurin_mulligan = 1,
    #                            slider_martin_mulligan = 2,
    #                            slider_laurin_virhe =3,
    #                            slider_martin_virhe =4,
    #                            slider_laurin_landit = 5,
    #                            slider_martin_landit = 6,
    #                            slider_laurin_lifet = 7,
    #                            slider_martin_lifet = 8,
    #                            slider_vuoroarvio =9,
    #                            slider_laurin_kasikortit = 10,
    #                            slider_martin_kasikorit =  11)

    uudet <- data.table(versio = "uusi",
                        slider_laurin_mulligan = slider_laurin_mulligan$value,
                                                     slider_martin_mulligan = slider_martin_mulligan$value,
                                                     slider_laurin_virhe = slider_laurin_virhe$value,
                                                     slider_martin_virhe = slider_martin_virhe$value,
                                                     slider_laurin_landit = slider_laurin_landit$value,
                                                     slider_martin_landit = slider_martin_landit$value,
                                                     slider_laurin_lifet = slider_laurin_lifet$value,
                                                     slider_martin_lifet = slider_martin_lifet$value,
                                                     slider_vuoroarvio = slider_vuoroarvio$value,
                                                     slider_laurin_kasikortit =  slider_laurin_kasikortit$value,
                                                     slider_martin_kasikorit =  slider_martin_kasikorit$value)

    if(is.null(isolate(inputLoop$input_data))) {
      inputLoop$input_data <- uudet[1 != 0]
      inputLoop$input_data [, versio := "vanha"]
    }

    yhdiste <- rbind(uudet, isolate(inputLoop$input_data))
    melttaus1 <- suppressWarnings(melt.data.table(yhdiste, id.vars =  c("versio")))
    melttaus1[, diff := var(value), by = variable]
    muuttunut_input <- melttaus1[diff != 0][1, as.character(variable)]
    #jos ei mikaan ei muuttunu, niin ei muuteta UI:ta

    erotus <- difftime(now(), isolate(inputLoop$timeStamp))


    isolate(if(is.na(muuttunut_input)){
    #  print("denied, ei muutoksia")
      inputLoop$allow_change <- FALSE
    } else {
      isolate (if ( erotus > 1  | muuttunut_input != inputLoop$which_input_changed) {
   #     print("allow")
        inputLoop$allow_change <- TRUE
        inputLoop$timeStamp <- now()
        uudet[, versio := "vanha"]
        inputLoop$input_data <- uudet
        inputLoop$which_input_changed <- muuttunut_input


      } else {
  #     print("denied")
        inputLoop$allow_change <- FALSE
      })

    })

  }, priority = 1000001)







func_login <- function(input_user_count, clientDataInput) {
  cdata <- clientDataInput
    login <- cdata[["url_search"]]

    nimi <- word(login, 2, sep = "=")
    print("nimi")
    print(login)
    print(nimi)
    if (login == "") {
        if (input_user_count == 1) {
        result <- "Lauri"
      } else {
        result <- "Martti"
      }
    } else {
    result <- nimi
  }
  return(result)
}
#user_logged$count <- user_logged$count + 1
isolate(user_logged$count <- user_logged$count + 1)
session$user <- isolate(func_login(user_logged$count, session$clientData))

if(session$user == "overlay") {
js$hidehead('none')
shinyjs::addClass(selector = "body", class = "sidebar-collapse")
updateTabItems(session,"sidebarmenu", "tab_overlay")
}


  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

  input_kansio_list <- c(
                         "tabstatic",
                         "tab",
                         "root"
                         )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourcing at server", filename))
        source(paste0("./scripts/", filename), local = TRUE)
        print(paste0("sourced at server", filename))
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }



  # observe({
  #
  #   follow_uid_uusi_peli
  #   required_data("STG_PELISTATSIT", force_update = TRUE)
  #   #tun once for each players.
  # print("updated pelistatsit from db")
  # print(STG_PELISTATSIT[is.na(Voittaja)])
  #     updatedTempData$a <- isolate(updatedTempData$a + 1)
  # })


  click_groupButton <- function(session, group_id, button_name) {
    #curr_value <- c("kol", "ys", "kas")
   # button_name <- "ys"

    curr_value <- eval(paste0("input$", group_id))
    curr_value <- eval(input$dmg_settings)
    #check if button is selected
    selected <- button_name %in% curr_value
    if (selected == TRUE) {
      new_value <- curr_value[!curr_value %in% button_name]
    } else {
      new_value <- c(curr_value, button_name)
    }
    updateRadioGroupButtons(session, group_id, selected = new_value)
  }

 output$debug_keymap <- renderDataTable({keymap$data})
 output$debug_local_env <- renderText({local_keymap$env})

  #required_data("STAT_DMG_TURN_ALL")
#  required_data("ADM_TURN_DATA_ALL")
  # sourcelist <- dir("./scripts/")
  # tab_sources <- sourcelist[grepl("tab", sourcelist)]
  #
  #
  # for(filename in tab_sources) {
  #   source(paste0("./scripts/", filename), local = TRUE)
  # }



   #obserEventit
  refresh_counter <- reactiveValues(a = 0)
  observeEvent(input$refresh,{
    refresh_counter$a <- refresh_counter$a + 1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)








observeEvent(input$loginbutton, {
  shinyalert(
    callbackR = function(x) {
      session$user <- x
    },

    title = "Login", text = "Type owner name", type = "input", closeOnEsc = TRUE,
    closeOnClickOutside = FALSE, html = TRUE, showCancelButton = TRUE,
    showConfirmButton = TRUE, inputType = "text",
     confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4", cancelButtonText = "Cancel", timer = 0,
    animation = TRUE, imageUrl = NULL, imageWidth = 100,
    imageHeight = 100, className = "",
    callbackJS = NULL)

})
output$Username <- renderText({

  req(session)
  invalidateLater(10000, session)
  session$user
})

#tätä voi käyttää, jos haluaa tallentaa inputtien arvot.
# observeEvent(input$arvo_peli,{
# input_values <<- lapply(reactiveValuesToList(input), unclass)
# saveR_and_send(input_values, "input", "input_values.R")
# })
#load("./external_files/input_values.R")
})
