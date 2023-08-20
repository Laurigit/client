#ui_custome_tournament
tabItem(tabName = "tab_custom_tournament",
        fluidPage(
          fluidRow(
            column(3, actionButton("aloita_custom", label = "Start game")),
            column(3, radioButtons("aloittaja_custom", "Aloittaja", choiceNames = c("vasen", "oikea", "eivalittu"), choiceValues = c(0, 1, -1), selected = -1)),
            column(3, textOutput("matchup_text")),
            column(3, radioButtons("activate_custom", "Activate custom tournament", choiceNames = c("inactive", "active"), choiceValues = c(0, 1), selected = 0))
          ),
          fluidRow(column(4, actionButton("tallenna_tulos_voittaja", "Tallenna tulos")),
                    column(4,      radioButtons("voittaja_custom", "Voittaja", selected = NA, choiceNames  = c("vasen", "oikea", "tasuri", "ei_pelattu"), choiceValues  = c(-1, 1, 0, NA)))),
          fluidRow(
                   column(3, uiOutput("matchup_tilanne")),
                   column(3, uiOutput("vasen_pelaaja_custom")),
                   column(3, uiOutput("oikea_pelaaja_custom")),
                   column(3, uiOutput("matchuptimer"))
                   ),
          fluidRow( column(3, dataTableOutput("sarjataulukko")))
          )
        )

