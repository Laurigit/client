

custom_tournament_tee_peleja <- function(player_list, kierroksia, con) {
  player_list <- c("Xira", "Eron", "Jace")
  kierroksia <- 50
  tulostaulu <- data.table(vasen = as.character(), oikea = as.character(), game_id = as.numeric(), match_id = as.numeric(), voittaja = as.numeric())
  tays_kierros <- data.table(expand.grid(player_list, player_list))[Var1 != Var2]




  tays_kierros[, row_id := seq_len(.N)]


  tays_kierros[, stringiin := paste0(Var1, ";", Var2), by = row_id]
  tays_kierros[,  c("name1") := paste0(sort(unlist(strsplit(stringiin, ";"))), collapse = "_"), by = row_id]
  tays_kierros[, vasen := word(name1, 1, 1, sep ="_")]
  tays_kierros[, oikea := word(name1, 2, 2, sep ="_")]
  sscols <-tays_kierros[, .(vasen, oikea, matchup = name1)][ , ID := .GRP, by = matchup]
  sscols[, monesko_kierros := seq_len(.N), by = ID]

  sscols[, bo3_rivi := seq_len(.N)]
  #sscols[, match_id := seq_len(.N)]
  #puolet <- sscols[1:(nrow(sscols)/2)]

  bo3data <- rbind(sscols, sscols, sscols)

  sortattu_pelijarestys <- bo3data[order( monesko_kierros, ID), .(vasen, oikea, game_id = 0, match_id = 0, voittaja = NA)]
  puolet_pois <- sortattu_pelijarestys[1:(nrow(sortattu_pelijarestys)/2)]
  puolet_pois[, nth_game_of_match := seq_len(.N), by = .(vasen, oikea)]

  tot_data <- NULL
  for (kierrosloop in 1:kierroksia) {
    #print(kierrosloop)
    tot_data <- rbind(tot_data, puolet_pois)
  }

  tot_data[, game_id := seq_len(.N)]
  tot_data[, match_id := ceiling(game_id / 3)]
  tot_data[, timestamp := ""]
  dbWriteTable(con, "CUSTOM_TOURNAMENT", tot_data, overwrite = TRUE, row.names = FALSE)
}
