wc <- function(fname, folder = "../common_data/", outputFileName = NULL) {

   if (is.null(outputFileName)) {
     outputFileName <- deparse(substitute(fname))
  }

  write.table(x = fname,
              file = paste0(folder, outputFileName, ".csv"),
              sep = ";",
              row.names = FALSE,

              dec = ",")
}
