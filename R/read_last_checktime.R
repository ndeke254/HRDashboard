#' Check last checktime (refresh time)
#'
#' @export
read_last_checktime <- \(
  checktime_file = Sys.getenv(
    x = "CHECKTIME_FILE",
    unset = system.file(
      "data/state/last_checktime.txt",
      package = "HRDashboard"
    )
  )
) {
  time <- readLines(checktime_file, warn = FALSE)
  if (identical(length(time), 0L)) {
    write_last_checktime(checktime_file = checktime_file)
  }

  as.POSIXct(
    x = readLines(checktime_file, warn = FALSE),
    format = "%Y-%m-%d %H:%M:%S",
    tz = tz
  )
}
