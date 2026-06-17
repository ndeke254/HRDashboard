#' Read current holidays
#'
#' @return A data.table
#' @export
read_holidays <- function() {
  query <- "
    SELECT
      id,
      name,
      month,
      day,
      is_fixed
    FROM holidays
  "

  holidays <- DBI::dbGetQuery(conn = duckdb_conn(), statement = query)
  data.table::setDT(holidays)

  holidays[,
    date := sprintf("%d-%02d-%02d", lubridate::year(Sys.Date()), month, day)
  ]
  holidays[, c("month", "day") := NULL]

  holidays[]
}
