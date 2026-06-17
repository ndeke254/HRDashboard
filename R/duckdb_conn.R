#' Create a DuckDB connection
#'
#' @param duckdb_path String. Path to the DuckDB database file.
#'   Default is read from the environment variable "DUCKDB_PATH".
#' @return DBI::dbConnect()
#' @export
duckdb_conn <- function(
  duckdb_path = Sys.getenv(
    x     = "DUCKDB_PATH",
    unset = file.path(
      system.file(package = "HRDashboard"),
      "data", "base", "attendance.duckdb"
    )
  )
) {
  if (!file.exists(duckdb_path)) {
    stop("'duckdb_path' does not exist!", call. = FALSE)
  }

  DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
}
