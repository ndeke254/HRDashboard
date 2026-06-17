#' Read live attendance for today from DuckDB
#'
#' @return data.table with columns: employee_id, name, dept_name,
#'   check_type, check_time, diff_mins, trend.
#' @export
read_live_attendance <- function() {
  query <- "
    SELECT
      lv.employee_id,
      e.name,
      d.name  AS dept_name,
      lv.check_type,
      lv.check_time,
      lv.diff_mins,
      lv.trend
    FROM live_attendance lv
    LEFT JOIN employees   e ON e.id = lv.employee_id
    LEFT JOIN departments d ON d.id = e.dept_id
    ORDER BY lv.check_time DESC
  "
  attendance <- DBI::dbGetQuery(conn = duckdb_conn(), statement = query)
  data.table::setDT(attendance)[]
}
