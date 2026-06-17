#' Read employees schedule
#'
#' @return A data.table
#' @export
read_employees_schedule <- function() {
  query <- "
  SELECT 
    s.employee_id,
    e.name,
    d.id as dept_id,
    d.name as dept_name,
    s.monday,
    s.tuesday,
    s.wednesday,
    s.thursday,
    s.friday
  FROM schedules s
  LEFT JOIN employees e ON e.id = s.employee_id
  LEFT JOIN departments d ON d.id = e.dept_id
  ORDER BY e.name
"

  schedule <- DBI::dbGetQuery(conn = duckdb_conn(), statement = query)
  data.table::setDT(schedule)[]
}
