#' Parquet glob path
#'
#' @keywords internal
.parquet <- function(
  parquet_root = Sys.getenv(
    x     = "PARQUET_ROOT",
    unset = system.file("data/warehouse", package = "HRDashboard")
  )
) {
  glue::glue(
    "read_parquet('{parquet_root}/attendance/**/*.parquet', hive_partitioning = true)"
  )
}

#' Get departments choices for selectize inputs
#'
#' @return Named list suitable for \code{selectizeInput}.
#' @export
get_departments_choices <- function() {
  departments <- DBI::dbGetQuery(
    conn      = duckdb_conn(),
    statement = "SELECT id, name FROM departments ORDER BY name"
  )

  if (nrow(departments) > 1L) {
    return(c(
      "All Departments" = "all",
      setNames(as.character(departments$id), departments$name)
    ))
  }

  setNames(as.character(departments$id), departments$name)
}

#' Get employees choices for selectize inputs
#'
#' @param dept_id Integer vector or \code{"all"}. Filter by department(s).
#' @return Named list (optgroup style) suitable for \code{selectizeInput}.
#' @export
get_employees_choices <- function(dept_id = "all") {
  if ("all" %in% dept_id || is.null(dept_id)) {
    where_clause <- ""
  } else {
    dept_ids     <- paste(as.integer(dept_id), collapse = ", ")
    where_clause <- glue::glue("WHERE e.dept_id IN ({dept_ids}) AND e.status = 'active'")
  }

  query <- glue::glue("
    SELECT
      e.id,
      e.name,
      e.dept_id,
      d.name AS dept_name
    FROM employees e
    LEFT JOIN departments d ON d.id = e.dept_id
    {where_clause}
    ORDER BY d.name, e.name
  ")
  employees <- DBI::dbGetQuery(conn = duckdb_conn(), statement = query)

  choices_list <- list()
  for (did in unique(employees$dept_id)) {
    dept_employees <- employees[employees$dept_id == did, ]
    dept_name      <- unique(dept_employees$dept_name)[1L]
    choices_list[[dept_name]] <- setNames(
      as.character(dept_employees$id),
      dept_employees$name
    )
  }

  if (length(choices_list) > 1L) {
    return(c(list(" " = c("All Employees" = "all")), choices_list))
  }

  choices_list
}

#' Get year choices for selectize inputs
#'
#' @return Named character vector suitable for \code{selectInput}.
#' @export
get_years_choices <- function() {
  tryCatch({
    yrs <- DBI::dbGetQuery(
      conn      = duckdb_conn(),
      statement = glue::glue(
        "SELECT DISTINCT year FROM {.parquet()} ORDER BY year DESC"
      )
    )
    as.character(yrs$year)
  }, error = function(e) {
    as.character(format(Sys.Date(), "%Y"))
  })
}

#' Get office/location choices for selectize inputs
#' @return Named character vector suitable for \code{selectInput}.
#' @export
get_locations_choices <- function() {
  tryCatch({
    locs <- DBI::dbGetQuery(
      conn      = duckdb_conn(),
      statement = "SELECT id, name, city, country FROM offices ORDER BY name"
    )
    c(
      "All Locations" = "all",
      setNames(as.character(locs$id),
               paste0(locs$name, " (", locs$country, ")"))
    )
  }, error = function(e) {
    c("All Locations" = "all")
  })
}
