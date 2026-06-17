#' Data warehouse path
#' @keywords internal
.p <- function(
  parquet_root = Sys.getenv(
    x     = "PARQUET_ROOT",
    unset = system.file("data/warehouse", package = "HRDashboard")
  )
) {
  glue::glue("read_parquet('{parquet_root}/**/*.parquet', hive_partitioning=true)")
}

#' Execution wrapper
#' @keywords internal
.q <- function(sql) {
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn = conn, statement = sql))[]
}

#' Build SQL WHERE clause (date range + optional dept/emp filters)
#' @keywords internal
.where <- function(date_from = NULL, date_to = NULL, dept_ids = NULL, emp_ids = NULL) {
  clauses <- character(0)

  if (!is.null(date_from) && !is.null(date_to)) {
    clauses <- c(
      clauses,
      glue::glue(
        "CAST(date AS DATE) BETWEEN DATE '{format(as.Date(date_from), '%Y-%m-%d')}' AND DATE '{format(as.Date(date_to), '%Y-%m-%d')}'"
      )
    )
  }

  if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L) {
    clauses <- c(
      clauses,
      glue::glue("dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})")
    )
  }

  if (!is.null(emp_ids) && !identical(emp_ids, "all") && length(emp_ids) > 0L) {
    clauses <- c(
      clauses,
      glue::glue("employee_id IN ({paste(as.integer(emp_ids), collapse = ',')})")
    )
  }

  if (identical(length(clauses), 0L)) return("")
  paste("WHERE", paste(clauses, collapse = "\n  AND "))
}

#' @keywords internal
.gc <- function(by) {
  switch(
    by,
    department = "dept_id, dept_name",
    employee   = "employee_id, employee_name, dept_id, dept_name",
    stop("'by' must be 'department' or 'employee'")
  )
}

#' @keywords internal
.grain_expr <- function(grain) {
  switch(
    grain,
    day   = "CAST(date AS VARCHAR)",
    week  = "STRFTIME(date, '%Y-W%W')",
    month = "STRFTIME(date, '%Y-%m')",
    stop("grain must be day/week/month")
  )
}

#' Attendance summary per group
#'
#' @param by String. \code{"department"} or \code{"employee"}.
#' @param date_from,date_to Date range.
#' @param dept_ids,emp_ids Optional filter vectors.
#' @return data.table
#' @export
query_attendance_summary <- function(
  by       = "department",
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids)
  .q(glue::glue("
    SELECT {gc},
      ROUND(100.0 * AVG(CAST(is_present AS DOUBLE)), 2)                       AS presence_rate_pct,
      ROUND(AVG(CASE WHEN is_present THEN hours_worked END), 2)                AS avg_hours_worked,
      ROUND(100.0 * AVG(CASE WHEN is_present AND is_late IS NOT NULL
                              THEN CAST(NOT is_late AS DOUBLE) END), 2)        AS on_time_rate_pct,
      ROUND(100.0 * AVG(CASE WHEN is_present AND is_early_leave IS NOT NULL
                              THEN CAST(is_early_leave AS DOUBLE) END), 2)     AS early_leave_pct,
      COUNT(*) AS n_scheduled
    FROM {.p()} {w}
    GROUP BY {gc}
    ORDER BY {gc}
  "))
}

#' Payroll summary per group
#'
#' @inheritParams query_attendance_summary
#' @export
query_payroll_summary <- function(
  by       = "department",
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids)
  .q(glue::glue("
    SELECT {gc},
      ROUND(SUM(gross_daily_pay), 2)                                         AS total_gross_pay,
      ROUND(SUM(regular_pay), 2)                                             AS total_regular_pay,
      ROUND(SUM(overtime_pay), 2)                                            AS total_overtime_pay,
      ROUND(AVG(CASE WHEN gross_daily_pay > 0 THEN gross_daily_pay END), 2)  AS avg_daily_pay,
      ROUND(100.0 * SUM(overtime_pay) / NULLIF(SUM(gross_daily_pay), 0), 2)  AS overtime_pct,
      COUNT(*) AS n_days
    FROM {.p()} {w}
    GROUP BY {gc}
    ORDER BY {gc}
  "))
}

#' Composite KPI summary per group
#'
#' @inheritParams query_attendance_summary
#' @export
query_kpi_summary <- function(
  by       = "department",
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids)
  .q(glue::glue("
    WITH base AS (SELECT * FROM {.p()} {w}),
    comp AS (
      SELECT {gc},
        100.0 * AVG(CAST(is_on_schedule AND is_present AS DOUBLE))       AS schedule_adherence_pct,
        100.0 * AVG(CASE WHEN is_present AND is_late IS NOT NULL
                         THEN CAST(NOT is_late AS DOUBLE) END)           AS arrival_adherence_pct,
        100.0 * AVG(CASE WHEN is_present AND hours_worked IS NOT NULL
                         THEN CAST(hours_worked >= 7.5 AS DOUBLE) END)  AS hours_adherence_pct,
        COUNT(*) AS n_obs
      FROM base GROUP BY {gc}
    )
    SELECT *,
      ROUND((schedule_adherence_pct + arrival_adherence_pct + hours_adherence_pct) / 3.0, 1) AS kpi_score
    FROM comp ORDER BY {gc}
  "))
}

#' Headcount summary
#'
#' @param dept_ids Optional department filter.
#' @export
query_headcount <- function(dept_ids = NULL) {
  conn  <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  today <- Sys.Date()
  d30   <- today - 30L

  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L) {
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})")
  } else {
    ""
  }

  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    SELECT
      COUNT(*)                                                           AS total_employees,
      SUM(CAST(status = 'active'   AS INTEGER))                        AS active_employees,
      SUM(CAST(status = 'inactive' AS INTEGER))                        AS inactive_employees,
      SUM(CAST(hire_date >= DATE '{d30}' AS INTEGER))                  AS new_hires_30d,
      SUM(CAST(exit_date >= DATE '{d30}' AND
               exit_date <= DATE '{today}' AS INTEGER))                AS attrition_30d
    FROM employees WHERE 1=1 {dept_filter}
  ")))[]
}

#' Time-series trend for a single metric
#'
#' @param metric One of \code{presence_rate_pct}, \code{avg_hours_worked},
#'   \code{on_time_rate_pct}, \code{total_gross_pay}, \code{total_overtime_pay},
#'   \code{kpi_score}.
#' @param grain One of \code{"day"}, \code{"week"}, \code{"month"}.
#' @param by Optional grouping: \code{"department"} or \code{"employee"}.
#' @inheritParams query_attendance_summary
#' @export
query_metric_trend <- function(
  metric    = "presence_rate_pct",
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL,
  by        = NULL,
  grain     = "week"
) {
  w   <- .where(date_from, date_to, dept_ids, emp_ids)
  pe  <- .grain_expr(grain)
  gc  <- if (!is.null(by)) paste0(.gc(by), ",") else ""
  ogc <- if (!is.null(by)) paste0(.gc(by), ",") else ""

  metric_expr <- switch(
    metric,
    presence_rate_pct  = "ROUND(100.0 * AVG(CAST(is_present AS DOUBLE)), 2)",
    avg_hours_worked   = "ROUND(AVG(CASE WHEN is_present THEN hours_worked END), 2)",
    on_time_rate_pct   = "ROUND(100.0 * AVG(CASE WHEN is_present AND is_late IS NOT NULL THEN CAST(NOT is_late AS DOUBLE) END), 2)",
    total_gross_pay    = "ROUND(SUM(gross_daily_pay), 2)",
    total_overtime_pay = "ROUND(SUM(overtime_pay), 2)",
    kpi_score          = "ROUND(AVG(daily_kpi_score), 1)",
    stop("Unknown metric: ", metric)
  )

  .q(glue::glue("
    SELECT {gc} {pe} AS period,
           MIN(date) AS period_start,
           {metric_expr} AS value,
           COUNT(*) AS n_obs
    FROM {.p()} {w}
    GROUP BY {gc} {pe}
    ORDER BY {ogc} period_start
  "))
}

#' Attendance rate trend (convenience wrapper)
#' @inheritParams query_metric_trend
#' @export
query_attendance_trend <- function(
  date_from = NULL, date_to = NULL,
  dept_ids  = NULL, emp_ids = NULL,
  by        = NULL,
  grain     = "week"
) {
  query_metric_trend("presence_rate_pct", date_from, date_to, dept_ids, emp_ids, by, grain)
}

#' Payroll trend (gross pay + overtime per period)
#' @inheritParams query_metric_trend
#' @export
query_payroll_trend <- function(
  date_from = NULL, date_to = NULL,
  dept_ids  = NULL, emp_ids = NULL,
  by        = NULL,
  grain     = "week"
) {
  w   <- .where(date_from, date_to, dept_ids, emp_ids)
  pe  <- .grain_expr(grain)
  gc  <- if (!is.null(by)) paste0(.gc(by), ",") else ""
  ogc <- if (!is.null(by)) paste0(.gc(by), ",") else ""

  .q(glue::glue("
    SELECT {gc} {pe} AS period,
           MIN(date)                          AS period_start,
           ROUND(SUM(gross_daily_pay), 2)    AS total_gross_pay,
           ROUND(SUM(overtime_pay), 2)       AS total_overtime_pay,
           COUNT(*) AS n_obs
    FROM {.p()} {w}
    GROUP BY {gc} {pe}
    ORDER BY {ogc} period_start
  "))
}

#' Hours-worked distribution in 0.5-hour buckets
#'
#' @inheritParams query_attendance_summary
#' @export
query_hours_distribution <- function(
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL
) {
  w <- .where(date_from, date_to, dept_ids, emp_ids)

  w_present <- if (nchar(trimws(w)) > 0L) {
    paste(w, "AND is_present AND hours_worked IS NOT NULL")
  } else {
    "WHERE is_present AND hours_worked IS NOT NULL"
  }

  .q(glue::glue("
    WITH bucketed AS (
      SELECT FLOOR(hours_worked * 2) / 2.0 AS bucket
      FROM {.p()} {w_present}
    )
    SELECT bucket,
           CAST(bucket AS VARCHAR) || 'h\u2013' || CAST(bucket + 0.5 AS VARCHAR) || 'h' AS bucket_label,
           COUNT(*) AS punch_count,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS pct_of_total
    FROM bucketed
    GROUP BY bucket
    ORDER BY bucket
  "))
}

#' Attendance status per employee for a date range
#'
#' @description Defaults to the current Monday-anchored week when no filters supplied.
#'
#' @param by Grouping column set.
#' @inheritParams query_attendance_summary
#' @export
query_attendance_status <- function(
  by        = "employee",
  date_from = NULL,
  date_to   = NULL,
  emp_ids   = NULL,
  dept_ids  = NULL
) {
  w  <- .where(date_from, date_to, dept_ids, emp_ids)
  gc <- if (!is.null(by)) .gc(by) else "employee_id"

  today  <- Sys.Date()
  monday <- today - (as.integer(format(today, "%u")) - 1L)

  full_where <- if (identical(nchar(trimws(w)), 0L)) {
    glue::glue("WHERE CAST(date AS DATE) BETWEEN DATE '{monday}' AND DATE '{today}'")
  } else {
    w
  }

  .q(glue::glue("
    SELECT {gc}, date, is_present, is_on_schedule, scheduled_type,
           is_late, is_early_leave, hours_worked
    FROM {.p()} {full_where}
    ORDER BY {gc}, date
  "))
}
