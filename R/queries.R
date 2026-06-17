#' Attendance data warehouse path
#' @keywords internal
.p <- function(
  parquet_root = Sys.getenv(
    x     = "PARQUET_ROOT",
    unset = system.file("data/warehouse", package = "HRDashboard")
  )
) {
  glue::glue("read_parquet('{parquet_root}/attendance/**/*.parquet', hive_partitioning=true)")
}

#' Payroll data warehouse path
#' @keywords internal
.pp <- function(
  parquet_root = Sys.getenv(
    x     = "PARQUET_ROOT",
    unset = system.file("data/warehouse", package = "HRDashboard")
  )
) {
  glue::glue("read_parquet('{parquet_root}/payroll/**/*.parquet', hive_partitioning=true)")
}

#' Execution wrapper
#' @keywords internal
.q <- function(sql) {
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn = conn, statement = sql))[]
}

#' Build SQL WHERE clause (date range + optional dept/emp/loc filters)
#' @keywords internal
.where <- function(date_from = NULL, date_to = NULL, dept_ids = NULL, emp_ids = NULL, loc_ids = NULL) {
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

  if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L) {
    clauses <- c(clauses,
      glue::glue("location_id IN ({paste(as.integer(loc_ids), collapse = ',')})"))
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
#' @param dept_ids,emp_ids,loc_ids Optional filter vectors.
#' @return data.table
#' @export
query_attendance_summary <- function(
  by       = "department",
  date_from = NULL,
  date_to   = NULL,
  dept_ids  = NULL,
  emp_ids   = NULL,
  loc_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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
  emp_ids   = NULL,
  loc_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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
  emp_ids   = NULL,
  loc_ids   = NULL
) {
  gc <- .gc(by)
  w  <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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
  loc_ids   = NULL,
  by        = NULL,
  grain     = "week"
) {
  w   <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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
  dept_ids  = NULL, emp_ids = NULL, loc_ids = NULL,
  by        = NULL,
  grain     = "week"
) {
  query_metric_trend("presence_rate_pct", date_from, date_to, dept_ids, emp_ids, loc_ids, by, grain)
}

#' Payroll trend (gross pay + overtime per period)
#' @inheritParams query_metric_trend
#' @export
query_payroll_trend <- function(
  date_from = NULL, date_to = NULL,
  dept_ids  = NULL, emp_ids = NULL, loc_ids = NULL,
  by        = NULL,
  grain     = "week"
) {
  w   <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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
  emp_ids   = NULL,
  loc_ids   = NULL
) {
  w <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)

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
  dept_ids  = NULL,
  loc_ids   = NULL
) {
  w  <- .where(date_from, date_to, dept_ids, emp_ids, loc_ids)
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

#' Workforce headcount overview (DuckDB employees + offices)
#' @param dept_ids Optional department filter.
#' @param loc_ids Optional location filter.
#' @export
query_workforce_overview <- function(dept_ids = NULL, loc_ids = NULL) {
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND e.dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND e.location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  today <- Sys.Date()
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    SELECT
      COUNT(*)                                                            AS total_employees,
      SUM(CAST(e.status = 'active' AS INTEGER))                         AS active_employees,
      ROUND(SUM(e.fte * CAST(e.status = 'active' AS DOUBLE)), 2)        AS total_fte,
      SUM(CAST(e.gender = 'F' AND e.status = 'active' AS INTEGER))      AS female_count,
      SUM(CAST(e.gender = 'M' AND e.status = 'active' AS INTEGER))      AS male_count,
      ROUND(AVG(CASE WHEN e.status = 'active'
            THEN DATE_DIFF('year', e.birth_date, DATE '{today}') END), 1) AS avg_age,
      ROUND(AVG(CASE WHEN e.status = 'active'
            THEN DATE_DIFF('year', e.hire_date,  DATE '{today}') END), 2) AS avg_tenure_yrs
    FROM employees e
    WHERE 1=1 {dept_filter} {loc_filter}
  ")))[]
}

#' Gender diversity breakdown
#' @param by One of "department", "location", "level".
#' @export
query_gender_diversity <- function(by = c("department", "location", "level"),
                                   dept_ids = NULL, loc_ids = NULL) {
  by <- match.arg(by)
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND e.dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND e.location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""

  group_col <- switch(by,
    department = "d.name",
    location   = "o.name",
    level      = "e.job_level"
  )
  join_clause <- switch(by,
    department = "LEFT JOIN departments d ON d.id = e.dept_id",
    location   = "LEFT JOIN offices o ON o.id = e.location_id",
    level      = ""
  )

  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    SELECT {group_col} AS group_label,
           e.gender,
           COUNT(*) AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY {group_col}), 1) AS pct
    FROM employees e
    {join_clause}
    WHERE e.status = 'active' {dept_filter} {loc_filter}
    GROUP BY {group_col}, e.gender
    ORDER BY {group_col}, e.gender
  ")))[]
}

#' Age distribution (10-year buckets)
#' @export
query_age_distribution <- function(dept_ids = NULL, loc_ids = NULL) {
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  today <- Sys.Date()
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    WITH ages AS (
      SELECT DATE_DIFF('year', birth_date, DATE '{today}') AS age
      FROM employees
      WHERE status = 'active' {dept_filter} {loc_filter}
    ),
    bucketed AS (
      SELECT
        CASE
          WHEN age < 25  THEN 'Under 25'
          WHEN age < 35  THEN '25 - 34'
          WHEN age < 45  THEN '35 - 44'
          WHEN age < 55  THEN '45 - 54'
          ELSE                '55+'
        END AS age_bucket,
        CASE
          WHEN age < 25  THEN 1
          WHEN age < 35  THEN 2
          WHEN age < 45  THEN 3
          WHEN age < 55  THEN 4
          ELSE                5
        END AS sort_key
      FROM ages
    )
    SELECT age_bucket, sort_key,
           COUNT(*) AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct
    FROM bucketed
    GROUP BY age_bucket, sort_key
    ORDER BY sort_key
  ")))[]
}

#' Education level distribution
#' @export
query_education_distribution <- function(dept_ids = NULL, loc_ids = NULL) {
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    WITH edu AS (
      SELECT education,
        CASE education
          WHEN 'High School' THEN 1
          WHEN 'Associate'   THEN 2
          WHEN 'Bachelor'    THEN 3
          WHEN 'Master'      THEN 4
          WHEN 'MBA'         THEN 5
          WHEN 'PhD'         THEN 6
          ELSE 7
        END AS sort_key
      FROM employees
      WHERE status = 'active' {dept_filter} {loc_filter}
    )
    SELECT education, sort_key,
           COUNT(*) AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct
    FROM edu
    GROUP BY education, sort_key
    ORDER BY sort_key
  ")))[]
}

#' Job level distribution
#' @export
query_level_distribution <- function(dept_ids = NULL, loc_ids = NULL) {
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND e.dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND e.location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    SELECT e.job_level,
           pc.level_label,
           COUNT(*) AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct,
           ROUND(SUM(e.fte), 1) AS fte_total
    FROM employees e
    LEFT JOIN payroll_config pc ON pc.job_level = e.job_level
    WHERE e.status = 'active' {dept_filter} {loc_filter}
    GROUP BY e.job_level, pc.level_label
    ORDER BY e.job_level
  ")))[]
}

#' Tenure distribution (years)
#' @export
query_tenure_distribution <- function(dept_ids = NULL, loc_ids = NULL) {
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter <- if (!is.null(loc_ids) && !identical(loc_ids, "all") && length(loc_ids) > 0L)
    glue::glue("AND location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  today <- Sys.Date()
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    WITH tenures AS (
      SELECT DATE_DIFF('year', hire_date, DATE '{today}') AS tenure_yrs
      FROM employees
      WHERE status = 'active' {dept_filter} {loc_filter}
    ),
    bucketed AS (
      SELECT
        CASE
          WHEN tenure_yrs < 1  THEN '< 1 yr'
          WHEN tenure_yrs < 3  THEN '1 - 2 yrs'
          WHEN tenure_yrs < 5  THEN '3 - 4 yrs'
          WHEN tenure_yrs < 8  THEN '5 - 7 yrs'
          WHEN tenure_yrs < 11 THEN '8 - 10 yrs'
          ELSE                      '10+ yrs'
        END AS tenure_bucket,
        CASE
          WHEN tenure_yrs < 1  THEN 1
          WHEN tenure_yrs < 3  THEN 2
          WHEN tenure_yrs < 5  THEN 3
          WHEN tenure_yrs < 8  THEN 4
          WHEN tenure_yrs < 11 THEN 5
          ELSE                      6
        END AS sort_key
      FROM tenures
    )
    SELECT tenure_bucket, sort_key,
           COUNT(*) AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct
    FROM bucketed
    GROUP BY tenure_bucket, sort_key
    ORDER BY sort_key
  ")))[]
}

#' Absenteeism rate from attendance Parquet
#' @export
query_absenteeism_rate <- function(date_from = NULL, date_to = NULL,
                                   dept_ids = NULL, loc_ids = NULL) {
  w <- .where(date_from, date_to, dept_ids, loc_ids = loc_ids)
  .q(glue::glue("
    SELECT
      dept_name,
      dept_id,
      ROUND(100.0 * SUM(CAST(NOT is_present AS INTEGER)) / NULLIF(COUNT(*), 0), 2)
        AS absenteeism_rate_pct,
      COUNT(*) AS total_scheduled,
      SUM(CAST(NOT is_present AS INTEGER)) AS total_absent
    FROM {.p()} {w}
    GROUP BY dept_id, dept_name
    ORDER BY absenteeism_rate_pct DESC
  "))
}

#' Payslip for a single employee-month from payroll Parquet
#' @export
query_payslip <- function(employee_id, year, month) {
  conn <- duckdb_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data.table::setDT(DBI::dbGetQuery(conn, glue::glue("
    SELECT p.*,
           e.name       AS employee_name_full,
           e.title,
           e.employee_no,
           e.gender,
           e.hire_date,
           e.education,
           o.name       AS office_name,
           o.city,
           o.country,
           d.name       AS department_name,
           pc.level_label
    FROM {.pp()} p
    LEFT JOIN employees e ON e.id = p.employee_id
    LEFT JOIN offices   o ON o.id = e.location_id
    LEFT JOIN departments d ON d.id = p.dept_id
    LEFT JOIN payroll_config pc ON pc.job_level = p.job_level
    WHERE p.employee_id = {as.integer(employee_id)}
      AND p.year  = {as.integer(year)}
      AND p.month = {as.integer(month)}
    LIMIT 1
  ")))[]
}

#' Monthly payroll KPI summary (across employees)
#' @export
query_payroll_kpi_summary <- function(year = NULL, month = NULL, dept_ids = NULL, loc_ids = NULL) {
  yr_filter   <- if (!is.null(year))  glue::glue("AND year = {as.integer(year)}")   else ""
  mo_filter   <- if (!is.null(month)) glue::glue("AND month = {as.integer(month)}") else ""
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter  <- if (!is.null(loc_ids)  && !identical(loc_ids,  "all") && length(loc_ids)  > 0L)
    glue::glue("AND location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  .q(glue::glue("
    SELECT
      COUNT(DISTINCT employee_id)      AS headcount,
      ROUND(SUM(gross_pay), 2)         AS total_gross,
      ROUND(SUM(net_pay), 2)           AS total_net,
      ROUND(SUM(total_allowances), 2)  AS total_allowances,
      ROUND(SUM(total_deductions), 2)  AS total_deductions,
      ROUND(AVG(net_pay), 2)           AS avg_net_pay
    FROM {.pp()}
    WHERE 1=1 {yr_filter} {mo_filter} {dept_filter} {loc_filter}
  "))
}

#' Payroll trend (monthly) from payroll Parquet
#' @export
query_payroll_cost_trend <- function(dept_ids = NULL, loc_ids = NULL,
                                     year = NULL) {
  yr_filter   <- if (!is.null(year)) glue::glue("AND year = {as.integer(year)}") else ""
  dept_filter <- if (!is.null(dept_ids) && !identical(dept_ids, "all") && length(dept_ids) > 0L)
    glue::glue("AND dept_id IN ({paste(as.integer(dept_ids), collapse = ',')})") else ""
  loc_filter  <- if (!is.null(loc_ids)  && !identical(loc_ids,  "all") && length(loc_ids)  > 0L)
    glue::glue("AND location_id IN ({paste(as.integer(loc_ids), collapse = ',')})") else ""
  .q(glue::glue("
    SELECT year, month,
           STRFTIME(MAKE_DATE(year, month, 1), '%Y-%m') AS period,
           ROUND(SUM(gross_pay), 2)        AS total_gross,
           ROUND(SUM(total_allowances), 2) AS total_allowances,
           ROUND(SUM(total_deductions), 2) AS total_deductions,
           ROUND(SUM(net_pay), 2)          AS total_net
    FROM {.pp()}
    WHERE 1=1 {yr_filter} {dept_filter} {loc_filter}
    GROUP BY year, month
    ORDER BY year, month
  "))
}
