# constants:
app_title   <- "HR Analytics Dashboard"
app_version <- "1.0.0"

# configs:
shift_start_mins <- 8L * 60L
shift_end_mins   <- 17L * 60L
late_threshold   <- 60L
early_threshold  <- 30L
target_work_hrs  <- 7.5
tz               <- "UTC"

colors <- list(
  white = "#ffffff",
  black = "#0a1e2b",
  primary = "#167070",
  teal = "#167070",
  navy = "#003D59",
  chart_text = "#5a6875",
  chart_text_light = "#8d9aa3",
  chart_ramp = c("#167070", "#125f5f", "#0e4d4d", "#0a3b3b", "#062929"),
  ash = "#b3b8ba",
  ash_light = "#e3e7e9",
  bg_page = "#eef3f7",
  bg_auth = "#F7F9FB",
  row_alt_bg = "#f5f8fa",
  present = "#167070",
  present_bg = "#d4f0ea",
  late = "#FB9334",
  late_bg = "#FEEFE1",
  absent = "#c0303a",
  absent_bg = "#fae0e1",
  wfh = "#0369A1",
  wfh_bg = "#cce9fd",
  half_day = "#dcd100",
  half_day_bg = "#fef9c3",
  wfo = "#167070",
  wfo_bg = "#d4f0ea",
  irregular = "#FB9334",
  irregular_bg = "#FEEFE1",
  out_of_schedule = "#e3e7e9",
  out_of_schedule_bg = "#167070",
  scheduled = "#eef3f7",
  scheduled_bg = "#5a6875",
  leave = "#f5f8fa",
  leave_bg = "#b3b8ba",
  on_track = "#167070",
  on_track_bg = "#d4f0ea",
  watch = "#FB9334",
  watch_bg = "#FEEFE1",
  at_risk = "#c0303a",
  at_risk_bg = "#fae0e1",
  trend_up = "#16a34a",
  trend_down = "#dc2626",
  trend_neutral = "#6b7280",
  single_bar = "#167070",
  comp_col_1 = "#003D59",
  comp_col_2 = "#FE6625",
  comp_col_3 = "#167070",
  comp_col_4 = "#FB9334",
  comp_col_5 = "#414A4F"
)

font_primary <- "Maven Pro, sans-serif"

`%||%`  <- \(a, b) if (!is.null(a) && length(a) > 0) a else b
`%!in%` <- Negate(`%in%`)

# metrics:
metrics_list <- list(
  kpi_attendance = list(
    presence_rate_pct = list(id = "presence_rate_pct", label = "Attendance Rate", suffix = "%")
  ),
  kpi_hours = list(
    avg_hours_worked = list(id = "avg_hours_worked", label = "Avg Hours/Day", suffix = " h")
  ),
  kpi_ontime = list(
    on_time_rate_pct = list(id = "on_time_rate_pct", label = "On-Time Rate", suffix = "%")
  ),
  kpi_pay = list(
    total_gross_pay = list(id = "total_gross_pay", label = "Gross Pay", suffix = "")
  ),
  kpi_overtime = list(
    total_overtime_pay = list(id = "total_overtime_pay", label = "Overtime Cost", suffix = "")
  ),
  kpi_score = list(
    kpi_score = list(id = "kpi_score", label = "KPI Score", suffix = "%")
  ),
  comparisons = list(
    total_gross_pay    = list(id = "total_gross_pay",    label = "Gross Pay",       suffix = ""),
    presence_rate_pct  = list(id = "presence_rate_pct",  label = "Attendance Rate", suffix = "%"),
    avg_hours_worked   = list(id = "avg_hours_worked",   label = "Avg Hours/Day",   suffix = " h"),
    on_time_rate_pct   = list(id = "on_time_rate_pct",   label = "On-Time Rate",    suffix = "%"),
    total_overtime_pay = list(id = "total_overtime_pay", label = "Overtime Cost",   suffix = ""),
    kpi_score          = list(id = "kpi_score",          label = "KPI Score",       suffix = "%")
  ),
  trends = list(
    presence_rate_pct  = list(id = "presence_rate_pct",  label = "Attendance Rate", suffix = "%"),
    avg_hours_worked   = list(id = "avg_hours_worked",   label = "Avg Hours/Day",   suffix = " h"),
    total_gross_pay    = list(id = "total_gross_pay",    label = "Gross Pay",       suffix = ""),
    total_overtime_pay = list(id = "total_overtime_pay", label = "Overtime Cost",   suffix = ""),
    on_time_rate_pct   = list(id = "on_time_rate_pct",   label = "On-Time Rate",    suffix = "%"),
    kpi_score          = list(id = "kpi_score",          label = "KPI Score",       suffix = "%")
  ),
  distributions = list(
    hours_distribution = list(id = "hours_distribution", label = "Hours Worked Distribution")
  ),
  attendance_proportions = list(
    status_proportions = list(id = "attendance_status_proportion", label = "Attendance Proportions")
  ),
  employees_schedule = list(
    employees_schedule = list(id = "schedule_this_week", label = "Weekly Schedule"),
    live_attendance    = list(id = "live_attendance",    label = "Live Attendance")
  )
)

tryCatch(
  sass::sass(
    input  = sass::sass_file("styles/main.scss"),
    output = "www/main.css",
    cache  = NULL
  ),
  error = function(e) message("CSS compile skipped: ", conditionMessage(e))
)

tryCatch(
  initialize_app(),
  error = function(e) message("[HR Dashboard] initialization failed: ", conditionMessage(e))
)

library(HRDashboard)
