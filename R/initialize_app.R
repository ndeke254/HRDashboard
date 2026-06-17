#' @import data.table
NULL

#' @keywords internal
.generate_synthetic_data <- function(duckdb_path, parquet_root) {
  set.seed(42L)

  DATE_FROM <- as.Date("2024-01-01")
  DATE_TO <- as.Date("2025-12-31")
  SHIFT_START <- 480L # 08:00
  LATE_THRESH <- 0L # cutoff at 480 = 08:00
  SHIFT_END <- 1020L # 17:00
  EARLY_THRESH <- 0L # cutoff at 1020 = 17:00
  TARGET_HRS <- 8L
  LUNCH_HRS <- 1L

  # clear existing data:
  if (fs::file_exists(duckdb_path)) {
    fs::file_delete(duckdb_path)
  }
  if (fs::dir_exists(parquet_root)) {
    fs::dir_delete(parquet_root)
  }
  fs::dir_create(dirname(duckdb_path), recurse = TRUE)
  fs::dir_create(file.path(parquet_root, "attendance"), recurse = TRUE)
  fs::dir_create(file.path(parquet_root, "payroll"),    recurse = TRUE)

  # offices (8 global branches):
  offices <- data.table::data.table(
    id      = 1:8,
    name    = c("New York HQ","London Office","Singapore Hub","Toronto Office",
                "Berlin Office","Sydney Office","Nairobi Office","Dubai Office"),
    city    = c("New York","London","Singapore","Toronto","Berlin","Sydney","Nairobi","Dubai"),
    country = c("USA","UK","Singapore","Canada","Germany","Australia","Kenya","UAE"),
    region  = c("Americas","EMEA","APAC","Americas","EMEA","APAC","Africa","EMEA")
  )

  # payroll config per job level:
  payroll_config <- data.table::data.table(
    job_level       = c("L1","L2","L3","L4","L5","L6","L7","L8"),
    level_label     = c("Junior","Mid-Level","Senior","Lead","Manager","Director","VP","C-Suite"),
    housing_pct     = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.18, 0.20, 0.25),
    transport_fixed = c(200,  250,  300,  350,  400,  500,  600,  800),
    meal_fixed      = c(100,  120,  140,  160,  180,  210,  240,  280),
    medical_fixed   = c( 80,  100,  130,  160,  200,  260,  320,  400),
    tax_rate        = c(0.15, 0.18, 0.21, 0.24, 0.26, 0.28, 0.31, 0.33),
    pension_pct     = c(0.04, 0.05, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10),
    health_premium  = c(120,  145,  175,  205,  240,  290,  350,  420)
  )

  # departments (8):
  departments <- data.table::data.table(
    id = 1:8,
    name = c(
      "Engineering",
      "Product",
      "Sales",
      "Marketing",
      "Finance",
      "Human Resources",
      "Operations",
      "Customer Support"
    )
  )
  dept_n <- c(55L, 18L, 32L, 18L, 14L, 11L, 22L, 30L)

  # name pools:
  first_m <- c(
    "James",
    "Oliver",
    "William",
    "Henry",
    "Lucas",
    "Mason",
    "Ethan",
    "Noah",
    "Liam",
    "Jack",
    "Alexander",
    "Daniel",
    "Michael",
    "Ryan",
    "David",
    "Matthew",
    "Nathan",
    "Tyler",
    "Dylan",
    "Caleb",
    "Logan",
    "Samuel",
    "Brandon",
    "Eric",
    "Aaron",
    "Adam",
    "Sean",
    "Marcus",
    "Julian",
    "Patrick"
  )
  first_f <- c(
    "Emma",
    "Olivia",
    "Sophia",
    "Isabella",
    "Ava",
    "Mia",
    "Amelia",
    "Harper",
    "Ella",
    "Lily",
    "Grace",
    "Chloe",
    "Hannah",
    "Layla",
    "Zoe",
    "Nora",
    "Emily",
    "Charlotte",
    "Aria",
    "Luna",
    "Stella",
    "Victoria",
    "Maya",
    "Penelope",
    "Riley",
    "Audrey",
    "Claire",
    "Sadie",
    "Leah",
    "Diana"
  )
  last_names <- c(
    "Smith",
    "Johnson",
    "Williams",
    "Brown",
    "Jones",
    "Garcia",
    "Miller",
    "Davis",
    "Martinez",
    "Wilson",
    "Anderson",
    "Taylor",
    "Thomas",
    "Jackson",
    "White",
    "Harris",
    "Martin",
    "Thompson",
    "Robinson",
    "Clark",
    "Rodriguez",
    "Lewis",
    "Lee",
    "Walker",
    "Hall",
    "Allen",
    "Young",
    "King",
    "Wright",
    "Scott",
    "Torres",
    "Nguyen",
    "Hill",
    "Adams",
    "Baker",
    "Nelson",
    "Carter",
    "Mitchell",
    "Perez",
    "Roberts",
    "Turner",
    "Phillips",
    "Campbell",
    "Parker",
    "Evans",
    "Edwards",
    "Collins",
    "Stewart",
    "Morris",
    "Rogers",
    "Reed"
  )

  titles_by_dept <- list(
    "Engineering" = c(
      "Software Engineer",
      "Senior Engineer",
      "Lead Engineer",
      "Engineering Manager",
      "DevOps Engineer",
      "QA Engineer",
      "Staff Engineer"
    ),
    "Product" = c(
      "Product Manager",
      "Senior PM",
      "Product Analyst",
      "UX Designer",
      "Product Director",
      "Associate PM"
    ),
    "Sales" = c(
      "Account Executive",
      "Sales Representative",
      "Sales Manager",
      "Business Development Rep",
      "Sales Director",
      "SDR"
    ),
    "Marketing" = c(
      "Marketing Specialist",
      "Content Strategist",
      "SEO Analyst",
      "Marketing Manager",
      "Brand Designer",
      "Growth Analyst"
    ),
    "Finance" = c(
      "Financial Analyst",
      "Accountant",
      "Finance Manager",
      "Controller",
      "FP&A Analyst",
      "Treasury Analyst"
    ),
    "Human Resources" = c(
      "HR Specialist",
      "Recruiter",
      "HR Manager",
      "People Ops Specialist",
      "HR Director",
      "Compensation Analyst"
    ),
    "Operations" = c(
      "Operations Analyst",
      "Project Manager",
      "Operations Manager",
      "Business Analyst",
      "Operations Coordinator",
      "Scrum Master"
    ),
    "Customer Support" = c(
      "Customer Support Manager",
      "Support Specialist",
      "CS Director",
      "Onboarding Specialist",
      "Account Manager",
      "Technical Support Engineer"
    )
  )

  # seniority-based hourly rates:
  .senior_titles <- c(
    "Senior Engineer",
    "Lead Engineer",
    "Engineering Manager",
    "Staff Engineer",
    "Senior PM",
    "Product Director",
    "Sales Manager",
    "Sales Director",
    "Marketing Manager",
    "Finance Manager",
    "Controller",
    "HR Manager",
    "HR Director",
    "Operations Manager",
    "CS Director",
    "VP",
    "Chief",
    "Director",
    "Manager"
  )
  .mid_level_titles <- c(
    "Software Engineer",
    "DevOps Engineer",
    "QA Engineer",
    "Product Analyst",
    "UX Designer",
    "Account Executive",
    "Marketing Specialist",
    "Content Strategist",
    "SEO Analyst",
    "Brand Designer",
    "Growth Analyst",
    "Financial Analyst",
    "Accountant",
    "FP&A Analyst",
    "Treasury Analyst",
    "HR Specialist",
    "Recruiter",
    "People Ops Specialist",
    "Compensation Analyst",
    "Operations Analyst",
    "Business Analyst",
    "Operations Coordinator",
    "Scrum Master",
    "Technical Support Engineer"
  )
  .junior_titles <- c(
    "Associate PM",
    "SDR",
    "Business Development Rep",
    "Sales Representative",
    "Support Specialist",
    "Onboarding Specialist"
  )

  .hourly_rate <- function(title) {
    if (
      any(sapply(.senior_titles, function(t) {
        grepl(t, title, ignore.case = TRUE)
      }))
    ) {
      round(max(45, min(100, stats::rnorm(1, 65, 10))), 2)
    } else if (
      any(sapply(.mid_level_titles, function(t) {
        grepl(t, title, ignore.case = TRUE)
      }))
    ) {
      round(max(30, min(65, stats::rnorm(1, 45, 8))), 2)
    } else if (
      any(sapply(.junior_titles, function(t) {
        grepl(t, title, ignore.case = TRUE)
      }))
    ) {
      round(max(18, min(40, stats::rnorm(1, 28, 5))), 2)
    } else {
      round(max(25, min(55, stats::rnorm(1, 38, 7))), 2)
    }
  }

  # employees (200):
  employees <- data.table::rbindlist(lapply(
    seq_len(nrow(departments)),
    function(i) {
      dept_id_val   <- departments$id[i]
      dept_name_val <- departments$name[i]
      n <- dept_n[i]
      genders <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.52, 0.48))
      titles <- sample(titles_by_dept[[dept_name_val]], n, replace = TRUE)
      data.table::data.table(
        dept_id = dept_id_val,
        name = vapply(
          seq_len(n),
          function(j) {
            fn <- if (genders[j] == "M") {
              sample(first_m, 1)
            } else {
              sample(first_f, 1)
            }
            paste(fn, sample(last_names, 1))
          },
          character(1L)
        ),
        title = titles,
        gender = genders,
        hourly_rate = vapply(titles, .hourly_rate, numeric(1L))
      )
    }
  ))
  employees[, id := .I]
  employees[, employee_no := 1000L + id]

  n_emp <- nrow(employees)

  # hire dates: 80% pre-2024, 20% during 2024-2025
  pre_idx <- sample(c(TRUE, FALSE), n_emp, replace = TRUE, prob = c(0.80, 0.20))
  employees[,
    hire_date := as.Date(
      ifelse(
        pre_idx,
        as.integer(sample(
          seq(as.Date("2019-01-01"), as.Date("2023-12-15"), by = "day"),
          n_emp,
          replace = TRUE
        )),
        as.integer(sample(
          seq(DATE_FROM, DATE_TO - 90L, by = "day"),
          n_emp,
          replace = TRUE
        ))
      ),
      origin = "1970-01-01"
    )
  ]

  # exit dates: ~8% attrition
  has_exit <- sample(
    c(TRUE, FALSE),
    n_emp,
    replace = TRUE,
    prob = c(0.08, 0.92)
  )
  employees[,
    exit_date := as.Date(
      ifelse(
        has_exit,
        as.integer(sample(
          seq(as.Date("2024-06-01"), DATE_TO, by = "day"),
          n_emp,
          replace = TRUE
        )),
        NA_real_
      ),
      origin = "1970-01-01"
    )
  ]

  employees[,
    status := data.table::fifelse(
      !is.na(exit_date) & exit_date <= DATE_TO,
      "inactive",
      "active"
    )
  ]

  # job_level derived from title:
  employees[, job_level := data.table::fcase(
    grepl("Chief|CEO|CTO|CFO|COO|CHRO", title, ignore.case = TRUE), "L8",
    grepl("\\bVP\\b|Vice President",    title, ignore.case = TRUE), "L7",
    grepl("Director",                   title, ignore.case = TRUE), "L6",
    grepl("Manager|Controller",         title, ignore.case = TRUE), "L5",
    grepl("Lead|Staff|Principal",       title, ignore.case = TRUE), "L4",
    grepl("Senior|Sr\\.",               title, ignore.case = TRUE), "L3",
    grepl("SDR|Associate PM|Support Specialist|Onboarding Specialist|Business Development Rep|Sales Representative", title, ignore.case = TRUE), "L1",
    default = "L2"
  )]

  # birth_date: age 22-62 at time of hire
  employees[, birth_date := as.Date(
    as.integer(hire_date) - as.integer(round(stats::runif(n_emp, min = 22*365, max = 62*365))),
    origin = "1970-01-01"
  )]

  # education correlated with job_level
  edu_levels <- c("High School","Associate","Bachelor","Master","MBA","PhD")
  edu_probs <- list(
    L1 = c(0.35, 0.25, 0.28, 0.08, 0.03, 0.01),
    L2 = c(0.15, 0.20, 0.42, 0.15, 0.06, 0.02),
    L3 = c(0.05, 0.10, 0.45, 0.26, 0.10, 0.04),
    L4 = c(0.02, 0.06, 0.38, 0.32, 0.14, 0.08),
    L5 = c(0.01, 0.04, 0.28, 0.35, 0.22, 0.10),
    L6 = c(0.00, 0.02, 0.22, 0.36, 0.27, 0.13),
    L7 = c(0.00, 0.01, 0.15, 0.34, 0.32, 0.18),
    L8 = c(0.00, 0.00, 0.10, 0.30, 0.37, 0.23)
  )
  employees[, education := {
    mapply(function(lvl) sample(edu_levels, 1L, prob = edu_probs[[lvl]]), job_level)
  }]

  # fte: 85% full-time, 10% 0.75, 5% 0.5
  employees[, fte := sample(c(1.0, 0.75, 0.5), n_emp, replace = TRUE, prob = c(0.85, 0.10, 0.05))]

  # location_id: weighted toward HQ
  employees[, location_id := sample(1:8, n_emp, replace = TRUE,
    prob = c(0.35, 0.15, 0.12, 0.10, 0.10, 0.08, 0.05, 0.05))]

  # schedules:
  sched_type <- sample(
    c("full", "hybrid4", "hybrid3", "compressed"),
    n_emp,
    replace = TRUE,
    prob = c(0.65, 0.20, 0.10, 0.05)
  )

  schedules <- data.table::data.table(
    employee_id = employees$id,
    monday = "wfo",
    tuesday = "wfo",
    wednesday = "wfo",
    thursday = "wfo",
    friday = "wfo"
  )

  h4_idx <- which(sched_type == "hybrid4")
  h4_off <- sample(c("monday", "friday"), length(h4_idx), replace = TRUE)
  for (k in seq_along(h4_idx)) {
    data.table::set(schedules, h4_idx[k], h4_off[k], "wfh")
  }

  h3_idx <- which(sched_type == "hybrid3")
  h3_off <- sample(c("monday", "friday"), length(h3_idx), replace = TRUE)
  for (k in seq_along(h3_idx)) {
    data.table::set(schedules, h3_idx[k], "wednesday", "wfh")
    data.table::set(schedules, h3_idx[k], h3_off[k], "wfh")
  }

  cx_idx <- which(sched_type == "compressed")
  for (k in cx_idx) {
    data.table::set(schedules, k, "friday", "leave")
  }

  # --- Holidays ---
  holidays <- data.table::data.table(
    day = c(1L, 15L, 19L, 4L, 11L, 25L, 26L),
    month = c(1L, 1L, 6L, 7L, 11L, 12L, 12L),
    name = c(
      "New Year's Day",
      "MLK Day",
      "Juneteenth",
      "Independence Day",
      "Veterans Day",
      "Christmas Day",
      "Boxing Day"
    ),
    is_fixed = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  # per-employee behavior profiles:
  profiles <- data.table::data.table(
    employee_id = employees$id,
    attend_rate = round(stats::rbeta(n_emp, shape1 = 18, shape2 = 2), 4),
    arr_mean = as.integer(stats::rnorm(n_emp, mean = 478L, sd = 18L)),
    arr_sd = as.integer(stats::runif(n_emp, min = 8L, max = 22L)),
    dep_mean = as.integer(stats::rnorm(n_emp, mean = 1028L, sd = 18L)),
    dep_sd = as.integer(stats::runif(n_emp, min = 8L, max = 22L))
  )
  profiles[, arr_mean := pmax(440L, pmin(530L, arr_mean))]
  profiles[, dep_mean := pmax(960L, pmin(1100L, dep_mean))]

  # workday calendar:
  hol_key <- paste(holidays$day, holidays$month, sep = "-")

  workdays_dt <- data.table::data.table(
    date = seq(DATE_FROM, DATE_TO, by = "day")
  )
  workdays_dt[, dow := tolower(weekdays(date))]
  workdays_dt[, year := as.integer(lubridate::year(date))]
  workdays_dt[, month := as.integer(lubridate::month(date))]
  workdays_dt[, day := as.integer(lubridate::mday(date))]
  workdays_dt <- workdays_dt[!dow %in% c("saturday", "sunday")]
  workdays_dt <- workdays_dt[!paste(day, month, sep = "-") %in% hol_key]

  n_days <- nrow(workdays_dt)

  # employee x workday cross-join:
  emp_ref <- schedules[
    employees[, .(
      employee_id = id,
      emp_name = name,
      dept_id,
      hire_date,
      exit_date
    )],
    on = "employee_id"
  ]

  # attach location_id, job_level, fte from employees to emp_ref:
  emp_ref <- employees[, .(employee_id = id, location_id, job_level, fte)][
    emp_ref, on = "employee_id"
  ]

  emp_day <- workdays_dt[rep(seq_len(n_days), times = n_emp)]
  emp_day[, employee_id := rep(emp_ref$employee_id, each = n_days)]
  emp_day <- emp_ref[emp_day, on = "employee_id"]

  emp_day[,
    is_on_schedule := data.table::fcase(
      dow == "monday"    , monday %in% c("wfo", "wfh", "half")    ,
      dow == "tuesday"   , tuesday %in% c("wfo", "wfh", "half")   ,
      dow == "wednesday" , wednesday %in% c("wfo", "wfh", "half") ,
      dow == "thursday"  , thursday %in% c("wfo", "wfh", "half")  ,
      dow == "friday"    , friday %in% c("wfo", "wfh", "half")    ,
      default = FALSE
    )
  ]

  emp_day[,
    scheduled_type := data.table::fcase(
      dow == "monday"    , monday    ,
      dow == "tuesday"   , tuesday   ,
      dow == "wednesday" , wednesday ,
      dow == "thursday"  , thursday  ,
      dow == "friday"    , friday    ,
      default = "leave"
    )
  ]

  emp_day[,
    is_active := (date >= hire_date) & (is.na(exit_date) | date <= exit_date)
  ]

  # retain only scheduled and active rows:
  emp_day <- emp_day[is_on_schedule == TRUE & is_active == TRUE]

  # simulate attendance and time punches:
  emp_day <- profiles[emp_day, on = "employee_id"]

  emp_day[,
    season_factor := data.table::fcase(
      month %in% c(1L, 2L)   , 0.93 ,
      month %in% c(11L, 12L) , 0.95 ,
      month %in% c(7L, 8L)   , 0.97 ,
      default = 1.00
    )
  ]
  emp_day[, dow_factor := data.table::fifelse(dow == "monday", 0.96, 1.00)]
  emp_day[, eff_rate := pmin(0.99, attend_rate * season_factor * dow_factor)]
  emp_day[, is_present := stats::runif(.N) < eff_rate]

  # crawl arrival & departure draws for present employees:
  emp_day[, `:=`(arr_raw = NA_integer_, dep_raw = NA_integer_)]
  emp_day[
    is_present == TRUE,
    arr_raw := as.integer(round(stats::rnorm(.N, arr_mean, arr_sd)))
  ]
  emp_day[
    is_present == TRUE,
    dep_raw := as.integer(round(stats::rnorm(.N, dep_mean, dep_sd)))
  ]

  # clamp to realistic bounds:
  emp_day[
    is_present == TRUE,
    `:=`(
      arr_raw = pmax(420L, pmin(600L, arr_raw)), # 07:00–10:00
      dep_raw = pmax(900L, pmin(1140L, dep_raw)) # 15:00–19:00
    )
  ]

  # guarantee at least 5 hours gap:
  emp_day[
    is_present == TRUE & dep_raw < arr_raw + 300L,
    dep_raw := arr_raw + 300L
  ]

  # missing punch: ~5% of present rows:
  emp_day[, has_missing_punch := FALSE]
  emp_day[is_present == TRUE, has_missing_punch := stats::runif(.N) < 0.05]

  # derived time fields:
  emp_day[,
    arrival_mins := data.table::fifelse(
      is_present & !has_missing_punch,
      arr_raw,
      NA_integer_
    )
  ]
  emp_day[,
    departure_mins := data.table::fifelse(
      is_present & !has_missing_punch,
      dep_raw,
      NA_integer_
    )
  ]

  emp_day[,
    hours_worked := data.table::fifelse(
      is_present & !has_missing_punch,
      (departure_mins - arrival_mins - 60L) / 60.0,
      NA_real_
    )
  ]

  # adherence flags:
  emp_day[,
    is_late := data.table::fifelse(
      !is.na(arrival_mins),
      arrival_mins > (SHIFT_START + LATE_THRESH),
      NA
    )
  ]
  emp_day[,
    is_early_leave := data.table::fifelse(
      !is.na(departure_mins),
      departure_mins < (SHIFT_END - EARLY_THRESH),
      NA
    )
  ]

  # pay columns:
  # join hourly_rate from employees table:
  emp_day <- employees[, .(employee_id = id, hourly_rate)][
    emp_day,
    on = "employee_id"
  ]

  emp_day[, regular_pay := NA_real_]
  emp_day[, overtime_pay := NA_real_]
  emp_day[, gross_daily_pay := NA_real_]

  emp_day[
    is_present == TRUE & !has_missing_punch,
    `:=`(
      regular_pay = pmin(hours_worked, 8.0) * hourly_rate,
      overtime_pay = pmax(0.0, hours_worked - 8.0) * hourly_rate * 1.5
    )
  ]
  emp_day[
    is_present == TRUE & !has_missing_punch,
    gross_daily_pay := regular_pay + overtime_pay
  ]
  emp_day[
    is_present == FALSE,
    `:=`(
      regular_pay = 0.0,
      overtime_pay = 0.0,
      gross_daily_pay = 0.0
    )
  ]

  # has_missing_punch and absent remain NA for pay — set absent rows already done;
  # missing punch rows: set to 0 as well since we can't compute:
  emp_day[
    is_present == TRUE & has_missing_punch,
    `:=`(
      regular_pay = 0.0,
      overtime_pay = 0.0,
      gross_daily_pay = 0.0
    )
  ]

  # daily KPI score:
  emp_day[, daily_kpi_score := NA_real_]
  emp_day[
    is_present == TRUE & !has_missing_punch,
    daily_kpi_score := (as.integer(is_on_schedule & is_present) +
      as.integer(data.table::fcoalesce(!is_late, FALSE)) +
      as.integer(data.table::fcoalesce(hours_worked >= TARGET_HRS, FALSE))) /
      3.0 *
      100.0
  ]

  # attach department names:
  dept_lkp <- departments[, .(dept_id = id, dept_name = name)]
  emp_day <- dept_lkp[emp_day, on = "dept_id"]

  # attach office names:
  office_lkp <- offices[, .(location_id = id, location_name = name)]
  emp_day <- office_lkp[emp_day, on = "location_id"]

  data.table::setnames(
    emp_day,
    c("emp_name", "dow"),
    c("employee_name", "day_of_week")
  )

  # build final data.table:
  final_cols <- c(
    "employee_id",
    "employee_name",
    "dept_id",
    "dept_name",
    "location_id",
    "location_name",
    "job_level",
    "fte",
    "date",
    "year",
    "month",
    "day",
    "day_of_week",
    "scheduled_type",
    "is_on_schedule",
    "is_present",
    "arrival_mins",
    "departure_mins",
    "hours_worked",
    "is_late",
    "is_early_leave",
    "has_missing_punch",
    "daily_kpi_score",
    "hourly_rate",
    "regular_pay",
    "overtime_pay",
    "gross_daily_pay"
  )
  fact <- emp_day[, ..final_cols]

  # write Parquet warehouse (attendance):
  arrow::write_dataset(
    dataset  = arrow::as_arrow_table(fact),
    path     = file.path(parquet_root, "attendance"),
    partitioning          = c("year", "month", "day"),
    format                = "parquet",
    existing_data_behavior = "overwrite"
  )

  # Monthly payroll aggregation:
  monthly_agg <- fact[, .(
    scheduled_days = .N,
    days_present   = sum(is_present, na.rm = TRUE),
    days_absent    = sum(!is_present, na.rm = TRUE),
    days_late      = sum(is_late == TRUE, na.rm = TRUE),
    total_hours    = round(sum(hours_worked, na.rm = TRUE), 2L),
    basic_pay      = round(sum(regular_pay,  na.rm = TRUE), 2L),
    overtime_pay   = round(sum(overtime_pay, na.rm = TRUE), 2L)
  ), by = .(employee_id, employee_name, dept_id, dept_name,
            location_id, location_name, job_level, fte, year, month)]

  # Join payroll config:
  monthly_agg <- payroll_config[monthly_agg, on = "job_level"]

  monthly_agg[, housing_allowance   := round(basic_pay * housing_pct, 2L)]
  monthly_agg[, transport_allowance := as.numeric(transport_fixed)]
  monthly_agg[, meal_allowance      := as.numeric(meal_fixed)]
  monthly_agg[, medical_allowance   := as.numeric(medical_fixed)]
  monthly_agg[, total_allowances    := housing_allowance + transport_allowance +
                                       meal_allowance + medical_allowance]
  monthly_agg[, gross_pay      := round(basic_pay + overtime_pay + total_allowances, 2L)]
  monthly_agg[, income_tax     := round((basic_pay + overtime_pay) * tax_rate, 2L)]
  monthly_agg[, pension_deduction := round(basic_pay * pension_pct, 2L)]
  monthly_agg[, health_deduction  := as.numeric(health_premium)]
  monthly_agg[, total_deductions  := income_tax + pension_deduction + health_deduction]
  monthly_agg[, net_pay        := round(gross_pay - total_deductions, 2L)]

  payroll_cols <- c(
    "employee_id","employee_name","dept_id","dept_name",
    "location_id","location_name","job_level","fte",
    "year","month",
    "scheduled_days","days_present","days_absent","days_late","total_hours",
    "basic_pay","overtime_pay",
    "housing_allowance","transport_allowance","meal_allowance","medical_allowance","total_allowances",
    "gross_pay","income_tax","pension_deduction","health_deduction","total_deductions","net_pay"
  )

  arrow::write_dataset(
    dataset  = arrow::as_arrow_table(monthly_agg[, ..payroll_cols]),
    path     = file.path(parquet_root, "payroll"),
    partitioning          = c("year", "month"),
    format                = "parquet",
    existing_data_behavior = "overwrite"
  )

  # write DuckDB reference tables:
  .write_duckdb_tables(duckdb_path, departments, employees, schedules, holidays, offices, payroll_config)

  invisible(TRUE)
}

#' @keywords internal
.write_duckdb_tables <- function(
  duckdb_path,
  departments,
  employees,
  schedules,
  holidays,
  offices,
  payroll_config
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(
    conn = conn,
    statement = "CREATE TABLE departments (id INTEGER PRIMARY KEY, name VARCHAR)"
  )
  DBI::dbWriteTable(conn, "departments", departments, append = TRUE)

  DBI::dbExecute(
    conn = conn,
    statement = "
    CREATE TABLE offices (
      id      INTEGER PRIMARY KEY,
      name    VARCHAR,
      city    VARCHAR,
      country VARCHAR,
      region  VARCHAR
    )
  "
  )
  DBI::dbWriteTable(conn, "offices", offices, append = TRUE)

  DBI::dbExecute(
    conn = conn,
    statement = "
    CREATE TABLE payroll_config (
      job_level       VARCHAR PRIMARY KEY,
      level_label     VARCHAR,
      housing_pct     DOUBLE,
      transport_fixed DOUBLE,
      meal_fixed      DOUBLE,
      medical_fixed   DOUBLE,
      tax_rate        DOUBLE,
      pension_pct     DOUBLE,
      health_premium  DOUBLE
    )
  "
  )
  DBI::dbWriteTable(conn, "payroll_config", payroll_config, append = TRUE)

  DBI::dbExecute(
    conn = conn,
    statement = "
    CREATE TABLE employees (
      id          BIGINT  PRIMARY KEY,
      employee_no INTEGER,
      dept_id     INTEGER REFERENCES departments(id),
      location_id INTEGER REFERENCES offices(id),
      name        VARCHAR,
      title       VARCHAR,
      gender      VARCHAR CHECK (gender IN ('F','M')),
      birth_date  DATE,
      education   VARCHAR,
      job_level   VARCHAR,
      fte         DOUBLE NOT NULL DEFAULT 1.0,
      status      VARCHAR CHECK (status IN ('active','inactive')) DEFAULT 'active',
      hire_date   DATE,
      exit_date   DATE,
      hourly_rate DOUBLE NOT NULL DEFAULT 0.0
    )
  "
  )
  DBI::dbWriteTable(
    conn = conn,
    name = "employees",
    value = employees[, .(
      id,
      employee_no,
      dept_id,
      location_id,
      name,
      title,
      gender,
      birth_date,
      education,
      job_level,
      fte,
      status,
      hire_date,
      exit_date,
      hourly_rate
    )],
    append = TRUE
  )

  DBI::dbExecute(
    conn = conn,
    statement = "
    CREATE TABLE schedules (
      employee_id BIGINT  PRIMARY KEY REFERENCES employees(id),
      monday      VARCHAR CHECK (monday    IN ('wfo','wfh','leave','half')),
      tuesday     VARCHAR CHECK (tuesday   IN ('wfo','wfh','leave','half')),
      wednesday   VARCHAR CHECK (wednesday IN ('wfo','wfh','leave','half')),
      thursday    VARCHAR CHECK (thursday  IN ('wfo','wfh','leave','half')),
      friday      VARCHAR CHECK (friday    IN ('wfo','wfh','leave','half'))
    )
  "
  )
  DBI::dbWriteTable(
    conn = conn,
    name = "schedules",
    value = schedules,
    append = TRUE
  )

  DBI::dbExecute(
    conn = conn,
    statement = "
    CREATE TABLE holidays (
      day      INTEGER,
      month    INTEGER,
      name     VARCHAR,
      is_fixed BOOLEAN,
      PRIMARY KEY (day, month)
    )
  "
  )
  DBI::dbWriteTable(
    conn = conn,
    name = "holidays",
    value = holidays,
    append = TRUE
  )

  invisible(TRUE)
}

#' @keywords internal
.create_duckdb_schema <- function(duckdb_path) {
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(conn = conn)

  if (!"departments" %in% tables) {
    DBI::dbExecute(
      conn = conn,
      statement = "
      CREATE TABLE departments (
        id   INTEGER PRIMARY KEY,
        name VARCHAR
      );
    "
    )
  }

  if (!"offices" %in% tables) {
    DBI::dbExecute(
      conn = conn,
      statement = "
      CREATE TABLE offices (
        id      INTEGER PRIMARY KEY,
        name    VARCHAR,
        city    VARCHAR,
        country VARCHAR,
        region  VARCHAR
      );
    "
    )
  }

  if (!"payroll_config" %in% tables) {
    DBI::dbExecute(
      conn = conn,
      statement = "
      CREATE TABLE payroll_config (
        job_level       VARCHAR PRIMARY KEY,
        level_label     VARCHAR,
        housing_pct     DOUBLE,
        transport_fixed DOUBLE,
        meal_fixed      DOUBLE,
        medical_fixed   DOUBLE,
        tax_rate        DOUBLE,
        pension_pct     DOUBLE,
        health_premium  DOUBLE
      );
    "
    )
  }

  if (!"employees" %in% tables) {
    DBI::dbExecute(
      conn = conn,
      statement = "
      CREATE TABLE employees (
        id          BIGINT  PRIMARY KEY,
        employee_no INTEGER,
        dept_id     INTEGER REFERENCES departments(id),
        location_id INTEGER REFERENCES offices(id),
        name        VARCHAR,
        title       VARCHAR,
        gender      VARCHAR CHECK (gender IN ('F','M')),
        birth_date  DATE,
        education   VARCHAR,
        job_level   VARCHAR,
        fte         DOUBLE NOT NULL DEFAULT 1.0,
        status      VARCHAR CHECK (status IN ('active','inactive')) DEFAULT 'active',
        hire_date   DATE,
        exit_date   DATE,
        hourly_rate DOUBLE NOT NULL DEFAULT 0.0
      );
    "
    )
  }

  invisible(TRUE)
}

#' Initialize the HR Analytics Dashboard
#'
#' @description Idempotent initializer. Generates synthetic data on first run;
#'   on subsequent runs it only ensures the DuckDB schema exists.
#'
#' @param duckdb_path Path to the DuckDB file.
#' @param parquet_root Path to the Parquet warehouse root.
#' @export
initialize_app <- function(
  duckdb_path = Sys.getenv(
    x = "DUCKDB_PATH",
    unset = file.path(
      system.file(package = "HRDashboard"),
      "data",
      "base",
      "attendance.duckdb"
    )
  ),
  parquet_root = Sys.getenv(
    x = "PARQUET_ROOT",
    unset = file.path(
      system.file(package = "HRDashboard"),
      "data",
      "warehouse"
    )
  )
) {
  fs::dir_create(file.path(parquet_root, "attendance"), recurse = TRUE)
  fs::dir_create(file.path(parquet_root, "payroll"),    recurse = TRUE)
  fs::dir_create(dirname(duckdb_path), recurse = TRUE)

  warehouse_is_empty <- identical(
    length(fs::dir_ls(
      path   = file.path(parquet_root, "attendance"),
      type   = "directory",
      regexp = "year=\\d+"
    )),
    0L
  )

  if (warehouse_is_empty) {
    .generate_synthetic_data(
      duckdb_path = duckdb_path,
      parquet_root = parquet_root
    )
  } else {
    .create_duckdb_schema(duckdb_path)
  }

  invisible(TRUE)
}
