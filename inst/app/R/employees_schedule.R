#' Weekly schedule and live attendance viewer
#'
#' @param id Module id.
#' @export
employees_scheduleUI <- function(id) {
  ns <- NS(id)
  metrics <- get_category_choices(id)
  choices <- setNames(
    lapply(metrics, `[[`, "id"),
    sapply(metrics, `[[`, "label")
  )

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        inputId = ns("schedule_metrics"),
        label = "Schedule Metrics",
        choices = choices,
        width = NULL,
        selectize = TRUE,
        selected = choices[[1]]
      )
    ),
    tags$div(
      class = "chart-time-container",
      uiOutput(ns("live_view"))
    )
  )
}

# helpers — resolve the monday of a given (year, week_label) → date:
.week_monday <- function(year, week_label, tz) {
  today <- lubridate::today(tz = tz)
  monday_this_week <- today - (lubridate::wday(today, week_start = 1) - 1L)
  if (
    is.null(year) ||
      is.na(year) ||
      identical(year, "") ||
      identical(year, "all")
  ) {
    return(monday_this_week)
  }
  if (
    is.null(week_label) ||
      is.na(week_label) ||
      identical(week_label, "") ||
      identical(week_label, "all")
  ) {
    return(monday_this_week)
  }

  parts <- strsplit(week_label, "-")[[1L]]
  if (length(parts) < 3L) {
    return(monday_this_week)
  }

  month_abbr <- parts[1L]
  week_n <- as.integer(parts[3L])
  if (is.na(week_n)) {
    return(monday_this_week)
  }

  month_num <- match(month_abbr, month.abb)
  if (is.na(month_num)) {
    return(monday_this_week)
  }

  first <- as.Date(sprintf("%d-%02d-01", as.integer(year), month_num))
  last <- seq(first, length.out = 2L, by = "month")[2L] - 1L
  all_days <- seq(first, last, by = "1 day")
  mondays <- all_days[lubridate::wday(all_days, week_start = 1) == 1L]
  idx <- which(ceiling(lubridate::mday(mondays) / 7) == week_n)

  if (!length(idx)) {
    return(monday_this_week)
  }
  mondays[idx[1L]]
}

.week_label_for <- function(monday) {
  paste0(
    lubridate::month(monday, label = TRUE, abbr = TRUE),
    "-Week-",
    ceiling(lubridate::mday(monday) / 7)
  )
}

# server:

employees_scheduleServer <- function(id, filters = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── weekly schedule reactives ─────────────────────────────────────────────

    displayed_week <- reactive({
      # Always show the current calendar week (Monday-anchored)
      today  <- lubridate::today(tz = tz)
      monday <- today - (lubridate::wday(today, week_start = 1L) - 1L)
      dates  <- seq(from = monday, by = "1 day", length.out = 5L)
      list(
        monday     = monday,
        dates      = dates,
        week_label = .week_label_for(monday),
        year       = lubridate::year(monday),
        today      = today
      )
    })

    week_map <- reactive({
      w <- displayed_week()
      wm <- data.table::data.table(
        day = tolower(weekdays(w$dates, abbreviate = FALSE)),
        date = w$dates,
        md = format(w$dates, "%m-%d")
      )

      holidays <- tryCatch(read_holidays(), error = function(e) NULL)

      if (!is.null(holidays) && nrow(holidays)) {
        data.table::setDT(holidays)
        holidays[, date := as.Date(date)]
        holidays[, md := format(date, "%m-%d")]

        fixed <- holidays[is_fixed == TRUE, .(md, name)]
        moveable <- holidays[is_fixed == FALSE, .(date, name)]

        wm[fixed, on = "md", holiday := i.name]
        wm[
          moveable,
          on = "date",
          holiday := data.table::fcoalesce(holiday, i.name)
        ]
      } else {
        wm[, holiday := NA_character_]
      }

      wm[, md := NULL]
      wm
    })

    schedule_raw <- reactive({
      req(!is.null(filters))
      f <- filters()

      sched <- read_employees_schedule()
      data.table::setDT(sched)

      if (
        !identical(f$departments, "all") &&
          !identical(f$departments, "") &&
          !is.null(f$departments) &&
          length(f$departments) > 0L
      ) {
        sched <- sched[dept_id %in% as.integer(f$departments)]
      }
      if (
        !identical(f$employees, "all") &&
          !identical(f$employees, "") &&
          !is.null(f$employees) &&
          length(f$employees) > 0L
      ) {
        sched <- sched[employee_id %in% as.integer(f$employees)]
      }

      days <- c("monday", "tuesday", "wednesday", "thursday", "friday")
      sched[,
        (days) := lapply(.SD, function(x) data.table::fcase(
          x == "wfo",   "WFO",
          x == "wfh",   "WFH",
          x == "leave", "Leave",
          x == "half",  "Half",
          default = toupper(x)
        )),
        .SDcols = days
      ]
      sched
    })

    attendance_wide <- reactive({
      w <- displayed_week()
      f <- filters()

      monday <- w$monday
      friday <- monday + 4L
      emp_ids <- if (!is.null(f$employees) && !identical(f$employees, "all")) f$employees else NULL

      att <- tryCatch(
        query_attendance_status(
          by        = "employee",
          date_from = monday,
          date_to   = friday,
          emp_ids   = emp_ids
        ),
        error = function(e) NULL
      )

      if (is.null(att) || !nrow(att)) {
        return(data.table::data.table(employee_id = integer(0)))
      }

      data.table::setDT(att)
      att[, day_of_week := tolower(weekdays(as.Date(date)))]
      att[
        ,
        attendance_status := data.table::fcase(
          is_present & !is_on_schedule,                         "Out of Schedule",
          is_present & (isTRUE(is_late) | isTRUE(is_early_leave)), "Irregular",
          is_present,                                           "Present",
          !is_present & is_on_schedule,                         "Absent",
          default = "Off"
        )
      ]

      data.table::dcast(
        att,
        employee_id ~ day_of_week,
        value.var = "attendance_status"
      )
    })

    schedule_combined <- reactive({
      sched <- data.table::copy(schedule_raw())
      att <- attendance_wide()

      merged <- data.table::merge.data.table(
        x = sched,
        y = att,
        by = "employee_id",
        all.x = TRUE,
        suffixes = c("", "_attendance")
      )

      days <- c("monday", "tuesday", "wednesday", "thursday", "friday")
      attendance_cols <- paste0(days, "_attendance")
      existing_att <- attendance_cols[attendance_cols %in% names(merged)]
      existing_days <- gsub("_attendance$", "", existing_att)

      if (length(existing_days)) {
        merged[
          ,
          (existing_days) := Map(
            data.table::fcoalesce,
            .SD[, existing_att, with = FALSE],
            .SD[, existing_days, with = FALSE]
          )
        ]
        merged[, (existing_att) := NULL]
      }

      data.table::setorder(merged, name)
      merged
    })

    employee_kpi_status <- reactive({
      req(!is.null(filters))
      f        <- filters()
      dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
      emp_ids  <- if (!is.null(f$employees)   && !identical(f$employees,   "all")) f$employees   else NULL
      tryCatch(
        query_kpi_summary(
          by        = "employee",
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        ),
        error = function(e) NULL
      )
    })

    # ── weekly table cell renderers ───────────────────────────────────────────

    make_cell <- function(day_name, wm) {
      holiday_name <- wm[day == day_name, holiday]
      is_holiday <- length(holiday_name) > 0L && !is.na(holiday_name[1L])

      bg_map <- list(
        "WFO"             = colors$wfo_bg,
        "WFH"             = colors$wfh_bg,
        "Half"            = colors$wfh_bg,
        "Leave"           = colors$leave,
        "Present"         = colors$present_bg,
        "Absent"          = colors$absent_bg,
        "Irregular"       = colors$irregular_bg,
        "Out of Schedule" = colors$out_of_schedule_bg,
        "Scheduled"       = colors$scheduled
      )
      cell_fg <- colors$black

      function(value) {
        if (is_holiday) {
          return(htmltools::div(
            style = paste(
              "background:#F1EFE8;color:#0a1e2b;",
              "border-radius:6px;padding:4px 8px;",
              "font-size:11px;text-align:center;"
            ),
            htmltools::div("HOLIDAY"),
            htmltools::div(
              style = "font-size:10px;opacity:.7;",
              holiday_name[1L]
            )
          ))
        }

        if (is.null(value) || is.na(value) || identical(value, "")) {
          value <- "Off"
        }
        bg <- bg_map[[value]] %||% colors$row_alt_bg

        htmltools::div(
          style = glue::glue(
            "background:{bg};color:{cell_fg};",
            "border-radius:6px;padding:4px 10px;",
            "font-size:11px;font-weight:400;",
            "text-align:center;min-width:70px;"
          ),
          value
        )
      }
    }

    make_header <- function(day_name, wm, today) {
      row <- wm[day == day_name]
      label <- tools::toTitleCase(day_name)
      date_str <- format(row$date, "%d %b")
      is_today <- length(row$date) > 0L && row$date == today
      primary <- colors$primary

      function(value) {
        htmltools::div(
          style = if (is_today) {
            glue::glue("color:{primary};font-weight:600;")
          } else {
            ""
          },
          htmltools::div(label),
          htmltools::div(
            style = paste0(
              "font-size:10px;color:",
              colors$chart_text,
              ";font-weight:400;"
            ),
            date_str
          )
        )
      }
    }

    make_name_cell <- function(dt) {
      function(value, index) {
        dept <- dt$dept_name[index]
        htmltools::div(
          htmltools::div(
            style = paste0(
              "font-size:12px;font-weight:500;color:",
              colors$black,
              ";"
            ),
            value
          ),
          htmltools::div(
            style = paste0(
              "font-size:10px;color:",
              colors$chart_text,
              ";"
            ),
            dept
          )
        )
      }
    }

    # ── live attendance cell renderers ────────────────────────────────────────

    make_check_type_cell <- function(value) {
      if (isTRUE(value == "In")) {
        htmltools::div(
          style = paste0(
            "background:",
            colors$present_bg,
            ";color:",
            colors$on_track,
            ";border-radius:4px;padding:3px 10px;font-size:11px;font-weight:500;text-align:center;"
          ),
          "In"
        )
      } else {
        htmltools::div(
          style = paste0(
            "background:",
            colors$wfh_bg,
            ";color:",
            colors$black,
            ";border-radius:4px;padding:3px 10px;font-size:11px;font-weight:500;text-align:center;"
          ),
          "Out"
        )
      }
    }

    live_name_cell <- function(value, index, data) {
      if (nrow(data) == 0L) {
        return(value)
      }
      dept <- data$dept_name[index]
      htmltools::div(
        htmltools::div(
          style = paste0(
            "font-size:12px;font-weight:500;color:",
            colors$black,
            ";"
          ),
          value
        ),
        htmltools::div(
          style = paste0("font-size:10px;color:", colors$chart_text, ";"),
          dept
        )
      )
    }

    diff_cell <- function(value, data, index) {
      diff_mins <- data$diff_mins[index]

      diff_label <- if (is.na(diff_mins)) {
        "-"
      } else if (abs(diff_mins) >= 60) {
        hrs <- abs(diff_mins) %/% 60
        mins <- abs(diff_mins) %% 60
        paste0(
          if (diff_mins < 0) "-" else "+",
          hrs,
          "h ",
          sprintf("%02dm", mins)
        )
      } else {
        paste0(if (diff_mins < 0) "-" else "+", abs(diff_mins), "m")
      }

      arrow <- switch(EXPR = value,
        up = "↑",
        down = "↓",
        "→"
      )
      color <- switch(
        EXPR = value,
        up = colors$trend_up,
        down = colors$trend_down,
        colors$trend_neutral
      )

      htmltools::div(
        style = "display:flex;align-items:center;gap:6px;",
        htmltools::span(
          style = paste0("font-size:14px;font-weight:600;color:", color, ";"),
          arrow
        ),
        htmltools::span(
          style = paste0("font-size:11px;color:", colors$chart_text, ";"),
          diff_label
        )
      )
    }

    # ── live data ─────────────────────────────────────────────────────────────

    live_data <- reactive({
      invalidateLater(300000, session)
      read_live_attendance()
    })

    # ── view switch ───────────────────────────────────────────────────────────

    output$live_view <- renderUI({
      if (identical(input$schedule_metrics, "schedule_this_week")) {
        tagList(
          reactable::reactableOutput(ns("weekly_output"))
          # uiOutput(ns("weekly_legend"))
        )
      } else {
        tagList(
          tags$div(
            class = "small mb-3 d-flex gap-2 align-items-end",
            tags$div(
              class = "live-badge",
              tags$span(class = "live-dot"),
              "LIVE"
            ),
            htmltools::div(
              class = "small fst-italic",
              textOutput(outputId = ns("last_refresh_time"))
            )
          ),
          reactable::reactableOutput(outputId = ns("schedule_output"))
        )
      }
    })

    # ── weekly schedule table render ──────────────────────────────────────────

    output$weekly_output <- reactable::renderReactable({
      dt <- schedule_combined()
      wm <- week_map()
      w <- displayed_week()
      req(nrow(dt) > 0L)

      kpi_dt <- employee_kpi_status()
      # if (!is.null(kpi_dt) && nrow(kpi_dt) > 0L) {
      #   data.table::setDT(kpi_dt)
      #   kpi_dt[
      #     ,
      #     kpi_status := data.table::fcase(
      #       kpi_score >= 80, "On Track",
      #       kpi_score >= 60, "On Watch",
      #       !is.na(kpi_score), "At Risk",
      #       default = "No Data"
      #     )
      #   ]
      #   dt <- data.table::merge.data.table(
      #     dt,
      #     kpi_dt[, .(employee_id, kpi_score, kpi_status)],
      #     by = "employee_id",
      #     all.x = TRUE
      #   )
      # } else {
      #   dt[, kpi_status := NA_character_]
      #   dt[, kpi_score := NA_real_]
      # }

      day_names <- c("monday", "tuesday", "wednesday", "thursday", "friday")

      day_cols <- setNames(
        lapply(day_names, function(d) {
          reactable::colDef(
            name = tools::toTitleCase(d),
            cell = make_cell(d, wm),
            header = make_header(d, wm, w$today),
            align = "center",
            minWidth = 90
          )
        }),
        day_names
      )

      black <- colors$black
      status_col <- list(
        kpi_score = reactable::colDef(
          name = "Score",
          align = "left",
          minWidth = 140,
          sticky = "right",
          style = list(borderLeft = paste0("2px solid ", colors$ash_light)),
          headerStyle = list(
            borderLeft = paste0("2px solid ", colors$ash_light)
          ),
          cell = function(value) {
            if (is.null(value) || is.na(value)) {
              return(htmltools::div(
                style = paste0("color:", colors$ash, ";font-size:11px;"),
                "—"
              ))
            }
            bar_color <- if (value >= 80) {
              "#167070"
            } else if (value >= 60) {
              "#FB9334"
            } else {
              "#c0303a"
            }
            fg_color <- if (value >= 80) {
              colors$on_track
            } else if (value >= 60) {
              colors$watch
            } else {
              colors$at_risk
            }
            bar_width <- paste0(min(value, 100L), "%")
            htmltools::div(
              style = "display:flex;align-items:center;gap:6px;padding:0 4px;",
              htmltools::div(
                style = paste0(
                  "flex:1;background:",
                  colors$ash_light,
                  ";border-radius:4px;height:8px;overflow:hidden;"
                ),
                htmltools::div(
                  style = paste0(
                    "background:",
                    bar_color,
                    ";width:",
                    bar_width,
                    ";height:100%;border-radius:4px;"
                  )
                )
              ),
              htmltools::span(
                style = paste0(
                  "font-size:11px;font-weight:500;color:",
                  fg_color,
                  ";min-width:36px;text-align:right;"
                ),
                paste0(round(value, 1L), "%")
              )
            )
          }
        ),
        kpi_status = reactable::colDef(show = FALSE)
      )

      all_cols <- c(
        list(
          employee_id = reactable::colDef(show = FALSE),
          dept_id = reactable::colDef(show = FALSE),
          dept_name = reactable::colDef(show = FALSE),
          name = reactable::colDef(
            name = "Employee",
            minWidth = 160,
            cell = make_name_cell(dt)
          )
        ),
        day_cols
        # status_col
      )

      reactable::reactable(
        data = dt,
        columns = all_cols,
        wrap = FALSE,
        sortable = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        compact = FALSE,
        searchable = TRUE,
        defaultPageSize = 10,
        defaultSorted = list(name = "asc"),
        theme = reactable::reactableTheme(
          headerStyle = list(
            fontSize = "11px",
            fontWeight = "500",
            textTransform = "uppercase",
            color = colors$chart_text,
            borderBottom = glue::glue("2px solid {black}")
          )
        )
      )
    })

    output$weekly_legend <- renderUI({
      tags$div(
        style = paste0(
          "display:flex;flex-wrap:wrap;gap:14px;align-items:center;",
          "padding:10px 4px 2px;font-size:11px;color:",
          colors$chart_text,
          ";"
        ),
        tags$span(
          style = "font-weight:600;letter-spacing:0.03em;",
          "KPI Score"
        ),
        tags$span(
          style = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "background:",
            colors$on_track_bg,
            ";color:",
            colors$on_track,
            ";",
            "border-radius:20px;padding:3px 10px;font-weight:700;"
          ),
          tags$span(
            style = "display:inline-block;width:8px;height:8px;border-radius:50%;background:rgba(22,112,112,0.45);"
          ),
          "On Track ≥ 80%"
        ),
        tags$span(
          style = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "background:",
            colors$watch_bg,
            ";color:",
            colors$watch,
            ";",
            "border-radius:20px;padding:3px 10px;font-weight:700;"
          ),
          tags$span(
            style = "display:inline-block;width:8px;height:8px;border-radius:50%;background:rgba(251,147,52,0.5);"
          ),
          "On Watch 60–79%"
        ),
        tags$span(
          style = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "background:",
            colors$at_risk_bg,
            ";color:",
            colors$at_risk,
            ";",
            "border-radius:20px;padding:3px 10px;font-weight:700;"
          ),
          tags$span(
            style = "display:inline-block;width:8px;height:8px;border-radius:50%;background:rgba(192,48,58,0.45);"
          ),
          "At Risk < 60%"
        )
      )
    })

    # ── live attendance table render ──────────────────────────────────────────

    output$schedule_output <- reactable::renderReactable({
      empty_data <- data.frame(
        employee_id = integer(0),
        name = character(0),
        dept_name = character(0),
        check_type = character(0),
        check_time = character(0),
        diff_mins = character(0),
        trend = character(0)
      )

      attendance_data <- live_data()
      attendance_data <- if (nrow(attendance_data) > 0L) {
        attendance_data[
          ,
          check_type := fcase(
            check_type == "I", "In",
            check_type == "O", "Out",
            default = check_type
          )
        ]
      } else {
        empty_data
      }

      columns <- list(
        employee_id = reactable::colDef(show = FALSE),
        dept_name = reactable::colDef(show = FALSE),
        name = reactable::colDef(
          name = "Employee",
          minWidth = 160,
          cell = function(value, index) {
            live_name_cell(value, index, attendance_data)
          }
        ),
        check_type = reactable::colDef(
          name = "Status",
          minWidth = 90,
          cell = make_check_type_cell,
          align = "center"
        ),
        check_time = reactable::colDef(
          name = "Time",
          minWidth = 90,
          cell = function(value) {
            time_formatted <- lubridate::ymd_hms(string = value, tz = "UTC") |>
              format(tz = tz, format = "%H:%M:%S")
            htmltools::div(
              style = paste0(
                "font-size:12px;font-weight:500;color:",
                colors$black,
                ";"
              ),
              time_formatted
            )
          }
        ),
        diff_mins = reactable::colDef(show = FALSE),
        trend = reactable::colDef(
          name = "Trend",
          minWidth = 90,
          cell = function(value, index) diff_cell(value, attendance_data, index)
        )
      )

      reactable::reactable(
        data = attendance_data,
        columns = columns,
        wrap = FALSE,
        sortable = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        compact = FALSE,
        searchable = TRUE,
        defaultPageSize = 10,
        theme = reactable::reactableTheme(
          headerStyle = list(
            fontSize = "11px",
            fontWeight = "500",
            textTransform = "uppercase",
            color = colors$chart_text,
            borderBottom = glue::glue("2px solid {colors$black}")
          )
        )
      )
    })

    output$last_refresh_time <- renderText({
      invalidateLater(30000, session)
      paste("Live attendance as of", format(Sys.time(), "%H:%M"))
    })
  })
}
