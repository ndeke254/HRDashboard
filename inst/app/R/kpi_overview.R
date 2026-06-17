# ui:

kpi_overviewUI <- function(id) {
  ns <- NS(id)
  metrics <- get_category_choices(id)
  choices <- setNames(
    lapply(metrics, `[[`, "id"),
    sapply(metrics, `[[`, "label")
  )
  icon_name <- .kpi_icon(id)

  title_ui <- if (length(choices) > 1L) {
    tags$div(
      class = "panel-header",
      selectInput(
        inputId  = ns("overview_metric"),
        label    = NULL,
        choices  = choices,
        selectize = TRUE,
        selected = choices[[1]]
      )
    )
  } else {
    tags$span(class = "kpi-card-title", names(choices)[[1]])
  }

  tagList(
    tags$div(class = "kpi-cap"),
    tags$div(
      class = "kpi-body",
      tags$div(
        class = "kpi-card-header",
        title_ui,
        tags$div(
          class = "metric-icon-wrap",
          bsicons::bs_icon(icon_name, class = "metric-icon")
        )
      ),
      uiOutput(ns("metric_description")),
      uiOutput(ns("metric_card")),
      uiOutput(ns("sub_metrics"))
    )
  )
}

# helpers:
.kpi_icon <- function(id) {
  switch(
    EXPR = id,
    kpi_attendance = "calendar2-check",
    kpi_hours      = "graph-up-arrow",
    kpi_ontime     = "alarm",
    kpi_pay        = "cash-stack",
    kpi_overtime   = "clock-history",
    kpi_score      = "trophy",
    "bar-chart"
  )
}

.kpi_description <- function(id) {
  switch(
    id,
    kpi_attendance = "Attendance rate showing how often scheduled employees were present.",
    kpi_hours      = "Average hours worked per day by present employees.",
    kpi_ontime     = "On-time rate showing how often employees arrived before the late threshold.",
    kpi_pay        = "Total gross pay (regular + overtime) for all present employees.",
    kpi_overtime   = "Total overtime pay cost for the selected period.",
    kpi_score      = "Composite KPI score across schedule adherence, arrival, and hours worked.",
    ""
  )
}

.metric_suffix <- function(metric) {
  if (is.null(metric)) return("")
  if (grepl("pct|rate|score", metric, ignore.case = TRUE)) return("%")
  if (grepl("hours|hrs|worked", metric, ignore.case = TRUE)) return(" h")
  if (grepl("pay|gross|overtime|regular", metric, ignore.case = TRUE)) return("")
  ""
}

.is_clock_metric <- function(metric) {
  !is.null(metric) && grepl("arrival_mins|departure_mins", metric, ignore.case = TRUE)
}

.metric_higher_is_better <- function(metric) {
  if (is.null(metric)) return(TRUE)
  !grepl("arrival_mins|minutes_late|minutes_early", metric, ignore.case = TRUE)
}

.metric_quality <- function(metric, value) {
  if (is.null(metric) || is.null(value) || is.na(value)) return("")

  if (grepl("pct|rate|score", metric, ignore.case = TRUE)) {
    if (value >= 80) return("metric-good")
    if (value >= 50) return("metric-moderate")
    return("metric-bad")
  }
  if (grepl("avg_hours_worked|hours_worked", metric, ignore.case = TRUE)) {
    if (value >= 7.5) return("metric-good")
    if (value >= 6.5) return("metric-moderate")
    return("metric-bad")
  }
  ""
}

.fmt_minutes <- function(sign, mins) {
  if (mins >= 60L) {
    h <- mins %/% 60L
    m <- mins %% 60L
    if (m == 0L) paste0(sign, h, "h") else paste0(sign, h, "h ", m, "m")
  } else {
    paste0(sign, mins, " mins")
  }
}

# Pay metric ids that should use query_payroll_summary:
.pay_metrics <- c("total_gross_pay", "total_regular_pay", "total_overtime_pay",
                  "avg_daily_pay", "overtime_pct")

metric_card <- function(value, suffix, change_val, icon_name = NULL, metric = NULL) {
  higher_is_better <- .metric_higher_is_better(metric)
  quality_class    <- .metric_quality(metric, value)
  is_clock         <- .is_clock_metric(metric)
  is_pay           <- !is.null(metric) && metric %in% .pay_metrics

  display_value <- if (is.null(value) || is.na(value)) {
    "\u2014"
  } else if (is_clock) {
    total <- as.integer(round(value))
    sprintf("%02d:%02d", total %/% 60L, total %% 60L)
  } else if (is_pay) {
    formatC(value, format = "f", digits = 2, big.mark = ",")
  } else {
    value
  }

  value_label <- if (is_clock || is_pay) display_value else paste(display_value, suffix)

  fmt_change <- function(v) {
    display_v <- if (!higher_is_better) -v else v
    sign <- if (display_v > 0) "+" else if (display_v < 0) "-" else ""
    abs_v <- abs(v)
    if (is_clock) {
      .fmt_minutes(sign, as.integer(round(abs_v)))
    } else if (is_pay) {
      paste0(sign, formatC(abs_v, format = "f", digits = 2, big.mark = ","))
    } else {
      paste0(sign, abs_v, suffix)
    }
  }

  change_node <- if (is.null(change_val) || is.na(change_val)) {
    tags$span(class = "change-badge no-data", "\u2014")
  } else if (change_val == 0) {
    tags$span(class = "change-badge zero-change", "0")
  } else {
    beneficial <- (change_val > 0 && higher_is_better) ||
      (change_val < 0 && !higher_is_better)
    cls <- if (beneficial) "positive-change" else "negative-change"
    tags$span(class = paste("change-badge", cls), fmt_change(change_val))
  }

  sep_ui <- if (identical(suffix, "%") && !is.null(value) && !is.na(value)) {
    pct <- min(max(value, 0), 100)
    tags$div(
      class = "metric-progress",
      tags$div(
        class = trimws(paste("metric-progress-fill", quality_class)),
        style = paste0("width:", pct, "%")
      )
    )
  } else {
    tags$div(class = "kpi-divider")
  }

  tags$div(
    class = "metric-card",
    tags$span(class = "metric", value_label),
    sep_ui,
    tags$div(class = "metric-change text-end", change_node)
  )
}

# server:
kpi_overviewServer <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    ns        <- session$ns
    icon_name <- .kpi_icon(id)

    # every card has exactly one primary metric:
    fixed_metric   <- get_category_choices(id)[[1]]$id
    primary_metric <- reactive(fixed_metric)

    # internal fetch helper — routes to correct query based on metric type:
    .fetch_value <- function(metric, f) {
      tryCatch(
        {
          dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
          emp_ids  <- if (!is.null(f$employees)   && !identical(f$employees,   "all")) f$employees   else NULL

          if (metric %in% .pay_metrics) {
            dt <- query_payroll_summary(
              by        = "department",
              date_from = f$date_from,
              date_to   = f$date_to,
              dept_ids  = dept_ids,
              emp_ids   = emp_ids
            )
            if (is.null(dt) || !nrow(dt) || !(metric %in% names(dt))) return(NA_real_)
            round(as.numeric(sum(dt[[metric]], na.rm = TRUE)), digits = 2)

          } else if (identical(metric, "kpi_score")) {
            dt <- query_kpi_summary(
              by        = "department",
              date_from = f$date_from,
              date_to   = f$date_to,
              dept_ids  = dept_ids,
              emp_ids   = emp_ids
            )
            if (is.null(dt) || !nrow(dt) || !("kpi_score" %in% names(dt))) return(NA_real_)
            round(as.numeric(mean(dt$kpi_score, na.rm = TRUE)), digits = 1)

          } else {
            dt <- query_attendance_summary(
              by        = "department",
              date_from = f$date_from,
              date_to   = f$date_to,
              dept_ids  = dept_ids,
              emp_ids   = emp_ids
            )
            if (is.null(dt) || !nrow(dt) || !(metric %in% names(dt))) return(NA_real_)
            round(as.numeric(mean(dt[[metric]], na.rm = TRUE)), digits = 1)
          }
        },
        error = function(e) NA_real_
      )
    }

    current_value <- reactive({
      req(filters())
      .fetch_value(primary_metric(), filters())
    })

    output$metric_card <- renderUI({
      metric <- primary_metric()
      metric_card(
        value      = current_value(),
        suffix     = .metric_suffix(metric),
        change_val = NA_real_,
        icon_name  = icon_name,
        metric     = metric
      )
    })

    output$metric_description <- renderUI({
      desc <- .kpi_description(id)
      if (!nzchar(desc)) return(NULL)
      tags$div(class = "kpi-card-description", desc)
    })

    output$sub_metrics <- renderUI({
      # kpi_score: status pill derived from the score:
      if (identical(id, "kpi_score")) {
        score <- current_value()
        cfg <- if (is.na(score) || is.null(score)) {
          list(label = "No Data", bg = colors$ash_light, fg = colors$chart_text)
        } else if (score >= 80) {
          list(label = "On Track", bg = colors$on_track_bg, fg = colors$on_track)
        } else if (score >= 60) {
          list(label = "On Watch", bg = colors$watch_bg, fg = colors$watch)
        } else {
          list(label = "At Risk", bg = colors$at_risk_bg, fg = colors$at_risk)
        }
        return(tags$div(
          class = "sub-metrics-container text-center",
          tags$span(
            style = paste0(
              "background:", cfg$bg, ";color:", cfg$fg, ";",
              "border-radius:20px;padding:4px 16px;",
              "font-size:12px;font-weight:600;display:inline-block;"
            ),
            cfg$label
          )
        ))
      }

      NULL
    })
  })
}
