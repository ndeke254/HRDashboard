#' Workforce Analytics Module
#' @param id Module id.
#' @export
workforceUI <- function(id) {
  ns <- NS(id)
  tagList(
    # KPI row
    tags$section(
      class = "kpi-row kpi-row--workforce",
      tags$div(
        class = "panel panel-metric panel-metric-featured",
        uiOutput(ns("kpi_headcount"))
      ),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_fte"))),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_tenure"))),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_gender_ratio")))
    ),
    # Demographics charts grid
    tags$section(
      class = "workforce-grid",
      tags$div(class = "panel panel-chart wf-col-4",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Gender Diversity"),
          tags$span(class = "panel-subtitle", "Active employees")
        ),
        echarts4r::echarts4rOutput(ns("gender_chart"), height = "260px")
      ),
      tags$div(class = "panel panel-chart wf-col-4",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Age Distribution"),
          tags$span(class = "panel-subtitle", "Active employees")
        ),
        echarts4r::echarts4rOutput(ns("age_chart"), height = "260px")
      ),
      tags$div(class = "panel panel-chart wf-col-4",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Job Level Distribution"),
          tags$span(class = "panel-subtitle", "Active headcount")
        ),
        echarts4r::echarts4rOutput(ns("level_chart"), height = "260px")
      ),
      tags$div(class = "panel panel-chart wf-col-6",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Education Level"),
          tags$span(class = "panel-subtitle", "Active employees")
        ),
        echarts4r::echarts4rOutput(ns("edu_chart"), height = "280px")
      ),
      tags$div(class = "panel panel-chart wf-col-6",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Tenure Distribution"),
          tags$span(class = "panel-subtitle", "Years with company")
        ),
        echarts4r::echarts4rOutput(ns("tenure_chart"), height = "280px")
      ),
      tags$div(class = "panel panel-chart wf-col-12",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Absenteeism Rate by Department"),
          tags$span(class = "panel-subtitle", "% scheduled days absent")
        ),
        echarts4r::echarts4rOutput(ns("abs_chart"), height = "220px")
      )
    )
  )
}

#' @export
workforceServer <- function(id, filters) {
  moduleServer(id, function(input, output, session) {

    f <- reactive(filters())

    .dept_ids <- reactive({
      x <- f()$departments
      if (is.null(x) || identical(x, "all")) NULL else x
    })
    .loc_ids <- reactive({
      x <- f()$locations
      if (is.null(x) || identical(x, "all")) NULL else x
    })

    overview <- reactive({
      tryCatch(
        query_workforce_overview(dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
    })

    # ── KPI cards ─────────────────────────────────────────────────────────────

    .kpi_card <- function(label, value, suffix = "", featured = FALSE) {
      tags$div(
        class = if (featured) "kpi-card kpi-card--featured" else "kpi-card",
        tags$span(class = "kpi-cap", label),
        tags$span(class = "kpi-value", paste0(value, suffix))
      )
    }

    output$kpi_headcount <- renderUI({
      ov <- overview()
      v  <- if (!is.null(ov) && nrow(ov) > 0L) ov$active_employees else "\u2014"
      .kpi_card("Active Employees", v, featured = TRUE)
    })
    output$kpi_fte <- renderUI({
      ov <- overview()
      v  <- if (!is.null(ov) && nrow(ov) > 0L) formatC(ov$total_fte, format = "f", digits = 1) else "\u2014"
      .kpi_card("Total FTE", v)
    })
    output$kpi_tenure <- renderUI({
      ov <- overview()
      v  <- if (!is.null(ov) && nrow(ov) > 0L) paste0(round(ov$avg_tenure_yrs, 1)) else "\u2014"
      .kpi_card("Avg Tenure", v, " yrs")
    })
    output$kpi_gender_ratio <- renderUI({
      ov <- overview()
      if (!is.null(ov) && nrow(ov) > 0L) {
        f_pct <- round(100 * ov$female_count / max(ov$active_employees, 1), 1)
        v <- paste0(f_pct, "% F")
      } else {
        v <- "\u2014"
      }
      .kpi_card("Gender Ratio", v)
    })

    # ── Charts ────────────────────────────────────────────────────────────────

    output$gender_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_gender_diversity(by = "level", dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      totals <- dt[, .(n = sum(n)), by = gender]
      f_n <- totals[gender == "F", n]
      m_n <- totals[gender == "M", n]
      f_n <- if (length(f_n) == 0L) 0L else f_n
      m_n <- if (length(m_n) == 0L) 0L else m_n
      pie_dt <- data.table::data.table(
        name  = c("Female", "Male"),
        value = c(f_n, m_n)
      )

      pie_dt |>
        echarts4r::e_charts(name) |>
        echarts4r::e_pie(value, radius = c("40%", "70%"),
                         label = list(formatter = "{b}\n{d}%", fontSize = 11)) |>
        echarts4r::e_color(c(colors$primary, colors$accent)) |>
        echarts4r::e_legend(show = TRUE, bottom = 0) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    output$age_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_age_distribution(dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      dt |>
        echarts4r::e_charts(age_bucket) |>
        echarts4r::e_bar(n, name = "Employees",
                         itemStyle = list(color = colors$accent, borderRadius = c(0,3,3,0))) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = "20%", right = "10%", top = "5%", bottom = "10%") |>
        echarts4r::e_x_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    output$level_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_level_distribution(dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      dt[, label := paste0(job_level, " \u2013 ", level_label)]
      dt |>
        echarts4r::e_charts(label) |>
        echarts4r::e_bar(n, name = "Headcount",
                         itemStyle = list(color = colors$secondary, borderRadius = c(0,3,3,0))) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = "28%", right = "10%", top = "5%", bottom = "10%") |>
        echarts4r::e_x_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    output$edu_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_education_distribution(dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      dt |>
        echarts4r::e_charts(education) |>
        echarts4r::e_bar(n, name = "Employees",
                         itemStyle = list(color = colors$muted, borderRadius = c(0,3,3,0))) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = "22%", right = "10%", top = "5%", bottom = "10%") |>
        echarts4r::e_x_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    output$tenure_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_tenure_distribution(dept_ids = .dept_ids(), loc_ids = .loc_ids()),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      dt |>
        echarts4r::e_charts(tenure_bucket) |>
        echarts4r::e_bar(n, name = "Employees",
                         itemStyle = list(color = colors$gold, borderRadius = c(0,3,3,0))) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = "22%", right = "10%", top = "5%", bottom = "10%") |>
        echarts4r::e_x_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    output$abs_chart <- echarts4r::renderEcharts4r({
      dt <- tryCatch(
        query_absenteeism_rate(
          date_from = f()$date_from,
          date_to   = f()$date_to,
          dept_ids  = .dept_ids(),
          loc_ids   = .loc_ids()
        ),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      data.table::setorder(dt, -absenteeism_rate_pct)

      dt |>
        echarts4r::e_charts(dept_name) |>
        echarts4r::e_bar(absenteeism_rate_pct, name = "Absenteeism %",
                         itemStyle = list(color = colors$late, borderRadius = c(0,3,3,0))) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = htmlwidgets::JS("function(p){ return p[0].name + ': ' + p[0].value + '%'; }")) |>
        echarts4r::e_grid(left = "22%", right = "10%", top = "5%", bottom = "10%") |>
        echarts4r::e_x_axis(name = "Absenteeism %", splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })
  })
}
