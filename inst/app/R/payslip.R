#' Payroll & Payslip Module
#' @param id Module id.
#' @export
payslipUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Payroll KPI summary
    tags$section(
      class = "kpi-row kpi-row--payroll",
      tags$div(class = "panel panel-metric panel-metric-featured", uiOutput(ns("kpi_gross"))),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_net"))),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_allowances"))),
      tags$div(class = "panel panel-metric", uiOutput(ns("kpi_deductions")))
    ),

    # Main payroll content area
    tags$div(
      class = "payroll-body",

      # Left: payroll cost trend chart
      tags$div(
        class = "panel panel-chart payroll-trend-panel",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Monthly Labor Cost"),
          tags$span(class = "panel-subtitle", "Gross vs Net pay trend")
        ),
        echarts4r::echarts4rOutput(ns("cost_trend"), height = "300px")
      ),

      # Right: payslip viewer
      tags$div(
        class = "panel payslip-panel",
        tags$div(class = "panel-header",
          tags$span(class = "panel-title", "Employee Payslip"),
          tags$span(class = "panel-subtitle", "Generate & download")
        ),
        tags$div(
          class = "payslip-controls",
          selectInput(
            inputId  = ns("ps_employee"),
            label    = "Employee",
            choices  = NULL,
            width    = "100%"
          ),
          tags$div(
            class = "payslip-period",
            selectInput(ns("ps_year"),  "Year",  choices = get_years_choices(),
                        selected = as.character(format(Sys.Date(), "%Y")), width = "100%"),
            selectInput(ns("ps_month"), "Month",
                        choices = setNames(1:12, month.abb),
                        selected = as.integer(format(Sys.Date(), "%m")), width = "100%")
          ),
          tags$div(
            class = "payslip-actions",
            actionButton(ns("ps_generate"), "Generate",  class = "btn-apply-filters"),
            downloadButton(ns("ps_download"), "Download", class = "btn-download-payslip")
          )
        ),
        uiOutput(ns("payslip_view"))
      )
    )
  )
}

#' @export
payslipServer <- function(id, filters) {
  moduleServer(id, function(input, output, session) {

    f <- reactive(filters())

    # Populate employee selector from current dept filter
    observe({
      dept_ids <- f()$departments
      updateSelectInput(
        session = session,
        inputId = "ps_employee",
        choices = {
          tryCatch(
            get_employees_choices(dept_id = if (identical(dept_ids, "all")) "all" else dept_ids),
            error = function(e) c("Error loading employees" = "")
          )
        }
      )
    })

    # Payroll KPIs from payroll parquet
    payroll_kpis <- reactive({
      yr <- as.integer(format(f()$date_from, "%Y"))
      mo <- as.integer(format(f()$date_from, "%m"))
      tryCatch(
        query_payroll_kpi_summary(
          year     = yr,
          month    = mo,
          dept_ids = if (!identical(f()$departments, "all")) f()$departments else NULL,
          loc_ids  = if (!identical(f()$locations, "all")) f()$locations else NULL
        ),
        error = function(e) NULL
      )
    })

    .fmt <- function(v) {
      if (is.null(v) || is.na(v)) return("\u2014")
      paste0("$", formatC(as.numeric(v), format = "f", digits = 0, big.mark = ","))
    }

    output$kpi_gross <- renderUI({
      kpis <- payroll_kpis()
      v <- if (!is.null(kpis) && nrow(kpis) > 0L) .fmt(kpis$total_gross) else "\u2014"
      tags$div(class = "kpi-card kpi-card--featured",
               tags$span(class = "kpi-cap", "Total Gross Pay"),
               tags$span(class = "kpi-value kpi-value--sm", v))
    })
    output$kpi_net <- renderUI({
      kpis <- payroll_kpis()
      v <- if (!is.null(kpis) && nrow(kpis) > 0L) .fmt(kpis$total_net) else "\u2014"
      tags$div(class = "kpi-card",
               tags$span(class = "kpi-cap", "Net Pay"),
               tags$span(class = "kpi-value kpi-value--sm", v))
    })
    output$kpi_allowances <- renderUI({
      kpis <- payroll_kpis()
      v <- if (!is.null(kpis) && nrow(kpis) > 0L) .fmt(kpis$total_allowances) else "\u2014"
      tags$div(class = "kpi-card",
               tags$span(class = "kpi-cap", "Allowances"),
               tags$span(class = "kpi-value kpi-value--sm", v))
    })
    output$kpi_deductions <- renderUI({
      kpis <- payroll_kpis()
      v <- if (!is.null(kpis) && nrow(kpis) > 0L) .fmt(kpis$total_deductions) else "\u2014"
      tags$div(class = "kpi-card",
               tags$span(class = "kpi-cap", "Deductions"),
               tags$span(class = "kpi-value kpi-value--sm", v))
    })

    # Cost trend chart
    output$cost_trend <- echarts4r::renderEcharts4r({
      yr <- as.integer(format(f()$date_from, "%Y"))
      dt <- tryCatch(
        query_payroll_cost_trend(
          year     = yr,
          dept_ids = if (!identical(f()$departments, "all")) f()$departments else NULL,
          loc_ids  = if (!identical(f()$locations, "all")) f()$locations else NULL
        ),
        error = function(e) NULL
      )
      req(!is.null(dt) && nrow(dt) > 0L)

      dt |>
        echarts4r::e_charts(period) |>
        echarts4r::e_line(total_gross, name = "Gross Pay",
                          itemStyle = list(color = colors$primary),
                          lineStyle  = list(color = colors$primary, width = 2)) |>
        echarts4r::e_line(total_net, name = "Net Pay",
                          itemStyle = list(color = colors$accent),
                          lineStyle  = list(color = colors$accent, width = 2)) |>
        echarts4r::e_area(total_allowances, name = "Allowances",
                          itemStyle = list(color = colors$gold),
                          areaStyle  = list(opacity = 0.2)) |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = "12%", right = "5%", top = "15%", bottom = "15%") |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 10, rotate = 30)) |>
        echarts4r::e_y_axis(axisLabel = list(
          formatter = htmlwidgets::JS("function(v){ return '$' + (v/1000).toFixed(0) + 'k'; }"),
          fontSize = 10
        )) |>
        echarts4r::e_theme_custom('{"backgroundColor":"transparent"}')
    })

    # Payslip data (triggered by Generate button)
    payslip_data <- eventReactive(input$ps_generate, {
      req(input$ps_employee, input$ps_year, input$ps_month)
      tryCatch(
        query_payslip(
          employee_id = as.integer(input$ps_employee),
          year  = as.integer(input$ps_year),
          month = as.integer(input$ps_month)
        ),
        error = function(e) NULL
      )
    })

    # Render payslip HTML
    output$payslip_view <- renderUI({
      ps <- payslip_data()
      if (is.null(ps) || nrow(ps) == 0L) {
        return(tags$div(
          class = "payslip-empty",
          tags$i(class = "fa-regular fa-file-lines"),
          tags$p("Select an employee and period, then click Generate.")
        ))
      }

      period_label <- paste(month.abb[ps$month], ps$year)

      tags$div(
        class = "payslip",

        # Header
        tags$div(
          class = "payslip-header",
          tags$div(
            class = "payslip-header-left",
            tags$h3(class = "payslip-name", ps$employee_name),
            tags$p(class = "payslip-meta",
                   paste0("ID #", ps$employee_no, " \u00b7 ", ps$title)),
            tags$p(class = "payslip-meta",
                   paste0(ps$department_name, " \u00b7 ", ps$office_name, ", ", ps$country))
          ),
          tags$div(
            class = "payslip-header-right",
            tags$span(class = "payslip-period-badge", period_label),
            tags$p(class = "payslip-level-badge", paste0(ps$job_level, " \u2013 ", ps$level_label))
          )
        ),

        # Attendance summary
        tags$div(
          class = "payslip-attendance",
          tags$div(class = "payslip-att-item",
            tags$span(class = "payslip-att-label", "Days Present"),
            tags$span(class = "payslip-att-value", ps$days_present)
          ),
          tags$div(class = "payslip-att-item",
            tags$span(class = "payslip-att-label", "Days Absent"),
            tags$span(class = "payslip-att-value", ps$days_absent)
          ),
          tags$div(class = "payslip-att-item",
            tags$span(class = "payslip-att-label", "Total Hours"),
            tags$span(class = "payslip-att-value", paste0(round(ps$total_hours, 1L), " h"))
          ),
          tags$div(class = "payslip-att-item",
            tags$span(class = "payslip-att-label", "Days Late"),
            tags$span(class = "payslip-att-value", ps$days_late)
          )
        ),

        # Earnings + Deductions side by side
        tags$div(
          class = "payslip-tables",

          # Earnings
          tags$div(
            class = "payslip-table-wrap",
            tags$table(
              class = "payslip-table",
              tags$thead(tags$tr(tags$th("Earnings"), tags$th("Amount"))),
              tags$tbody(
                tags$tr(tags$td("Basic Pay"),          tags$td(.fmt_pay(ps$basic_pay))),
                tags$tr(tags$td("Overtime Pay"),       tags$td(.fmt_pay(ps$overtime_pay))),
                tags$tr(tags$td("Housing Allowance"),  tags$td(.fmt_pay(ps$housing_allowance))),
                tags$tr(tags$td("Transport Allowance"),tags$td(.fmt_pay(ps$transport_allowance))),
                tags$tr(tags$td("Meal Allowance"),     tags$td(.fmt_pay(ps$meal_allowance))),
                tags$tr(tags$td("Medical Allowance"),  tags$td(.fmt_pay(ps$medical_allowance))),
                tags$tr(class = "payslip-subtotal",
                        tags$td("Gross Pay"), tags$td(.fmt_pay(ps$gross_pay)))
              )
            )
          ),

          # Deductions
          tags$div(
            class = "payslip-table-wrap",
            tags$table(
              class = "payslip-table",
              tags$thead(tags$tr(tags$th("Deductions"), tags$th("Amount"))),
              tags$tbody(
                tags$tr(tags$td("Income Tax"),        tags$td(.fmt_pay(ps$income_tax))),
                tags$tr(tags$td("Pension"),           tags$td(.fmt_pay(ps$pension_deduction))),
                tags$tr(tags$td("Health Insurance"),  tags$td(.fmt_pay(ps$health_deduction))),
                tags$tr(class = "payslip-subtotal",
                        tags$td("Total Deductions"), tags$td(.fmt_pay(ps$total_deductions)))
              )
            )
          )
        ),

        # Net pay bar
        tags$div(
          class = "payslip-net",
          tags$span(class = "payslip-net-label", "Net Pay"),
          tags$span(class = "payslip-net-value", .fmt_pay(ps$net_pay))
        )
      )
    })

    # Download handler — HTML payslip
    output$ps_download <- downloadHandler(
      filename = function() {
        paste0(
          "payslip_",
          gsub("\\s+", "_", tolower(input$ps_employee)), "_",
          input$ps_year, "_",
          sprintf("%02d", as.integer(input$ps_month)),
          ".html"
        )
      },
      content = function(file) {
        ps <- payslip_data()
        if (is.null(ps) || nrow(ps) == 0L) {
          writeLines("<p>No data found.</p>", file)
          return(invisible(NULL))
        }
        period_label <- paste(month.abb[ps$month], ps$year)
        html <- htmltools::tagList(
          htmltools::tags$html(
            htmltools::tags$head(
              htmltools::tags$meta(charset = "UTF-8"),
              htmltools::tags$title(paste("Payslip \u2014", ps$employee_name, period_label)),
              htmltools::tags$style(
                "body{font-family:'Inter',sans-serif;max-width:700px;margin:40px auto;color:#0F1C1A;}",
                "h2{margin:0 0 4px;font-size:20px;} .meta{color:#6C8A88;font-size:12px;margin:0;}",
                "table{width:100%;border-collapse:collapse;margin-bottom:16px;}",
                "th{background:#E5EFEE;text-align:left;padding:8px;font-size:11px;text-transform:uppercase;}",
                "td{padding:7px 8px;border-bottom:1px solid #E2E8F0;font-size:13px;}",
                "td:last-child{text-align:right;}",
                ".sub td{font-weight:700;} .net{background:#0F1C1A;color:#fff;padding:14px 20px;",
                "display:flex;justify-content:space-between;border-radius:6px;margin-top:16px;}",
                ".net-val{font-size:22px;font-weight:700;}"
              )
            ),
            htmltools::tags$body(
              htmltools::tags$h2(ps$employee_name),
              htmltools::tags$p(class = "meta", paste0("ID #", ps$employee_no, " \u00b7 ", ps$title)),
              htmltools::tags$p(class = "meta", paste0(ps$department_name, " \u00b7 ", ps$office_name, ", ", ps$country)),
              htmltools::tags$p(class = "meta", paste0("Period: ", period_label, " \u00b7 Level: ", ps$job_level, " \u2013 ", ps$level_label)),
              htmltools::tags$br(),
              htmltools::tags$table(
                htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Earnings"), htmltools::tags$th("Amount"))),
                htmltools::tags$tbody(
                  htmltools::tags$tr(htmltools::tags$td("Basic Pay"),          htmltools::tags$td(.fmt_pay(ps$basic_pay))),
                  htmltools::tags$tr(htmltools::tags$td("Overtime Pay"),       htmltools::tags$td(.fmt_pay(ps$overtime_pay))),
                  htmltools::tags$tr(htmltools::tags$td("Housing Allowance"),  htmltools::tags$td(.fmt_pay(ps$housing_allowance))),
                  htmltools::tags$tr(htmltools::tags$td("Transport Allowance"),htmltools::tags$td(.fmt_pay(ps$transport_allowance))),
                  htmltools::tags$tr(htmltools::tags$td("Meal Allowance"),     htmltools::tags$td(.fmt_pay(ps$meal_allowance))),
                  htmltools::tags$tr(htmltools::tags$td("Medical Allowance"),  htmltools::tags$td(.fmt_pay(ps$medical_allowance))),
                  htmltools::tags$tr(class = "sub", htmltools::tags$td("Gross Pay"), htmltools::tags$td(.fmt_pay(ps$gross_pay)))
                )
              ),
              htmltools::tags$table(
                htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Deductions"), htmltools::tags$th("Amount"))),
                htmltools::tags$tbody(
                  htmltools::tags$tr(htmltools::tags$td("Income Tax"),       htmltools::tags$td(.fmt_pay(ps$income_tax))),
                  htmltools::tags$tr(htmltools::tags$td("Pension"),          htmltools::tags$td(.fmt_pay(ps$pension_deduction))),
                  htmltools::tags$tr(htmltools::tags$td("Health Insurance"), htmltools::tags$td(.fmt_pay(ps$health_deduction))),
                  htmltools::tags$tr(class = "sub", htmltools::tags$td("Total Deductions"), htmltools::tags$td(.fmt_pay(ps$total_deductions)))
                )
              ),
              htmltools::tags$div(class = "net",
                htmltools::tags$span("Net Pay"),
                htmltools::tags$span(class = "net-val", .fmt_pay(ps$net_pay))
              )
            )
          )
        )
        writeLines(as.character(html), file)
      }
    )
  })
}

# Internal helper — format currency
.fmt_pay <- function(v) {
  if (is.null(v) || is.na(v)) return("\u2014")
  paste0("$", formatC(as.numeric(v), format = "f", digits = 2, big.mark = ","))
}
