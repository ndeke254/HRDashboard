#' Compare a metric across selected employees or departments
#'
#' @description Horizontal bar chart that ranks entities (employees or
#'   departments) by the chosen metric, with a vertical reference line at
#'   the overall mean and colour intensity scaling with the value. The
#'   comparison scope respects the dashboard's period filters and can be
#'   narrowed to specific picks via the in-module selectors.
#'
#' @param id Module id.
#' @export

# ui:

comparisonUI <- function(id) {
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
        inputId = ns("compare_metric"),
        label = "Metric",
        choices = choices,
        selectize = TRUE,
        selected = choices[[1]]
      )
    ),
    div(
      class = "chart-breakdown-container",
      echarts4r::echarts4rOutput(
        outputId = ns("comparison_chart"),
        height = "560px"
      )
    )
  )
}

# helpers:

# shared entity palette — same order used by trends and distributions:
.entity_palette <- c(
  colors$comp_col_1,
  colors$comp_col_2,
  colors$comp_col_3,
  colors$comp_col_4,
  colors$comp_col_5,
  "#9333ea"
)

# deterministic label → color: sort labels alphabetically, cycle through palette.
# returns a named character vector so the same label always gets the same color
# regardless of which chart or what order the data arrives in.
assign_entity_colors <- function(labels) {
  sorted_unique <- sort(unique(as.character(labels)))
  n <- length(.entity_palette)
  color_map <- setNames(
    .entity_palette[((seq_along(sorted_unique) - 1L) %% n) + 1L],
    sorted_unique
  )
  unname(color_map[as.character(labels)])
}

# pick a unit suffix from the metric name:
.compare_suffix <- function(metric) {
  if (is.null(metric)) {
    return("")
  }
  if (grepl("pct|adherence|rate|score", metric, ignore.case = TRUE)) {
    return("%")
  }
  if (grepl("hours|hrs", metric, ignore.case = TRUE)) {
    return(" h")
  }
  if (grepl("mins|minutes", metric, ignore.case = TRUE)) {
    return(" m")
  }
  ""
}

# server:
comparisonServer <- function(id, filters, shared_metric = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    metrics <- get_category_choices(id)
    choices <- setNames(
      lapply(metrics, `[[`, "id"),
      sapply(metrics, `[[`, "label")
    )

    # sync outward: when local picker changes, push to shared reactive
    if (!is.null(shared_metric)) {
      observeEvent(input$compare_metric, {
        if (!is.null(input$compare_metric) &&
            !identical(shared_metric(), input$compare_metric)) {
          shared_metric(input$compare_metric)
        }
      }, ignoreInit = TRUE)

      # sync inward: when shared reactive changes, update local picker
      observeEvent(shared_metric(), {
        req(shared_metric())
        if (!identical(input$compare_metric, shared_metric())) {
          updateSelectInput(session, "compare_metric", selected = shared_metric())
        }
      }, ignoreInit = TRUE)
    }

    # derive grain from active filters:
    # employees selected → compare employees; otherwise compare departments
    active_by <- reactive({
      f <- filters()
      has_emp <- length(f$employees) > 0L && !identical(f$employees, "all")
      if (has_emp) "employee" else "department"
    })

    # pay metric ids:
    .pay_metrics <- c("total_gross_pay", "total_regular_pay", "total_overtime_pay",
                      "avg_daily_pay", "overtime_pct")

    # raw data → route to the correct query based on metric type:
    raw_data <- reactive({
      req(input$compare_metric)
      by_val <- active_by()
      f      <- filters()
      metric <- input$compare_metric

      dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
      emp_ids  <- if (identical(by_val, "employee") && !is.null(f$employees) && !identical(f$employees, "all")) f$employees else NULL

      dt <- if (identical(metric, "kpi_score")) {
        query_kpi_summary(
          by        = by_val,
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        )
      } else if (metric %in% .pay_metrics) {
        query_payroll_summary(
          by        = by_val,
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        )
      } else {
        query_attendance_summary(
          by        = by_val,
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        )
      }

      if (is.null(dt) || !nrow(dt)) return(data.table::data.table())
      data.table::setDT(dt)
      # alias the target column to 'figure' so comparison_data() is unchanged:
      if (metric %in% names(dt)) data.table::setnames(dt, metric, "figure")
      dt
    })

    # normalised → label + value, sorted desc:
    comparison_data <- reactive({
      req(input$compare_metric)
      by_val <- active_by()

      dt <- raw_data()
      req(nrow(dt) > 0L)

      out <- if (identical(by_val, "employee")) {
        dt[, .(
          entity_id = as.character(employee_id),
          label = employee_name,
          value = as.numeric(figure)
        )]
      } else {
        dt[, .(
          entity_id = as.character(dept_id),
          label = dept_name,
          value = as.numeric(figure)
        )]
      }

      out <- out[!is.na(value)]
      data.table::setorder(out, -value)
      out
    })

    # render → horizontal bars with mean reference line:
    output$comparison_chart <- echarts4r::renderEcharts4r({
      dt <- comparison_data()
      req(nrow(dt) > 0L)

      by_val <- active_by()
      grain_label <- if (identical(by_val, "employee")) {
        "Employee"
      } else {
        "Department"
      }
      metric_lbl <- {
        hit <- names(choices)[vapply(
          choices,
          identical,
          logical(1L),
          input$compare_metric
        )]
        if (length(hit)) hit[[1L]] else input$compare_metric
      }
      suffix <- .compare_suffix(input$compare_metric)

      f <- filters()
      period_label <- if (!is.null(f$date_from) && !is.null(f$date_to)) {
        sprintf("%s to %s", f$date_from, f$date_to)
      } else {
        "All dates"
      }
      vmin <- min(dt$value, na.rm = TRUE)
      vmax <- max(dt$value, na.rm = TRUE)
      vmean <- round(mean(dt$value, na.rm = TRUE), digits = 2)

      fmt_r <- function(mins) {
        sprintf(
          "%02d:%02d",
          as.integer(mins %/% 60L),
          as.integer(round(mins %% 60))
        )
      }
      mean_label <- if (identical(suffix, " m")) {
        fmt_r(vmean)
      } else {
        sprintf("%.1f%s", vmean, suffix)
      }

      # colour bars by entity label only when a specific selection is active;
      # fall back to a single colour when showing the unfiltered all-entity view:
      has_specific <- {
        f2 <- filters()
        (length(f2$employees) > 0L && !identical(f2$employees, "all")) ||
          (length(f2$departments) > 0L && !identical(f2$departments, "all"))
      }
      dt[
        ,
        bar_colour := if (has_specific) {
          assign_entity_colors(label)
        } else {
          colors$on_track
        }
      ]

      # reverse factor levels → highest value sits at the top of the chart:
      plot_dt <- data.table::copy(dt)
      plot_dt[, label := factor(label, levels = rev(label))]

      text_style <- list(
        fontSize = 11,
        color = colors$chart_text,
        fontFamily = font_primary
      )

      plot_dt |>
        echarts4r::e_charts(label) |>
        echarts4r::e_bar(
          serie = value,
          name = "Value",
          barWidth = "70%",
          itemStyle = list(borderRadius = c(0, 4, 4, 0))
        ) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_add_nested("itemStyle", color = "bar_colour") |>
        echarts4r::e_title(
          text = sprintf("%s by %s", metric_lbl, grain_label),
          subtext = sprintf(
            "Period: %s | %d %ss",
            period_label,
            nrow(dt),
            tolower(grain_label)
          ),
          left = "center",
          top = 5,
          textStyle = list(
            fontSize = 14,
            fontWeight = "bold",
            color = colors$black,
            fontFamily = font_primary
          ),
          subtextStyle = text_style
        ) |>
        echarts4r::e_tooltip(
          trigger = "axis",
          axisPointer = list(type = "shadow"),
          textStyle = text_style,
          formatter = htmlwidgets::JS(sprintf(
            "
            function(params) {
              if (!params || params.length === 0) return '';
                var p = params[0];
                var val = parseFloat(p.value[0]);
                var suffix = '%s';
                var formatted;

              if (suffix === ' m') {
                var h = Math.floor(val / 60);
                var m = Math.round(val %% 60);
                formatted = (h<10?'0':'')+h+':'+(m<10?'0':'')+m;
              } else if (suffix === ' h') {
                formatted = val.toFixed(1) + ' hours';
              } else {
               formatted = val.toFixed(1) + suffix;
              }

              return '<b>' + p.name + '</b><br/>' + p.marker + ' ' + formatted;
            }
           ",
            suffix
          ))
        ) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_x_axis(
          name = metric_lbl,
          nameLocation = "middle",
          nameGap = 30,
          nameTextStyle = text_style,
          axisLabel = list(
            fontSize = 11,
            color = colors$chart_text,
            fontFamily = font_primary,
            formatter = htmlwidgets::JS(sprintf(
              "function(v){
								var suffix = '%s';
								if (suffix === ' m') {
									var h = Math.floor(v / 60);
									var m = Math.round(v %% 60);
									return (h<10?'0':'')+h+':'+(m<10?'0':'')+m;
								}
								return v + suffix;
							}",
              suffix
            ))
          ),
          splitLine = list(
            show = TRUE,
            lineStyle = list(color = colors$bg_page, type = "dashed")
          )
        ) |>
        echarts4r::e_y_axis(
          axisLabel = list(
            fontSize = 11,
            color = colors$black,
            fontFamily = font_primary
          ),
          axisTick = list(show = FALSE),
          axisLine = list(show = FALSE),
          splitLine = list(show = FALSE)
        ) |>
        echarts4r::e_mark_line(
          data = list(xAxis = vmean),
          lineStyle = list(
            color = "#f59e0b",
            width = 1.5,
            type = "dashed"
          ),
          label = list(
            show = TRUE,
            position = "end",
            formatter = sprintf("Mean %s", mean_label),
            fontSize = 10,
            color = "#92400e",
            backgroundColor = colors$white,
            padding = c(2, 6),
            fontFamily = font_primary
          ),
          symbol = "none"
        ) |>
        echarts4r::e_grid(
          top = 70,
          bottom = 30,
          left = 20,
          right = 60,
          containLabel = TRUE
        ) |>
        echarts4r::e_toolbox_feature("saveAsImage", backgroundColor = "#ffffff") |>
        echarts4r::e_theme(name = "walden")
    })
  })
}
