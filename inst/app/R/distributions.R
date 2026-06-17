# ui:

distributionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      tags$span(class = "panel-title", "Hours Worked Distribution")
    ),
    tags$div(
      class = "chart-time-container",
      echarts4r::echarts4rOutput(
        outputId = ns("distributions_graphs"),
        height   = "560px"
      )
    )
  )
}

# server:

distributionsServer <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    distribution_data <- reactive({
      f <- filters()

      dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
      emp_ids  <- if (!is.null(f$employees)   && !identical(f$employees,   "all")) f$employees   else NULL

      dt <- tryCatch(
        query_hours_distribution(
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        ),
        error = function(e) NULL
      )

      if (is.null(dt) || !nrow(dt)) return(data.table::data.table())
      data.table::setDT(dt)
      dt
    })

    output$distributions_graphs <- echarts4r::renderEcharts4r({
      graph_data <- distribution_data()
      req(nrow(graph_data) > 0L)

      f <- filters()
      period_label <- if (!is.null(f$date_from) && !is.null(f$date_to)) {
        sprintf("%s to %s", f$date_from, f$date_to)
      } else {
        "All dates"
      }
      total_punches <- sum(graph_data$punch_count, na.rm = TRUE)

      text_style <- list(
        fontSize   = 11,
        color      = colors$chart_text,
        fontFamily = font_primary
      )

      graph_data |>
        echarts4r::e_charts(bucket_label) |>
        echarts4r::e_bar(
          serie     = pct_of_total,
          name      = "% of Days",
          barWidth  = "85%",
          itemStyle = list(
            borderRadius = c(4, 4, 0, 0),
            color        = colors$present,
            borderColor  = colors$white,
            borderWidth  = 0
          )
        ) |>
        echarts4r::e_title(
          text        = "Hours Worked Distribution",
          subtext     = sprintf("Period: %s | %d days", period_label, total_punches),
          left        = "center",
          top         = 5,
          textStyle   = list(
            fontSize   = 14,
            fontFamily = font_primary,
            color      = colors$black,
            fontWeight = "bold"
          ),
          subtextStyle = text_style
        ) |>
        echarts4r::e_mark_line(
          lineStyle = list(color = "#f8973480", width = 1.5, type = "dashed"),
          label     = list(
            show       = TRUE,
            fontSize   = 10,
            fontFamily = font_primary,
            formatter  = "Target: 7.5h",
            position   = "end",
            color      = colors$present,
            backgroundColor = colors$white,
            padding    = c(2, 6)
          ),
          data = list(xAxis = "7.5h\u20138h"),
          z    = -1
        ) |>
        echarts4r::e_tooltip(
          trigger   = "axis",
          textStyle = text_style,
          formatter = htmlwidgets::JS(
            "function(params){
              var p = params[0];
              var v = Array.isArray(p.value) ? p.value[1] : p.value;
              var pct = (v != null && !isNaN(+v)) ? (+v).toFixed(1) + '%' : '-';
              return (p.axisValueLabel || p.name) + '<br/>' + p.marker + ' <b>' + pct + '</b>';
            }"
          ),
          axisPointer = list(type = "shadow", shadowStyle = list(opacity = 0.3))
        ) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_x_axis(
          name          = "Hours Worked",
          nameLocation  = "middle",
          nameGap       = 50,
          nameTextStyle = text_style,
          axisLabel     = list(
            fontSize   = 11,
            color      = colors$chart_text,
            fontFamily = font_primary,
            interval   = 0,
            rotate     = 30
          ),
          splitLine     = list(show = FALSE),
          axisTick      = list(alignWithLabel = TRUE)
        ) |>
        echarts4r::e_y_axis(
          name          = "% of Total Days",
          nameLocation  = "middle",
          nameGap       = 50,
          nameTextStyle = text_style,
          axisLabel     = c(
            text_style,
            list(formatter = htmlwidgets::JS("function(v){return v + '%';}"))
          ),
          splitLine = list(
            show      = TRUE,
            lineStyle = list(color = colors$bg_page, type = "dashed")
          )
        ) |>
        echarts4r::e_grid(
          bottom       = "13%",
          top          = 70,
          left         = 30,
          containLabel = TRUE
        ) |>
        echarts4r::e_toolbox(
          feature = list(
            saveAsImage = list(backgroundColor = "#ffffff"),
            dataZoom    = list(),
            reset       = list()
          )
        ) |>
        echarts4r::e_theme(name = "walden")
    })
  })
}
