#' Visualize attendance status proportions as a doughnut chart
#'
#' @param id Module id.
#' @export
attendance_proportionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header breakdown-header",
      tags$div(class = "item", "Attendance Proportions")
    ),
    tags$div(
      class = "chart-breakdown-container",
      echarts4r::echarts4rOutput(ns("schedule_chart"), height = "500px")
    )
  )
}

# server:

attendance_proportionsServer <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    status_data <- reactive({
      f <- filters()

      dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
      emp_ids  <- if (!is.null(f$employees)   && !identical(f$employees,   "all")) f$employees   else NULL

      att <- tryCatch(
        query_attendance_status(
          by        = "employee",
          date_from = f$date_from,
          date_to   = f$date_to,
          dept_ids  = dept_ids,
          emp_ids   = emp_ids
        ),
        error = function(e) NULL
      )

      if (is.null(att) || !nrow(att)) return(data.table::data.table())
      data.table::setDT(att)

      att[
        ,
        status := data.table::fcase(
          is_present & !is_on_schedule,  "Out of Schedule",
          is_present & (isTRUE(is_late) | isTRUE(is_early_leave)), "Irregular",
          is_present,                    "Present",
          !is_present & is_on_schedule,  "Absent",
          default = "Off"
        )
      ]

      result <- att[, .(count = .N), by = status]
      result[, pct := round(100 * count / sum(count), 1)]
      data.table::setorder(result, -count)
      result
    })

    output$schedule_chart <- echarts4r::renderEcharts4r({
      dt <- status_data()
      req(nrow(dt) > 0L)

      f <- filters()
      period_label <- if (!is.null(f$date_from) && !is.null(f$date_to)) {
        sprintf("%s to %s", f$date_from, f$date_to)
      } else {
        "All dates"
      }
      total <- sum(dt$count)

      text_style <- list(
        fontSize = 11,
        color = colors$chart_text,
        fontFamily = font_primary
      )

      # pivot to wide: one row, one column per status, values = pct
      dt[, dummy := "Attendance"]
      wide_dt <- data.table::dcast(
        dt,
        dummy ~ status,
        value.var = "pct",
        fill = 0
      )
      series_cols <- setdiff(names(wide_dt), "dummy")
      bar_colors <- assign_entity_colors(series_cols)

      chart <- wide_dt |>
        echarts4r::e_charts(dummy)

      for (i in seq_along(series_cols)) {
        col <- series_cols[[i]]
        is_last <- i == length(series_cols)
        chart <- chart |>
          echarts4r::e_bar_(
            col,
            stack = "total",
            barWidth = "30%",
            label = list(
              show = TRUE,
              position = "inside",
              formatter = htmlwidgets::JS(
                "function(p){return p.value>4?p.value.toFixed(1)+'%':''}"
              ),
              fontSize = 11,
              fontFamily = font_primary,
              color = "#ffffff",
              fontWeight = "bold"
            ),
            itemStyle = list(
              borderRadius = if (is_last) c(0, 4, 4, 0) else 0,
              borderWidth = 0
            )
          )
      }

      chart |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_title(
          text = "Attendance Status Distribution",
          subtext = sprintf("Period: %s | %d records", period_label, total),
          left = "center",
          top = 5,
          textStyle = list(
            fontSize = 14,
            fontFamily = font_primary,
            color = colors$black,
            fontWeight = "bold"
          ),
          subtextStyle = text_style
        ) |>
        echarts4r::e_tooltip(
          trigger = "axis",
          textStyle = text_style,
          axisPointer = list(type = "shadow"),
          formatter = htmlwidgets::JS(
            "function(params){
              if(!params||!params.length)return'';
              return params.map(function(p){
                var v=Array.isArray(p.value)?p.value[0]:p.value;
                return p.marker+' '+p.seriesName+': <b>'+(+v).toFixed(1)+'%</b>';
              }).join('<br/>');
            }"
          )
        ) |>
        echarts4r::e_x_axis(
          name = "% of Records",
          nameLocation = "middle",
          nameGap = 30,
          nameTextStyle = text_style,
          max = 100,
          axisLabel = list(
            fontSize = 11,
            color = colors$chart_text,
            fontFamily = font_primary,
            formatter = htmlwidgets::JS("function(v){return v+'%';}")
          ),
          splitLine = list(
            show = TRUE,
            lineStyle = list(color = colors$bg_page, type = "dashed")
          )
        ) |>
        echarts4r::e_y_axis(
          axisLabel = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLine = list(show = FALSE),
          splitLine = list(show = FALSE)
        ) |>
        echarts4r::e_legend(show = TRUE, bottom = 0, textStyle = text_style) |>
        echarts4r::e_grid(
          top = 70,
          bottom = "20%",
          left = 20,
          right = 20,
          containLabel = TRUE
        ) |>
        echarts4r::e_toolbox_feature("saveAsImage", backgroundColor = "#ffffff") |>
        echarts4r::e_theme(name = "walden") |>
        echarts4r::e_color(bar_colors)
    })
  })
}
