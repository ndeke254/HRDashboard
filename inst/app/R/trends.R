# ui:

trendsUI <- function(id) {
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
        inputId = ns("trend_metric"),
        label = "Metric",
        choices = choices,
        selectize = TRUE,
        selected = choices[[1]]
      )
    ),
    tags$div(
      class = "chart-time-container",
      echarts4r::echarts4rOutput(
        outputId = ns("trend_chart"),
        height = "560px"
      )
    )
  )
}

# server:

trendsServer <- function(id, filters, shared_metric = NULL) {
  moduleServer(id, function(input, output, session) {
    choices <- local({
      metrics <- get_category_choices(id)
      setNames(lapply(metrics, `[[`, "id"), sapply(metrics, `[[`, "label"))
    })

    # sync outward: when local picker changes, push to shared reactive
    if (!is.null(shared_metric)) {
      observeEvent(input$trend_metric, {
        if (!is.null(input$trend_metric) &&
            !identical(shared_metric(), input$trend_metric)) {
          shared_metric(input$trend_metric)
        }
      }, ignoreInit = TRUE)

      # sync inward: when shared reactive changes, update local picker
      observeEvent(shared_metric(), {
        req(shared_metric())
        if (!identical(input$trend_metric, shared_metric())) {
          updateSelectInput(session, "trend_metric", selected = shared_metric())
        }
      }, ignoreInit = TRUE)
    }

    metric_label <- reactive({
      hit <- names(choices)[vapply(
        choices,
        identical,
        logical(1L),
        input$trend_metric
      )]
      if (length(hit)) hit[[1L]] else input$trend_metric
    })

    # derive grouping from the active global filters:
    # employees selected → one line per employee
    # specific departments selected → one line per department
    # otherwise → single overall line
    active_by <- reactive({
      f <- filters()
      has_emp <- length(f$employees) > 0L && !identical(f$employees, "all")
      has_dept <- length(f$departments) > 0L && !identical(f$departments, "all")
      if (has_emp) {
        "employee"
      } else if (has_dept) {
        "department"
      } else {
        NULL
      }
    })

    trend_data <- reactive({
      req(input$trend_metric)
      by_val <- active_by()
      f      <- filters()

      dept_ids <- if (!is.null(f$departments) && !identical(f$departments, "all")) f$departments else NULL
      emp_ids  <- if (identical(by_val, "employee") && !is.null(f$employees) && !identical(f$employees, "all")) f$employees else NULL

      dt <- query_metric_trend(
        metric    = input$trend_metric,
        date_from = f$date_from,
        date_to   = f$date_to,
        dept_ids  = dept_ids,
        emp_ids   = emp_ids,
        by        = by_val,
        grain     = "week"
      )

      if (is.null(dt) || !nrow(dt)) return(data.table::data.table())
      data.table::setDT(dt)
      dt
    })

    output$trend_chart <- echarts4r::renderEcharts4r({
      dt <- trend_data()
      req(nrow(dt) > 0L)

      by_val <- active_by()
      suffix <- .compare_suffix(input$trend_metric)

      text_style <- list(
        fontSize = 11,
        color = colors$chart_text,
        fontFamily = font_primary
      )

      # resolve group label column and reshape to wide for multi-line:
      grp_col <- if (!is.null(by_val)) {
        switch(by_val,
          department = "dept_name",
          employee = "employee_name",
          NULL
        )
      }

      if (!is.null(grp_col) && grp_col %in% names(dt)) {
        plot_dt <- data.table::dcast(
          dt,
          period ~ get(grp_col),
          value.var = "value"
        )
      } else {
        plot_dt <- dt[, .(period, value)]
      }

      series_cols <- setdiff(names(plot_dt), "period")
      multi <- length(series_cols) > 1L

      # y-axis tick formatter:
      y_fmt <- if (identical(suffix, " m")) {
        htmlwidgets::JS(
          "function(v){var h=Math.floor(v/60),m=Math.round(v%60);return (h<10?'0':'')+h+':'+(m<10?'0':'')+m;}"
        )
      } else {
        htmlwidgets::JS(sprintf("function(v){return v+'%s';}", suffix))
      }

      # tooltip formatter — handles single and multi-series, all unit types:
      tt_fmt <- if (identical(suffix, " m")) {
        htmlwidgets::JS(
          "function(params){
            if(!params||!params.length)return'';
            var hdr=(params[0].axisValueLabel||params[0].name||'')+'<br/>';
            return hdr+params.map(function(p){
              var v=Array.isArray(p.value)?p.value[1]:p.value;
              var h=Math.floor(+v/60),m=Math.round(+v%60);
              return p.marker+' '+p.seriesName+': <b>'+(h<10?'0':'')+h+':'+(m<10?'0':'')+m+'</b>';
            }).join('<br/>');
          }"
        )
      } else {
        htmlwidgets::JS(sprintf(
          "function(params){
            if(!params||!params.length)return'';
            var hdr=(params[0].axisValueLabel||params[0].name||'')+'<br/>';
            return hdr+params.map(function(p){
              var v=Array.isArray(p.value)?p.value[1]:p.value;
              return p.marker+' '+p.seriesName+': <b>'+(+v).toFixed(1)+'%s</b>';
            }).join('<br/>');
          }",
          suffix
        ))
      }

      # mark line label formatter:
      mark_fmt <- if (identical(suffix, " m")) {
        htmlwidgets::JS(
          "function(p){var v=p.value,h=Math.floor(v/60),m=Math.round(v%60);return 'Avg '+(h<10?'0':'')+h+':'+(m<10?'0':'')+m;}"
        )
      } else {
        htmlwidgets::JS(sprintf(
          "function(p){return 'Avg '+p.value.toFixed(1)+'%s';}",
          suffix
        ))
      }

      f <- filters()
      period_label <- if (!is.null(f$date_from) && !is.null(f$date_to)) {
        sprintf("%s to %s", f$date_from, f$date_to)
      } else {
        "All dates"
      }
      group_suffix <- if (!is.null(by_val) && multi) {
        grain <- if (identical(by_val, "employee")) "employee" else "department"
        sprintf(" | %d %ss", length(series_cols), grain)
      } else {
        ""
      }

      entity_colors <- assign_entity_colors(series_cols)

      # build base chart then add one line + average mark per series column:
      chart <- plot_dt |>
        echarts4r::e_charts(period) |>
        echarts4r::e_title(
          text = sprintf("%s Trend", metric_label()),
          subtext = sprintf("Period: %s%s", period_label, group_suffix),
          left = "center",
          top = 5,
          textStyle = list(
            fontSize   = 14,
            fontFamily = font_primary,
            color      = colors$black,
            fontWeight = "bold"
          ),
          subtextStyle = text_style
        )

      for (i in seq_along(series_cols)) {
        col <- series_cols[[i]]
        col_color <- if (!multi) colors$on_track else entity_colors[[i]]

        chart <- chart |>
          echarts4r::e_line_(
            col,
            smooth     = TRUE,
            symbol     = "circle",
            symbolSize = 5,
            lineStyle  = list(width = 2, color = col_color),
            itemStyle  = list(color = col_color)
          ) |>
          echarts4r::e_mark_line(
            data = list(type = "average"),
            lineStyle = list(type = "dashed", width = 1, color = col_color),
            label = list(
              show = TRUE,
              position = "end",
              formatter = mark_fmt,
              fontSize = 10,
              fontFamily = font_primary,
              color = col_color,
              backgroundColor = colors$white,
              padding = c(2, 6)
            ),
            symbol = "none"
          )
      }

      chart |>
        echarts4r::e_tooltip(
          trigger = "axis",
          textStyle = text_style,
          formatter = tt_fmt,
          axisPointer = list(type = "line")
        ) |>
        echarts4r::e_x_axis(
          name = "Period in Dates",
          nameLocation = "middle",
          nameGap = 70,
          nameTextStyle = text_style,
          axisLabel = list(
            fontSize = 11,
            color = colors$chart_text,
            fontFamily = font_primary,
            rotate = 30
          ),
          splitLine = list(show = FALSE),
          axisTick = list(alignWithLabel = TRUE)
        ) |>
        echarts4r::e_y_axis(
          name = "Metric Value",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = text_style,
          axisLabel = c(
            text_style,
            list(formatter = y_fmt)
          ),
          splitLine = list(
            show = TRUE,
            lineStyle = list(color = colors$bg_page, type = "dashed")
          )
        ) |>
        echarts4r::e_legend(
          show = multi,
          bottom = 0,
          textStyle = text_style
        ) |>
        echarts4r::e_grid(
          top = 70,
          bottom = if (multi) "15%" else "10%",
          left = 20,
          containLabel = TRUE
        ) |>
        echarts4r::e_toolbox_feature("saveAsImage", backgroundColor = "#ffffff") |>
        echarts4r::e_theme(name = "walden") |>
        echarts4r::e_color(entity_colors)
    })
  })
}
