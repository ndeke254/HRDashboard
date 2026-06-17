server <- function(input, output, session) {
  # Department → employee cascade:
  observeEvent(input$departments, {
    req(input$departments)
    dept_ids <- if ("all" %in% input$departments) "all" else input$departments
    updateSelectizeInput(
      session  = session,
      inputId  = "employees",
      choices  = get_employees_choices(dept_id = dept_ids),
      selected = if (identical(dept_ids, "all")) "all"
    )
  })

  # Current filters (date range only):
  current_filters <- eventReactive(
    input$apply_filters,
    {
      dr <- input$date_range
      list(
        departments = input$departments,
        employees   = input$employees,
        date_from   = if (!is.null(dr)) dr[[1L]] else as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
        date_to     = if (!is.null(dr)) dr[[2L]] else Sys.Date()
      )
    },
    ignoreNULL = FALSE
  )

  # Modules:
  shared_chart_metric <- reactiveVal(NULL)
  kpi_overviewServer("kpi_attendance", filters = current_filters)
  kpi_overviewServer("kpi_hours",      filters = current_filters)
  kpi_overviewServer("kpi_ontime",     filters = current_filters)
  kpi_overviewServer("kpi_pay",        filters = current_filters)
  kpi_overviewServer("kpi_overtime",   filters = current_filters)
  kpi_overviewServer("kpi_score",      filters = current_filters)
  attendance_proportionsServer("attendance_proportions", filters = current_filters)
  employees_scheduleServer("employees_schedule", filters = current_filters)
  distributionsServer("distributions", filters = current_filters)
  comparisonServer("comparisons", filters = current_filters, shared_metric = shared_chart_metric)
  trendsServer("trends", filters = current_filters, shared_metric = shared_chart_metric)
}
