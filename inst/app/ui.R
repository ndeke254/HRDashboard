# Dashboard UI:
ui <- bslib::page(
  title = app_title,
  theme = bslib::bs_theme(version = 5, preset = "bootstrap"),
  lang = "en",

  # dependencies:
  shinyjs::useShinyjs(),
  shinybusy::add_busy_bar(
    height = "3px",
    color = colors$primary
  ),
  shinybusy::add_loading_state(
    selector = c(".reactable", ".echarts4r"),
    svgColor = colors$primary
  ),
  shinybusy::busy_start_up(
    loader = shinybusy::spin_epic("orbit", color = colors$navy),
    text = tags$span(class = "busy-text", app_title),
    mode = "auto"
  ),
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = file.path("images", "favicon.ico")
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = file.path(".", "main.css")
    ),
    tags$script(src = file.path("js", "app.js"))
  ),
  tags$body(
    tags$h1(
      class = "main-header",
      tags$span(class = "main-title", app_title)
    ),
    tags$main(
      class = "dashboard",
      tags$header(
        class = "dashboard-header",
        tags$h2(
          class = "dashboard-heading",
          tags$span(class = "dashboard-title", app_title),
          tags$span(class = "dashboard-version", paste0("v", app_version))
        )
      ),
      tags$div(
        class = "filter-container",
        tags$div(
          class = "selectize-filters",
          selectizeInput(
            inputId = "departments",
            label = "Departments",
            choices = get_departments_choices(),
            multiple = TRUE,
            selected = "all",
            options = list(
              placeholder = "Select departments...",
              maxItems = 3,
              maxOptions = 5,
              plugins = list("remove_button", "drag_drop")
            )
          ),
          selectizeInput(
            inputId = "employees",
            label = "Employees",
            choices = NULL,
            multiple = TRUE,
            selected = "all",
            options = list(
              placeholder = "Select employees...",
              maxItems = 5,
              maxOptions = 5,
              plugins = list("remove_button", "drag_drop")
            )
          ),
          selectizeInput(
            inputId  = "locations",
            label    = "Offices",
            choices  = get_locations_choices(),
            multiple = TRUE,
            selected = "all",
            options  = list(
              placeholder = "Select offices...",
              maxItems    = 3L,
              maxOptions  = 8L,
              plugins     = list("remove_button")
            )
          )
        ),
        tags$div(
          class = "date-range-filter-row",
          dateRangeInput(
            inputId = "date_range",
            label = NULL,
            start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
            end = Sys.Date(),
            max = Sys.Date(),
            format = "d M yyyy",
            weekstart = 1L,
            separator = "\u2013"
          ),
          actionButton(
            inputId = "apply_filters",
            label = "Apply Filters",
            class = "btn-apply-filters"
          )
        )
      ),

      tags$div(
        class = "dashboard-body",

        # ── Tab navigation ──────────────────────────────────────────────────
        tags$nav(
          class = "dashboard-nav",
          tags$button(class = "nav-tab nav-tab--active", `data-tab` = "attendance",
                      tags$i(class = "fa-regular fa-calendar-check"), " Attendance"),
          tags$button(class = "nav-tab", `data-tab` = "workforce",
                      tags$i(class = "fa-solid fa-users"), " Workforce"),
          tags$button(class = "nav-tab", `data-tab` = "payroll",
                      tags$i(class = "fa-solid fa-money-bill-wave"), " Payroll")
        ),

        # ── Attendance tab (existing content) ───────────────────────────────
        tags$div(
          id    = "tab-attendance",
          class = "tab-panel tab-panel--active",

          # KPI row:
          tags$section(
            class = "dashboard-panels",

            tags$div(class = "kpi-section-heading", "KPIs Overview"),

            tags$div(
              id = "kpi_attendance",
              class = "panel panel-metric panel-metric-featured",
              kpi_overviewUI("kpi_attendance")
            ),
            tags$div(
              id = "kpi_pay",
              class = "panel panel-metric",
              kpi_overviewUI("kpi_pay")
            ),
            tags$div(
              id = "kpi_hours",
              class = "panel panel-metric",
              kpi_overviewUI("kpi_hours")
            ),
            tags$div(
              id = "kpi_ontime",
              class = "panel panel-metric",
              kpi_overviewUI("kpi_ontime")
            ),
            tags$div(
              id = "kpi_overtime",
              class = "panel panel-metric",
              kpi_overviewUI("kpi_overtime")
            ),
            tags$div(
              id = "kpi_score",
              class = "panel panel-metric",
              kpi_overviewUI("kpi_score")
            ),

            # comparisons:
            tags$div(
              id = "comparisons",
              class = "panel panel-chart chart-breakdown",
              comparisonUI("comparisons")
            ),

            # trends:
            tags$div(
              id = "trends",
              class = "panel panel-chart chart-time",
              trendsUI("trends")
            ),

            # distributions:
            tags$div(
              id = "distributions",
              class = "panel panel-chart chart-time",
              distributionsUI("distributions")
            ),

            # attendance proportions:
            tags$div(
              id = "attendance_proportions",
              class = "panel panel-chart chart-breakdown",
              attendance_proportionsUI("attendance_proportions")
            ),

            # employees schedule / live attendance:
            tags$details(
              id = "employees_schedule",
              class = "panel panel-chart-wide chart-time collapsible-panel",
              tags$summary(
                class = "collapsible-summary",
                tags$div(
                  class = "panel-header",
                  tags$span(class = "panel-title", "Weekly Schedule"),
                  tags$span(
                    class = "collapsible-summary-note",
                    "Click to expand or collapse"
                  )
                )
              ),
              tags$div(
                class = "collapsible-panel-body",
                employees_scheduleUI("employees_schedule")
              )
            )
          )
        ),

        # ── Workforce tab ────────────────────────────────────────────────────
        tags$div(
          id    = "tab-workforce",
          class = "tab-panel",
          workforceUI("workforce")
        ),

        # ── Payroll tab ──────────────────────────────────────────────────────
        tags$div(
          id    = "tab-payroll",
          class = "tab-panel",
          payslipUI("payslip")
        )
      )
    ),

    # footer:
    tags$footer(
      class = "footer",
      tags$h3(
        class = "footer-heading",
        tags$span(
          paste0(
            "\u00a9 ",
            format(Sys.Date(), "%Y"),
            " HR Analytics Dashboard. Demo Project."
          )
        )
      )
    )
  )
)
