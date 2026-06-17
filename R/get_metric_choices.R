#' Get metric choices for a dashboard category
#'
#' @param category String. One of the top-level keys in \code{metrics_list}
#'   defined in \code{global.R}: \code{"kpi_attendance"}, \code{"kpi_hours"},
#'   \code{"kpi_ontime"}, \code{"kpi_pay"}, \code{"kpi_overtime"},
#'   \code{"kpi_score"}, \code{"comparisons"}, \code{"trends"},
#'   \code{"distributions"}, \code{"attendance_proportions"},
#'   \code{"employees_schedule"}.
#'
#' @return Named list of metric specs, each with \code{id}, \code{label},
#'   and \code{suffix}.
#' @export
get_category_choices <- function(category) {
  category <- match.arg(
    arg     = category,
    choices = c(
      "kpi_attendance",
      "kpi_hours",
      "kpi_ontime",
      "kpi_pay",
      "kpi_overtime",
      "kpi_score",
      "comparisons",
      "trends",
      "distributions",
      "attendance_proportions",
      "employees_schedule"
    )
  )
  metrics_list[[category]]
}

#' Get metric parameters
#'
#' @param category String. Category name (see \code{get_category_choices}).
#' @param metric   String. Metric id within the category.
#'
#' @return Named list with \code{id}, \code{label}, \code{suffix}.
#' @export
get_metric_parameters <- function(category, metric) {
  choices <- get_category_choices(category)
  choices[[metric]]
}

