#' Create an Approximation Operator
#'
#' Creates an approximation operator object from a supported operator family
#' and, when applicable, a supported subfamily.
#'
#' @param family Character string specifying the operator family.
#' @param subfamily Optional character string specifying the operator subfamily.
#'   The default is \code{NULL}. For families that provide a general
#'   formulation, \code{NULL} represents the general family.
#' @param n Positive integer specifying the approximation parameter.
#' @param variant Character string specifying the operator variant.
#' @param params A list containing family- or subfamily-specific parameters.
#' @param control A list containing numerical control parameters.
#'
#' @return An object of class \code{"approx_operator"}.
#'
#' @export
approx_operator <- function(
    family,
    n,
    subfamily = NULL,
    variant = "discrete",
    params = list(),
    control = list()
) {

  if (!is.character(family) ||
      length(family) != 1L ||
      is.na(family) ||
      !nzchar(family)) {
    stop(
      "`family` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.character(variant) ||
      length(variant) != 1L ||
      is.na(variant) ||
      !nzchar(variant)) {
    stop(
      "`variant` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.null(subfamily)) {
    if (!is.character(subfamily) ||
        length(subfamily) != 1L ||
        is.na(subfamily) ||
        !nzchar(subfamily)) {
      stop(
        paste0(
          "`subfamily` must be NULL or a single ",
          "non-empty character string."
        ),
        call. = FALSE
      )
    }
  }

  if (!is.list(params)) {
    stop(
      "`params` must be a list.",
      call. = FALSE
    )
  }

  if (!is.list(control)) {
    stop(
      "`control` must be a list.",
      call. = FALSE
    )
  }

  family <- tolower(family)
  variant <- tolower(variant)

  if (!is.null(subfamily)) {
    subfamily <- tolower(subfamily)
  }

  spec <- get_family_spec(family)

  # Validate subfamily
  if (is.null(spec$subfamilies)) {

    if (!is.null(subfamily)) {
      stop(
        paste0(
          "Family '", family,
          "' does not support subfamilies."
        ),
        call. = FALSE
      )
    }

  } else {

    if (!is.null(subfamily) &&
        !subfamily %in% spec$subfamilies) {
      stop(
        paste0(
          "Subfamily '", subfamily,
          "' is not supported for family '",
          family,
          "'."
        ),
        call. = FALSE
      )
    }
  }

  # Validate variant
  if (!variant %in% spec$variants) {
    stop(
      paste0(
        "Variant '", variant,
        "' is not supported for family '",
        family,
        "'."
      ),
      call. = FALSE
    )
  }

  # Look for a registered computational implementation.
  operator_spec <- get_operator_spec(
    family = family,
    subfamily = subfamily,
    variant = variant,
    error = FALSE
  )

  evaluation_engine <- NULL
  moment_engine <- NULL
  operator_metadata <- list()

  if (!is.null(operator_spec)) {

    evaluation_engine <-
      operator_spec$evaluation_engine

    moment_engine <-
      operator_spec$moment_engine

    operator_metadata <-
      operator_spec$metadata

    if (!is.null(
      operator_spec$parameter_validator
    )) {
      operator_spec$parameter_validator(
        params
      )
    }
  }

  metadata <- spec$metadata

  if (length(operator_metadata) > 0L) {
    metadata <- utils::modifyList(
      metadata,
      operator_metadata
    )
  }

  op <- new_approx_operator(
    family = family,
    subfamily = subfamily,
    n = n,
    variant = variant,
    domain = spec$domain,
    parameters = params,
    evaluation_engine = evaluation_engine,
    moment_engine = moment_engine,
    control = control,
    metadata = metadata
  )

  validate_approx_operator(op)

  op
}
