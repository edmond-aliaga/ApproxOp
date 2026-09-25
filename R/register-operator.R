#' Register a Custom Approximation Operator
#'
#' Registers a user-defined approximation operator so that it can be
#' created with \code{approx_operator()} and used with the computational
#' tools provided by ApproxOp.
#'
#' A registered operator must provide an evaluation engine. A moment
#' engine and a parameter validator are optional. When no moment engine
#' is supplied, moments are computed through the general
#' \code{approximate()} mechanism.
#'
#' Registration is atomic: if registration fails, the operator and family
#' registries are restored to their previous state.
#'
#' @param family Character string specifying the operator family.
#' @param domain Numeric vector of length two giving the operator domain.
#' @param evaluation_engine Function used to evaluate the operator.
#'   It must accept the arguments \code{f}, \code{x}, and
#'   \code{operator}, and return a numeric vector of length
#'   \code{length(x)}.
#' @param subfamily Optional character string specifying the operator
#'   subfamily. The default is \code{NULL}.
#' @param variant Character string specifying the operator variant.
#'   The default is \code{"discrete"}.
#' @param moment_engine Optional function for computing ordinary moments.
#'   It must accept the arguments \code{operator}, \code{order}, and
#'   \code{x}. The default is \code{NULL}.
#' @param parameter_validator Optional function used to validate the
#'   parameter list supplied through \code{approx_operator()}.
#'   The function must accept one argument containing the parameter list.
#' @param display_name Optional character string used as a descriptive
#'   name for the operator.
#' @param metadata Optional list containing additional operator metadata.
#' @param overwrite Logical. If \code{TRUE}, an existing registration
#'   with the same family, subfamily, and variant may be replaced.
#'
#' @return Invisibly returns \code{TRUE}.
#'
#' @export
register_operator <- function(
    family,
    domain,
    evaluation_engine,
    subfamily = NULL,
    variant = "discrete",
    moment_engine = NULL,
    parameter_validator = NULL,
    display_name = NULL,
    metadata = list(),
    overwrite = FALSE
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

  if (!is.character(variant) ||
      length(variant) != 1L ||
      is.na(variant) ||
      !nzchar(variant)) {
    stop(
      "`variant` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.numeric(domain) ||
      length(domain) != 2L ||
      anyNA(domain) ||
      domain[1] >= domain[2]) {
    stop(
      "`domain` must be a numeric vector c(lower, upper).",
      call. = FALSE
    )
  }

  if (!is.function(evaluation_engine)) {
    stop(
      "`evaluation_engine` must be a function.",
      call. = FALSE
    )
  }

  if (!is.null(moment_engine) &&
      !is.function(moment_engine)) {
    stop(
      "`moment_engine` must be NULL or a function.",
      call. = FALSE
    )
  }

  if (!is.null(parameter_validator) &&
      !is.function(parameter_validator)) {
    stop(
      "`parameter_validator` must be NULL or a function.",
      call. = FALSE
    )
  }

  if (!is.null(display_name)) {
    if (!is.character(display_name) ||
        length(display_name) != 1L ||
        is.na(display_name) ||
        !nzchar(display_name)) {
      stop(
        paste0(
          "`display_name` must be NULL or a single ",
          "non-empty character string."
        ),
        call. = FALSE
      )
    }
  }

  if (!is.list(metadata)) {
    stop(
      "`metadata` must be a list.",
      call. = FALSE
    )
  }

  if (!is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)) {
    stop(
      "`overwrite` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  family <- tolower(family)
  variant <- tolower(variant)

  if (!is.null(subfamily)) {
    subfamily <- tolower(subfamily)
  }

  key <- operator_registry_key(
    family = family,
    subfamily = subfamily,
    variant = variant
  )

  family_existed <- exists(
    family,
    envir = .operator_registry,
    inherits = FALSE
  )

  operator_existed <- exists(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  )

  old_family_spec <- NULL
  old_operator_spec <- NULL

  if (family_existed) {
    old_family_spec <- get(
      family,
      envir = .operator_registry,
      inherits = FALSE
    )
  }

  if (operator_existed) {
    old_operator_spec <- get(
      key,
      envir = .operator_implementation_registry,
      inherits = FALSE
    )
  }

  registration_complete <- FALSE

  on.exit(
    {
      if (!registration_complete) {

        if (family_existed) {

          assign(
            family,
            old_family_spec,
            envir = .operator_registry
          )

        } else if (exists(
          family,
          envir = .operator_registry,
          inherits = FALSE
        )) {

          rm(
            list = family,
            envir = .operator_registry
          )
        }

        if (operator_existed) {

          assign(
            key,
            old_operator_spec,
            envir = .operator_implementation_registry
          )

        } else if (exists(
          key,
          envir = .operator_implementation_registry,
          inherits = FALSE
        )) {

          rm(
            list = key,
            envir = .operator_implementation_registry
          )
        }
      }
    },
    add = TRUE
  )

  if (!family_existed) {

    family_metadata <- metadata

    if (!is.null(display_name)) {
      family_metadata$display_name <- display_name
    }

    register_family(
      name = family,
      domain = domain,
      variants = variant,
      subfamilies = subfamily,
      metadata = family_metadata
    )

  } else {

    family_spec <- get_family_spec(
      family
    )

    if (!isTRUE(all.equal(
      family_spec$domain,
      domain
    ))) {
      stop(
        paste0(
          "The supplied `domain` does not match the registered ",
          "domain for family '",
          family,
          "'."
        ),
        call. = FALSE
      )
    }

    variants <- unique(
      c(
        family_spec$variants,
        variant
      )
    )

    subfamilies <- family_spec$subfamilies

    if (!is.null(subfamily)) {
      subfamilies <- unique(
        c(
          subfamilies,
          subfamily
        )
      )
    }

    family_metadata <- family_spec$metadata

    if (!is.null(display_name)) {
      family_metadata$display_name <- display_name
    }

    if (length(metadata) > 0L) {
      family_metadata <- utils::modifyList(
        family_metadata,
        metadata
      )
    }

    register_family(
      name = family,
      domain = family_spec$domain,
      variants = variants,
      subfamilies = subfamilies,
      metadata = family_metadata
    )
  }

  operator_metadata <- metadata

  if (!is.null(display_name)) {
    operator_metadata$display_name <- display_name
  }

  register_operator_spec(
    family = family,
    subfamily = subfamily,
    variant = variant,
    evaluation_engine = evaluation_engine,
    moment_engine = moment_engine,
    parameter_validator = parameter_validator,
    metadata = operator_metadata,
    overwrite = overwrite
  )

  registration_complete <- TRUE

  invisible(TRUE)
}
