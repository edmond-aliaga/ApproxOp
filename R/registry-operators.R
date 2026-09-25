# Internal registry of operator implementations
#
# This registry stores computational components associated with a
# particular combination of family, subfamily, and variant.
#
# It is separate from the family registry because a family specification
# describes the mathematical family, whereas an operator specification
# describes how a particular operator is evaluated and validated.

.operator_implementation_registry <- new.env(parent = emptyenv())


operator_registry_key <- function(
    family,
    subfamily = NULL,
    variant = "discrete"
) {

  family <- tolower(family)
  variant <- tolower(variant)

  if (is.null(subfamily)) {
    subfamily_key <- "<general>"
  } else {
    subfamily_key <- tolower(subfamily)
  }

  paste(
    family,
    subfamily_key,
    variant,
    sep = "::"
  )
}


register_operator_spec <- function(
    family,
    subfamily = NULL,
    variant = "discrete",
    evaluation_engine,
    moment_engine = NULL,
    parameter_validator = NULL,
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

  if (exists(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  ) && !overwrite) {
    stop(
      paste0(
        "An operator specification is already registered for '",
        key,
        "'."
      ),
      call. = FALSE
    )
  }

  assign(
    key,
    list(
      family = family,
      subfamily = subfamily,
      variant = variant,
      evaluation_engine = evaluation_engine,
      moment_engine = moment_engine,
      parameter_validator = parameter_validator,
      metadata = metadata
    ),
    envir = .operator_implementation_registry
  )

  invisible(TRUE)
}


get_operator_spec <- function(
    family,
    subfamily = NULL,
    variant = "discrete",
    error = TRUE
) {

  if (!is.logical(error) ||
      length(error) != 1L ||
      is.na(error)) {
    stop(
      "`error` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  key <- operator_registry_key(
    family = family,
    subfamily = subfamily,
    variant = variant
  )

  if (!exists(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  )) {

    if (!error) {
      return(NULL)
    }

    stop(
      paste0(
        "No operator specification is registered for '",
        key,
        "'."
      ),
      call. = FALSE
    )
  }

  get(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  )
}
