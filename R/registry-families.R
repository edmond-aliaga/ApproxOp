# Internal registry of operator families

.operator_registry <- new.env(parent = emptyenv())


register_family <- function(
    name,
    domain,
    variants = "discrete",
    subfamilies = NULL,
    metadata = list()
) {

  if (!is.character(name) ||
      length(name) != 1L ||
      !nzchar(name)) {
    stop(
      "`name` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  name <- tolower(name)

  if (!is.numeric(domain) ||
      length(domain) != 2L ||
      anyNA(domain) ||
      domain[1] >= domain[2]) {
    stop(
      "`domain` must be a numeric vector c(lower, upper).",
      call. = FALSE
    )
  }

  if (!is.character(variants) ||
      length(variants) < 1L) {
    stop(
      "`variants` must be a non-empty character vector.",
      call. = FALSE
    )
  }

  variants <- tolower(variants)

  if (!is.null(subfamilies)) {

    if (!is.character(subfamilies) ||
        length(subfamilies) < 1L ||
        anyNA(subfamilies) ||
        any(!nzchar(subfamilies))) {
      stop(
        paste0(
          "`subfamilies` must be NULL or a ",
          "non-empty character vector."
        ),
        call. = FALSE
      )
    }

    subfamilies <- tolower(subfamilies)
  }

  if (!is.list(metadata)) {
    stop(
      "`metadata` must be a list.",
      call. = FALSE
    )
  }

  assign(
    name,
    list(
      name = name,
      domain = domain,
      variants = variants,
      subfamilies = subfamilies,
      metadata = metadata
    ),
    envir = .operator_registry
  )

  invisible(TRUE)
}


get_family_spec <- function(name) {

  if (!is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)) {
    stop(
      "`name` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  name <- tolower(name)

  if (!exists(
    name,
    envir = .operator_registry,
    inherits = FALSE
  )) {
    stop(
      paste0(
        "Unsupported operator family: '",
        name,
        "'."
      ),
      call. = FALSE
    )
  }

  get(
    name,
    envir = .operator_registry,
    inherits = FALSE
  )
}


# Bernstein family
register_family(
  name = "bernstein",
  domain = c(0, 1),
  variants = "discrete",
  subfamilies = NULL,
  metadata = list(
    display_name = "Bernstein"
  )
)


# Szasz-Mirakyan family
register_family(
  name = "szasz",
  domain = c(0, Inf),
  variants = "discrete",
  subfamilies = NULL,
  metadata = list(
    display_name = "Szasz-Mirakyan"
  )
)


# Baskakov family
register_family(
  name = "baskakov",
  domain = c(0, Inf),
  variants = "discrete",
  subfamilies = NULL,
  metadata = list(
    display_name = "Baskakov"
  )
)


# Sheffer-based Szasz-type family
register_family(
  name = "sheffer",
  domain = c(0, Inf),
  variants = "discrete",
  subfamilies = c(
    "appell",
    "charlier",
    "meixner"
  ),
  metadata = list(
    display_name = "Sheffer-based Szasz-type"
  )
)
