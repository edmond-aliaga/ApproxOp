# Internal constructor for approximation operators
#
# This function creates the basic S3 object used throughout ApproxOp.
# It is intended for internal package use.

new_approx_operator <- function(
    family,
    subfamily = NULL,
    n,
    variant = "discrete",
    domain = NULL,
    parameters = list(),
    index = NULL,
    generator = NULL,
    weight = NULL,
    functional = NULL,
    transform = NULL,
    evaluation_engine = NULL,
    moment_engine = NULL,
    control = list(),
    metadata = list()
) {
  structure(
    list(
      family = family,
      subfamily = subfamily,
      n = n,
      variant = variant,
      domain = domain,
      parameters = parameters,
      index = index,
      generator = generator,
      weight = weight,
      functional = functional,
      transform = transform,
      evaluation_engine = evaluation_engine,
      moment_engine = moment_engine,
      control = control,
      metadata = metadata
    ),
    class = "approx_operator"
  )
}
