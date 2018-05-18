# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# for now this is basically just position_dodge.r; once i figure out how it
# works i can start modifying.
# because the forces in d3 are iterative (ie. they continually advance one
# step at a time, since they're used in interactive contexts), my solution will
# have to decide when the positions have converged. that could be interesting.

hello <- function() {
  print("Hello, world!")
}

position_clump <- function(seed = NA) {

  # assign a random seed if one isn't given
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionClump,
          # width = width,
          # height = height,
          seed = seed
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionClump <- ggproto(
  "PositionClump", Position, required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(
      width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
      height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
      seed = self$seed
    )
  },

  compute_layer = function(data, params, panel) {
    trans_x <- if (params$width > 0) function(x) jitter(x, amount = params$width)
    trans_y <- if (params$height > 0) function(x) jitter(x, amount = params$height)

    with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
  }
)
