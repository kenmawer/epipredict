#' Smooth AR forecaster with optional covariates
#'
#' @param x Covariates. Allowed to be missing (resulting in AR on `y`).
#' @param y Response.
#' @param key_vars Factor(s). A prediction will be made for each unique
#'   combination.
#' @param time_value the time value associated with each row of measurements.
#' @param args Additional arguments specifying the forecasting task. Created
#'   by calling `smooth_arx_args_list()`.
#'
#' @return A data frame of point (and optionally interval) forecasts across
#'   multiple aheads for each unique combination of `key_vars`.
#' @export
smooth_arx_forecaster <- function(x, y, key_vars, time_value,
                                  args = smooth_arx_args_list()) {
  assign_arg_list(args)
  if (is.null(key_vars)) {
    keys <- NULL
    distinct_keys <- tibble(.dump = NA)
  } else {
    keys <- tibble(key_vars)
    distinct_keys <- dplyr::distinct(keys)
  }

  if (length(y) < min_train_window + max_lags + max(ahead)) {
    qnames <- probs_to_string(levels)
    out <- purrr::map_dfr(ahead, ~ distinct_keys, .id = "ahead") %>%
      dplyr::mutate(ahead = magrittr::extract(!!ahead, as.integer(ahead)),
                    point = NA) %>%
      dplyr::select(!any_of(".dump"))
    return(enframer(out, qnames))
  }

  dat <- create_lags_and_leads(x, y, lags, ahead, time_value, keys)
  if (intercept) dat$x0 <- 1

  H <- cbind(1 / sqrt(length(ahead)),
             poly(ahead, degree = degree - 1, simple = TRUE))

  if (kronecker_version) return(kronecker_arx(dat, H, time_value, keys, distinct_keys, args))
  else return(smooth_arx(dat, H, time_value, keys, distinct_keys, args))
}


#' Smooth ARX forecaster argument constructor
#'
#' Constructs a list of arguments for [smooth_arx_forecaster()].
#'
#' @template param-lags
#' @template param-ahead
#' @param degree Integer. Order of the orthodonal polynomials to use for
#'   smoothing. Should be strictly less than `length(ahead)`.
#' @param kronecker_version Logical. Do we ensure that we've "seen" the latest
#'   `ahead` value. The default `FALSE` is computationally simpler but uses
#'   less recent data.
#' @template param-min_train_window
#' @template param-levels
#' @template param-intercept
#' @template param-symmetrize
#' @template param-nonneg
#' @param quantile_by_key Not currently implemented.
#'
#' @return A list containing updated parameter choices.
#' @export
#'
#' @examples
#' smooth_arx_args_list()
#' smooth_arx_args_list(symmetrize = FALSE)
#' smooth_arx_args_list(levels = c(.1, .3, .7, .9), min_train_window = 120)
smooth_arx_args_list <- function(
  lags = c(0, 7, 14), ahead = 1:28,
  degree = 4, kronecker_version = FALSE,
  min_train_window = 20,
  levels = c(0.05, 0.95), intercept = TRUE,
  symmetrize = TRUE,
  nonneg = TRUE,
  quantile_by_key = FALSE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(degree, min_train_window)
  arg_is_nonneg_int(degree, ahead, min_train_window, lags)
  arg_is_lgl(intercept, symmetrize, nonneg, kronecker_version)
  arg_is_probabilities(levels, allow_null=TRUE)

  max_lags <- max(lags)

  if (length(ahead) == 1)
    stop("Smoothing is immaterial for only a single ahead. You\n",
         "may want `arx_forecaster()` instead.")
  if (degree > length(ahead))
    stop("Smoothing requires requesting fewer degrees of freedom then ahead values.")

  enlist(
    lags = .lags, ahead = as.integer(ahead), degree = as.integer(degree),
    min_train_window, kronecker_version, levels, intercept, symmetrize, nonneg,
    max_lags)
}


smooth_arx <- function(dat, H, time_value, keys, distinct_keys, args) {

  assign_arg_list(args)

  # smooth and fit
  dat <- df_mat_mul(dat, H, "y", matches("^y\\d+"))
  ny <- grab_names(dat, matches("^y\\d+"))
  nx <- grab_names(dat, matches("^x\\d+"))
  form <- stats::as.formula(paste(
    "cbind(", paste(ny, collapse = ","), ") ~ ", # multivariate y
    paste(nx, collapse = "+"), "+ 0"))
  obj <- stats::lm(form, data = dat)

  point <- make_predictions(obj, dat, time_value, keys) %>%
    tcrossprod(H) %>%
    as.data.frame()

  r <- residuals(obj) %>%
    tcrossprod(H) %>%
    as.data.frame() %>%
    magrittr::set_names(ahead)

  q <- purrr::map2_dfr(
    r, point, ~ residual_quantiles(.x, .y, levels, symmetrize), .id = "ahead"
  ) %>% mutate(ahead = as.integer(ahead))

  if (nonneg) q <- dplyr::mutate(q, dplyr::across(!ahead, ~ pmax(.x, 0)))

  return(
    purrr::map_dfr(ahead, ~ distinct_keys) %>%
      dplyr::select(!any_of(".dump")) %>%
      dplyr::bind_cols(q) %>%
      dplyr::relocate(ahead)
  )
}

kronecker_arx <- function(dat, H, time_value, keys, distinct_keys, args) {

  assign_arg_list(args)
  time_value <- rep(time_value, length(ahead))

  # smooth and fit
  xmat <- dat %>%
    dplyr::select(matches("^x\\d+")) %>%
    as.matrix()
  dat <- dat %>%
    dplyr::select(! matches("^x\\d+")) %>%
    tidyr::pivot_longer(
      matches("^y\\d+"), names_to = "y_ahead", values_to = "all_y")
  xtilde <- kronecker(H, xmat)
  nx <- paste0("x", 1:ncol(xtilde))
  names(xtilde = nx)
  dat <- dplyr::bind_cols(dat, as.data.frame(xtilde))
  form <- stats::as.formula(
    paste("y_all ~ ", paste(nx, collapse = "+"), "+ 0"))
  obj <- stats::lm(form, data = dat)

  if (is.null(keys)) keys <- data.frame(y_ahead = dat$y_ahead)
  else {
    keys <- dplyr::bind_cols(
      y_ahead = dat$y_ahead,
      keys[rep(seq_len(nrow(keys)), length(ahead)), ])
  }

  point <- make_predictions(obj, dat, time_value, keys)
  r <- residuals(obj)
}
