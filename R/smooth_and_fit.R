smooth_and_fit <- function(dat, H, kronecker_version) {
  if (kronecker_version) {
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
  } else {
    dat <- df_mat_mul(dat, H, "y", matches("^y\\d+"))
    ny <- grab_names(dat, matches("^y\\d+"))
    nx <- grab_names(dat, matches("^x\\d+"))
    form <- stats::as.formula(paste(
      "cbind(", paste(ny, collapse = ","), ") ~ ", # multivariate y
      paste(nx, collapse = "+"), "+ 0"))
  }
  obj <- stats::lm(form, data = dat)
  return(enlist(obj, dat))
}
