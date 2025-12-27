# FILE: R/plot_shap.R
#
# SHAP / Shapley plotting helpers (ggplot2).
#
# @keywords internal
NULL

#' @keywords internal
.autoiml_plot_shap_local = function(auto,
  row_id,
  class_label = NULL,
  sample_size = 100L,
  background_n = 200L,
  top_n = 15L,
  sort_by_abs = TRUE) {
  if (!inherits(auto, "AutoIML")) {
    stop("'auto' must be an AutoIML object.", call. = FALSE)
  }
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  class_label_param = class_label

  shap_dt = auto$shap(
    row_id = row_id,
    class_label = class_label_param,
    sample_size = sample_size,
    background_n = background_n,
    seed = auto$seed
  )

  if (data.table::nrow(shap_dt) == 0L) {
    stop("No SHAP values computed (empty result).", call. = FALSE)
  }

  dt = data.table::copy(shap_dt)

  if (!is.null(class_label_param)) {
    dt = dt[class_label == ..class_label_param]
  }

  if (isTRUE(sort_by_abs)) {
    dt[, abs_phi := abs(phi)]
    data.table::setorder(dt, -abs_phi)
  } else {
    data.table::setorder(dt, -phi)
  }

  top_n = as.integer(top_n)
  if (is.finite(top_n) && top_n > 0L) {
    dt = dt[seq_len(min(top_n, .N))]
  }

  # Keep original ordering in plot
  dt[, feature := factor(feature, levels = rev(unique(feature)))]

  ggplot2::ggplot(dt, ggplot2::aes(x = feature, y = phi)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("SHAP / Shapley (row_id = ", row_id, ")"),
      subtitle = if (!is.null(class_label_param)) paste0("class = ", class_label_param) else NULL,
      x = NULL,
      y = "Contribution (phi)"
    )
}
