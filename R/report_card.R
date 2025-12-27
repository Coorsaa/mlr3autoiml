# FILE: R/report_card.R

#' @title Report card for an AutoIML run
#'
#' @description
#' Summarize gate outcomes in a compact, tabular report card (one row per gate).
#'
#' This is a convenience extractor around the `gate_results` stored in an
#' [AutoIMLResult] (or in an [AutoIML] object after `run()`).
#'
#' @param x AutoIML or AutoIMLResult
#' @return data.table report card
#' @export
report_card = function(x) {
  if (inherits(x, "AutoIML")) {
    if (!is.null(x$result)) {
      return(report_card(x$result))
    }
    stop("AutoIML has no result yet; call $run() first.", call. = FALSE)
  }

  if (inherits(x, "AutoIMLResult")) {
    gates = x$gate_results
    if (is.null(gates) || length(gates) == 0L) {
      return(data.table::data.table())
    }

    dt = data.table::rbindlist(lapply(gates, function(gr) {
      data.table::data.table(
        gate_id = gr$gate_id,
        gate_name = gr$gate_name,
        pdr = gr$pdr,
        status = gr$status,
        summary = gr$summary
      )
    }), fill = TRUE)

    dt[, irl := x$irl]
    dt[, purpose := x$purpose]
    return(dt[])
  }

  stop("Unsupported input to report_card().", call. = FALSE)
}
