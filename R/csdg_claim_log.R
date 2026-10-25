#' Log of the claims checked in a session
#'
#' @description
#' The package keeps an append-only log of the claims of the current R session: every claim derived with
#' [claim_relies_mainly()], [claim_top_k()], [claim_order()], or [claim_direction()], and every call of
#' [csdg_check()], [csdg_assess()], and [csdg_confirm()].
#' `csdg_claim_log()` returns the log, and `csdg_claim_log_reset()` empties it, for example at the start of an
#' analysis.
#'
#' The article asks researchers to report every claim they checked, including the claims that were not met or
#' remained unresolved, because a claim selected from many tried claims is more likely to be met by chance and
#' readers can judge this only if they know what was tried.
#' The log records this list as the analysis runs, so that it does not depend on memory; [csdg_export_assessment()]
#' writes it alongside an assessment (`claim_log.csv`).
#' The log holds no row-level data: the data are identified by the task id, their dimensions, and a hash.
#' It is kept in memory only and is lost when the R session ends or the package is reloaded.
#'
#' @return `csdg_claim_log()`: a [data.table::data.table()] with one row per entry and the columns `entry` (running
#'   number), `time` (UTC), `function_name`, `claim_id`, `kind` (`"relies_mainly"`, `"top_k"`, `"order"`, or
#'   `"direction"`), `statement` (the wording of the claim), `quantity` (`"pfi"`, `"loco"`, or `"ale"`), `learners`
#'   (labels of the learners the claim covers), `task_id`, `n_rows`, `n_features`, `data_hash` (SHA-256 hash of the
#'   task data, the hash that [csdg_fit()] stores), `origin` (`"exploratory"` or `"tested on new data"`), and `result`
#'   (empty for derived claims; the worst status per required gate for [csdg_check()]; the label of the assessment
#'   for [csdg_assess()] and [csdg_confirm()]).
#'   `csdg_claim_log_reset()`: the log before it was emptied, invisibly.
#' @seealso [csdg_export_assessment()]
#' @examples
#' csdg_claim_log_reset()
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' imp = csdg_importance(csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1),
#'   repetitions = 5, seed = 2)
#' claim = claim_order(imp, "a", "b")
#' res = csdg_assess(csdg_check(claim, imp))
#' csdg_claim_log()[, .(entry, function_name, statement, result)]
#' @export
csdg_claim_log = function() {
  entries = .csdg_claim_log_state$entries
  if (!length(entries)) return(.csdg_claim_log_template())
  rbindlist(entries, use.names = TRUE)
}

#' @rdname csdg_claim_log
#' @export
csdg_claim_log_reset = function() {
  old = csdg_claim_log()
  .csdg_claim_log_state$entries = list()
  invisible(old)
}

.csdg_claim_log_state = new.env(parent = emptyenv())
.csdg_claim_log_state$entries = list()

.csdg_claim_log_template = function() {
  data.table(entry = integer(), time = .POSIXct(numeric(), tz = "UTC"), function_name = character(),
    claim_id = character(), kind = character(), statement = character(), quantity = character(),
    learners = character(), task_id = character(), n_rows = integer(), n_features = integer(),
    data_hash = character(), origin = character(), result = character())
}

# Data description of a log entry from a CSDGFits or a CSDGCheck object.
.csdg_log_data = function(x) {
  if (inherits(x, "CSDGCheck")) {
    return(list(task_id = x$source$task_id, n_rows = x$source$n, n_features = length(x$claim$source$features),
      data_hash = x$source$data_hash))
  }
  list(task_id = x$task_id, n_rows = x$n, n_features = length(x$features), data_hash = x$data_hash)
}

# Appends one entry to the claim log; `x` is the CSDGFits or CSDGCheck object that holds the data description.
.csdg_log_claim = function(function_name, claim, x, result = NA_character_) {
  data = .csdg_log_data(x)
  entries = .csdg_claim_log_state$entries
  quantity = if (identical(claim$kind, "direction")) "ale" else claim$source$quantity %||% "pfi"
  entry = data.table(
    entry = length(entries) + 1L,
    time = .POSIXct(as.numeric(Sys.time()), tz = "UTC"),
    function_name = function_name,
    claim_id = claim$id %||% NA_character_,
    kind = claim$kind %||% NA_character_,
    statement = claim$statement %||% NA_character_,
    quantity = quantity,
    learners = paste(unname(claim$labels_learner[claim$covered]) %||% claim$covered, collapse = "; "),
    task_id = data$task_id %||% NA_character_,
    n_rows = as.integer(data$n_rows %||% NA_integer_),
    n_features = as.integer(data$n_features %||% NA_integer_),
    data_hash = data$data_hash %||% NA_character_,
    origin = if (identical(claim$origin, "independently_confirmed")) "tested on new data" else "exploratory",
    result = as.character(result)
  )
  .csdg_claim_log_state$entries = c(entries, list(entry))
  invisible(entry)
}
