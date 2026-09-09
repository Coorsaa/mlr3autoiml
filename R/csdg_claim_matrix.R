.csdg_claim_matrix_statuses = c("met", "not_met", "unresolved", "not_applicable")

.csdg_claim_matrix_status_labels = c(
  met = "Met",
  not_met = "Not met",
  unresolved = "Unresolved",
  not_applicable = "Not applicable"
)

.csdg_claim_matrix_palette = function(style = .autoiml_plot_styles) {
  palette = .autoiml_plot_palette(style)
  c(
    met = palette$status[["pass"]],
    not_met = palette$status[["fail"]],
    unresolved = palette$status[["warn"]],
    not_applicable = palette$status[["skip"]]
  )
}

.csdg_wrap_claim_text = function(x, width) {
  vapply(
    x,
    function(value) paste(strwrap(value, width = width), collapse = "\n"),
    character(1L)
  )
}

#' Build a claim-by-evidence matrix
#'
#' @description
#' Creates a validated, explicitly scoped claim matrix for reporting which conclusions the available evidence permits.
#' The function records user-supplied judgments and does not infer claim support,
#' combine gates into a score, or rank claims.
#'
#' @param claim Character vector containing the claims under review.
#' @param evidence_needed Character vector describing the evidence required for each claim.
#' @param conclusion Character vector containing the conclusion permitted by the available evidence for each claim.
#' @param status Character vector containing one status per claim.
#'   Supported values are `"met"`, `"not_met"`, `"unresolved"`, and `"not_applicable"`.
#' @param claim_id Optional unique character identifiers.
#'   Deterministic identifiers are generated when this is `NULL`.
#' @param claim_version A scalar character string or one string per claim, such as `"C0"` or `"C1"`.
#' @param parent_claim_id A scalar character string, one string per claim, or `NA`,
#'   identifying the stronger parent claim.
#' @param revision_relation A scalar character string or one string per claim,
#'   describing the relation to the parent claim.
#' @param derivation_scope A scalar character string, or one string per claim, stating how the judgments were derived.
#'
#' @return A `CSDGClaimMatrix` data table with one ordered row per claim and no aggregate score.
#' @examples
#' claims = csdg_claim_matrix(
#'   claim = c(
#'     "Performance is adequate in the analytic sample",
#'     "The model transports to a new setting"
#'   ),
#'   evidence_needed = c("Repeated out-of-fold evaluation", "Prospective external validation"),
#'   conclusion = c("Supported only for the analytic sample", "Not evaluated"),
#'   status = c("met", "not_applicable")
#' )
#' claims
#' @export
csdg_claim_matrix = function(
    claim,
    evidence_needed,
    conclusion,
    status,
    claim_id = NULL,
    claim_version = "C0",
    parent_claim_id = NA_character_,
    revision_relation = "original",
    derivation_scope = "User-supplied claim boundary; not an independently executed gate plan") {
  assert_character(claim, any.missing = FALSE, min.len = 1L, min.chars = 1L, .var.name = "claim")
  assert_character(
    evidence_needed,
    any.missing = FALSE,
    min.len = 1L,
    min.chars = 1L,
    .var.name = "evidence_needed"
  )
  assert_character(
    conclusion,
    any.missing = FALSE,
    min.len = 1L,
    min.chars = 1L,
    .var.name = "conclusion"
  )
  assert_character(status, any.missing = FALSE, min.len = 1L, min.chars = 1L, .var.name = "status")
  assert_subset(status, .csdg_claim_matrix_statuses, empty.ok = FALSE, .var.name = "status")

  n_claims = length(claim)
  lengths = c(
    claim = n_claims,
    evidence_needed = length(evidence_needed),
    conclusion = length(conclusion),
    status = length(status)
  )
  if (any(lengths != n_claims)) {
    .csdg_stop(
      "`claim`, `evidence_needed`, `conclusion`, and `status` must have equal lengths; got %s.",
      paste(sprintf("%s=%d", names(lengths), lengths), collapse = ", ")
    )
  }

  claim_id = claim_id %||% sprintf("claim_%03d", seq_len(n_claims))
  assert_character(
    claim_id,
    any.missing = FALSE,
    len = n_claims,
    min.chars = 1L,
    unique = TRUE,
    .var.name = "claim_id"
  )

  status_label = unname(.csdg_claim_matrix_status_labels[status])

  claim_version = rep_len(claim_version, n_claims)
  parent_claim_id = rep_len(parent_claim_id, n_claims)
  revision_relation = rep_len(revision_relation, n_claims)
  assert_character(
    claim_version,
    any.missing = FALSE,
    len = n_claims,
    min.chars = 1L,
    .var.name = "claim_version"
  )
  assert_character(
    parent_claim_id,
    any.missing = TRUE,
    len = n_claims,
    min.chars = 1L,
    .var.name = "parent_claim_id"
  )
  assert_character(
    revision_relation,
    any.missing = FALSE,
    len = n_claims,
    min.chars = 1L,
    .var.name = "revision_relation"
  )
  assert_subset(
    revision_relation,
    c("original", .csdg_claim_relations),
    empty.ok = FALSE,
    .var.name = "revision_relation"
  )

  assert_character(
    derivation_scope,
    any.missing = FALSE,
    min.len = 1L,
    min.chars = 1L,
    .var.name = "derivation_scope"
  )
  if (length(derivation_scope) == 1L) {
    derivation_scope = rep(derivation_scope, n_claims)
  } else if (length(derivation_scope) != n_claims) {
    .csdg_stop("`derivation_scope` must have length 1 or %d; got %d.", n_claims, length(derivation_scope))
  }

  result = data.table(
    claim_order = seq_len(n_claims),
    claim_id = claim_id,
    claim_version = claim_version,
    parent_claim_id = parent_claim_id,
    revision_relation = revision_relation,
    status = status,
    status_label = status_label,
    claim = claim,
    evidence_needed = evidence_needed,
    conclusion = conclusion,
    derivation_scope = derivation_scope
  )
  setattr(result, "class", c("CSDGClaimMatrix", class(result)))
  result
}

#' Plot a claim-by-evidence matrix
#'
#' @description
#' Draws an accessible table-like plot with explicit status text and the selected package visual style.
#' Colors reinforce, but do not replace, the status labels.
#'
#' @param x A `CSDGClaimMatrix` returned by [csdg_claim_matrix()].
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param claim_width Approximate character width used to wrap the claim column.
#' @param evidence_width Approximate character width used to wrap the evidence column.
#' @param conclusion_width Approximate character width used to wrap the conclusion column.
#' @param base_size Base text size in points.
#' @param style Either `"color"` for the default blue-red rendering or `"monochrome"` for an achromatic
#'   print-oriented rendering.
#'
#' @return A `ggplot` object.
#' @examples
#' claims = csdg_claim_matrix(
#'   claim = "The explanation is stable across accepted models",
#'   evidence_needed = "Full-rank comparison across the prespecified near-equivalent set",
#'   conclusion = "The original claim remains unresolved",
#'   status = "unresolved"
#' )
#' csdg_plot_claim_matrix(claims)
#' @export
csdg_plot_claim_matrix = function(
    x,
    title = "Claim-by-evidence matrix",
    subtitle = paste(
      "User-supplied claim judgments; not inferred by the package.",
      "Statuses are categorical and are not combined into an overall score."
    ),
    claim_width = 34L,
    evidence_width = 22L,
    conclusion_width = 48L,
    base_size = 11,
    style = c("color", "monochrome")) {
  assert_class(x, "CSDGClaimMatrix", .var.name = "x")
  assert_string(title, min.chars = 1L, .var.name = "title")
  assert_string(subtitle, min.chars = 1L, .var.name = "subtitle")
  assert_int(claim_width, lower = 10L, .var.name = "claim_width")
  assert_int(evidence_width, lower = 10L, .var.name = "evidence_width")
  assert_int(conclusion_width, lower = 10L, .var.name = "conclusion_width")
  assert_number(base_size, lower = 6, finite = TRUE, .var.name = "base_size")
  style = match.arg(style)
  assert_choice(style, .autoiml_plot_styles, .var.name = "style")

  required_columns = c(
    "claim_order", "claim_id", "claim_version", "parent_claim_id", "revision_relation", "status",
    "status_label", "claim", "evidence_needed", "conclusion", "derivation_scope"
  )
  missing_columns = setdiff(required_columns, names(x))
  if (length(missing_columns)) {
    .csdg_stop("`x` is missing required columns: %s.", paste(missing_columns, collapse = ", "))
  }
  if (!nrow(x)) {
    .csdg_stop("`x` must contain at least one claim.")
  }
  assert_integerish(
    x$claim_order,
    lower = 1L,
    any.missing = FALSE,
    unique = TRUE,
    .var.name = "x$claim_order"
  )
  assert_character(x$claim_id, any.missing = FALSE, min.chars = 1L, unique = TRUE, .var.name = "x$claim_id")
  assert_character(x$claim, any.missing = FALSE, min.chars = 1L, .var.name = "x$claim")
  assert_character(x$evidence_needed, any.missing = FALSE, min.chars = 1L, .var.name = "x$evidence_needed")
  assert_character(x$conclusion, any.missing = FALSE, min.chars = 1L, .var.name = "x$conclusion")
  assert_character(x$derivation_scope, any.missing = FALSE, min.chars = 1L, .var.name = "x$derivation_scope")
  assert_subset(x$status, .csdg_claim_matrix_statuses, empty.ok = FALSE, .var.name = "x$status")

  source = copy(as.data.table(x))
  setorder(source, claim_order)
  source[, status_label := unname(.csdg_claim_matrix_status_labels[status])]
  n_claims = nrow(source)
  source[, row_y := rev(seq_len(.N))]
  source[, row_fill := rep(c("row_white", "row_gray"), length.out = .N)]
  source[, status_plot_label := .csdg_wrap_claim_text(status_label, 12L)]
  source[, claim_plot := .csdg_wrap_claim_text(claim, as.integer(claim_width))]
  source[, evidence_plot := .csdg_wrap_claim_text(
    gsub("/", "/ ", evidence_needed, fixed = TRUE),
    as.integer(evidence_width)
  )]
  source[, conclusion_plot := .csdg_wrap_claim_text(conclusion, as.integer(conclusion_width))]

  monochrome = identical(style, "monochrome")
  status_palette = .csdg_claim_matrix_palette(style)
  fill_palette = c(status_palette, row_white = "#FFFFFF", row_gray = "#F7F7F7")
  status_text = if (monochrome) {
    c(
      met = "#1A1A1A",
      not_met = "#FFFFFF",
      unresolved = "#1A1A1A",
      not_applicable = "#1A1A1A"
    )
  } else {
    c(
      met = "#FFFFFF",
      not_met = "#FFFFFF",
      unresolved = "#1A1A1A",
      not_applicable = "#1A1A1A"
    )
  }
  tile_border = if (monochrome) "grey55" else "#FFFFFF"
  header_y = n_claims + 0.82

  ggplot(source) +
    geom_tile(
      aes(x = 0.5, y = row_y, fill = row_fill),
      width = 1,
      height = 0.96
    ) +
    geom_tile(
      aes(x = 0.07, y = row_y, fill = status),
      width = 0.12,
      height = 0.66,
      color = tile_border,
      linewidth = 0.5
    ) +
    geom_text(
      aes(x = 0.07, y = row_y, label = status_plot_label, color = status),
      size = base_size * 3.25 / 12,
      fontface = "bold",
      lineheight = 0.95
    ) +
    geom_text(
      aes(x = 0.15, y = row_y, label = claim_plot),
      hjust = 0,
      size = base_size * 3.15 / 12,
      lineheight = 1.02
    ) +
    geom_text(
      aes(x = 0.46, y = row_y, label = evidence_plot),
      hjust = 0,
      size = base_size * 3.05 / 12,
      lineheight = 1.02
    ) +
    geom_text(
      aes(x = 0.66, y = row_y, label = conclusion_plot),
      hjust = 0,
      size = base_size * 3.05 / 12,
      lineheight = 1.02
    ) +
    annotate(
      "text",
      x = c(0.07, 0.15, 0.46, 0.66),
      y = header_y,
      label = c("Status", "Claim", "Evidence needed", "Permissible conclusion"),
      hjust = c(0.5, 0, 0, 0),
      fontface = "bold",
      size = base_size * 3.5 / 12
    ) +
    scale_fill_manual(values = fill_palette, drop = FALSE) +
    scale_color_manual(values = status_text, drop = FALSE) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0.45, n_claims + 1.08), expand = FALSE, clip = "off") +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = base_size, base_family = .autoiml_plot_font_family()) +
    theme(
      plot.title = element_text(
        face = "plain",
        size = base_size * 14 / 12,
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        color = "grey35",
        size = base_size * 10.5 / 12,
        hjust = 0,
        margin = margin(b = 12)
      ),
      plot.margin = margin(12, 14, 10, 14),
      legend.position = "none"
    )
}
