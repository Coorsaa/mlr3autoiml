#' Verify declared access to evidence sources
#'
#' Checks public files using exact path-component case even on case-insensitive filesystems.
#' Protected locators are not opened, and external resources are not fetched.
#' Dependencies and available aggregate substitutions form separate acyclic graphs: an aggregate may legitimately
#' depend on the protected source for which it is also the available public substitute.
#' File availability does not establish that the file supports a claim or reproduces protected analyses.
#'
#' @param registry Nonempty list of source entries.
#'   Each entry requires a unique `id`, `access` (`"public"`, `"protected"`, or `"external"`), and `rationale`.
#'   Public entries require a case-exact relative file `path`; protected entries require an opaque `path` or `uri`;
#'   external entries require a `uri`.
#'   Optional character vectors `dependencies` and `available_aggregate_ids` refer to other registered identifiers.
#'   Aggregate identifiers must refer to public entries.
#'   An optional lowercase SHA-256 `sha256` is compared with public-file contents only.
#' @param root Existing root directory for public relative paths.
#'
#' @return A data table with one row per source and access-specific `status`:
#'   `"verified_public"`, `"declared_protected"`, or `"declared_external"`.
#'   Paths, supplied URIs, public SHA-256 hashes, reference list columns, and rationales are retained.
#' @importFrom digest digest
#' @export
csdg_resolve_sources = function(registry, root) {
  assert_list(registry, min.len = 1L, .var.name = "registry")
  assert_string(root, min.chars = 1L, .var.name = "root")
  if (!dir.exists(root)) .csdg_stop("`root` must be an existing directory.")
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  entries = lapply(registry, function(entry) {
    .assert_named_list(entry, "source entry")
    for (field in c("id", "access", "rationale")) {
      assert_string(entry[[field]], min.chars = 1L, .var.name = paste0("source$", field))
    }
    assert_choice(entry$access, c("public", "protected", "external"), .var.name = "source$access")
    for (field in c("path", "uri", "sha256")) {
      assert_string(entry[[field]], min.chars = 1L, null.ok = TRUE, .var.name = paste0("source$", field))
    }
    if (entry$access == "public" && is.null(entry$path)) .csdg_stop("Public sources require `path`.")
    if (entry$access == "protected" && is.null(entry$path) && is.null(entry$uri)) {
      .csdg_stop("Protected sources require an opaque `path` or `uri` locator.")
    }
    if (entry$access == "external" && is.null(entry$uri)) .csdg_stop("External sources require `uri`.")
    if (!is.null(entry$sha256) && !grepl("^[0-9a-f]{64}$", entry$sha256)) {
      .csdg_stop("`sha256` must contain 64 lowercase hexadecimal characters.")
    }
    for (field in c("dependencies", "available_aggregate_ids")) {
      entry[[field]] = entry[[field]] %||% character()
      assert_character(entry[[field]], any.missing = FALSE, unique = TRUE, .var.name = paste0("source$", field))
      if (any(!nzchar(entry[[field]]))) .csdg_stop("Source references cannot contain empty identifiers.")
    }
    entry
  })
  ids = vapply(entries, `[[`, character(1L), "id")
  if (anyDuplicated(ids)) .csdg_stop("Source identifiers must be unique.")
  access = vapply(entries, `[[`, character(1L), "access")
  names(entries) = ids
  for (entry in entries) {
    references = c(entry$dependencies, entry$available_aggregate_ids)
    if (any(!references %in% ids)) {
      .csdg_stop("Source '%s' refers to unregistered identifiers: %s.", entry$id,
        paste(setdiff(references, ids), collapse = ", "))
    }
    if (any(access[match(entry$available_aggregate_ids, ids)] != "public")) {
      .csdg_stop("Available aggregates for '%s' must be public sources.", entry$id)
    }
  }
  for (field in c("dependencies", "available_aggregate_ids")) {
    .csdg_source_graph_acyclic(entries, field)
  }
  rbindlist(lapply(entries, function(entry) {
    hash = NA_character_
    if (entry$access == "public") {
      path = .csdg_source_public_path(root, entry$path)
      hash = digest(path, algo = "sha256", file = TRUE)
      if (!is.null(entry$sha256) && !identical(hash, entry$sha256)) {
        .csdg_stop("Public source '%s' has a SHA-256 mismatch.", entry$id)
      }
    }
    data.table(
      id = entry$id,
      access = entry$access,
      path = entry$path %||% NA_character_,
      uri = entry$uri %||% NA_character_,
      status = switch(entry$access,
        public = "verified_public", protected = "declared_protected", external = "declared_external"),
      sha256 = hash,
      dependencies = list(entry$dependencies),
      available_aggregate_ids = list(entry$available_aggregate_ids),
      rationale = entry$rationale
    )
  }), use.names = TRUE)
}

.csdg_source_public_path = function(root, path) {
  if (grepl("^(/|~|[A-Za-z]:)|\\\\", path) || grepl("//|/$", path)) {
    .csdg_stop("Public source paths must be relative, slash-separated file paths.")
  }
  components = strsplit(path, "/", fixed = TRUE)[[1L]]
  if (any(components %in% c(".", ".."))) .csdg_stop("Public source paths cannot contain '.' or '..' components.")
  current = root
  for (component in components) {
    children = list.files(current, all.files = TRUE, no.. = TRUE)
    if (!component %in% children) {
      .csdg_stop("Public source '%s' is missing or does not match case-sensitive path components.", path)
    }
    current = file.path(current, component)
    resolved = normalizePath(current, winslash = "/", mustWork = TRUE)
    if (!startsWith(resolved, paste0(sub("/$", "", root), "/"))) {
      .csdg_stop("Public source '%s' resolves outside the declared root.", path)
    }
  }
  if (isTRUE(file.info(current)$isdir)) .csdg_stop("Public source '%s' must identify a file, not a directory.", path)
  current
}

.csdg_source_graph_acyclic = function(entries, field) {
  visited = new.env(parent = emptyenv())
  visit = function(id, stack = character()) {
    if (id %in% stack) .csdg_stop("Source %s cycle: %s.", field, paste(c(stack, id), collapse = " -> "))
    if (exists(id, envir = visited, inherits = FALSE)) return(invisible(NULL))
    for (child in entries[[id]][[field]]) visit(child, c(stack, id))
    assign(id, TRUE, envir = visited)
    invisible(NULL)
  }
  for (id in names(entries)) visit(id)
  invisible(TRUE)
}
