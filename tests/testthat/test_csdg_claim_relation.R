same_context = function() {
  setNames(rep("same", 6L), mlr3autoiml:::.csdg_scope_elements)
}

test_that("opposite PFI propositions do not become identical through matching contexts", {
  parent = csdg_claim(id = "pfi_a", statement = "PFI(A) exceeds PFI(B).", target = "PFI(A) versus PFI(B)")
  claim = csdg_claim(id = "pfi_b", statement = "PFI(A) is smaller than PFI(B).", target = parent$target)
  unchecked = csdg_claim_relation(parent, claim, same_context(), "The comparison context is unchanged.")
  declared = csdg_claim_relation(
    parent, claim, same_context(), "The comparison context is unchanged.",
    proposition_relation = "incomparable",
    proposition_rationale = "The strict inequalities have opposite directions under the same PFI definition."
  )

  expect_identical(unchecked$scope_relation, "same")
  expect_identical(unchecked$context_relation, "same")
  expect_identical(unchecked$scope$element, mlr3autoiml:::.csdg_scope_elements)
  expect_identical(unchecked$relation, "unchecked")
  expect_identical(unchecked$semantic_status, "unchecked")
  expect_identical(declared$relation, "incomparable")
  expect_identical(declared$semantic_status, "declared_not_verified")
  expect_identical(declared$parent_statement, parent$statement)
  expect_identical(declared$claim_statement, claim$statement)
  expect_error(csdg_claim_relation(parent, claim, same_context(), "Same context.", "same"),
    "proposition_rationale")
})

test_that("a subgroup mean is not a logical weakening of a population mean", {
  values = c(10, -1)
  expect_gt(mean(values), 0)
  expect_lt(mean(values[2L]), 0)
  parent = csdg_claim(id = "population", statement = "The population mean is positive.")
  claim = csdg_claim(id = "subgroup", statement = "The subgroup mean is positive.")
  context = same_context()
  context[["data"]] = "narrower"
  relation = csdg_claim_relation(
    parent, claim, context, "The subgroup is a subset of the population.",
    proposition_relation = "incomparable",
    proposition_rationale = "A positive marginal mean does not imply a positive subgroup mean.",
    revision_kind = "restriction_without_entailment"
  )

  expect_identical(relation$scope_relation, "narrower")
  expect_identical(relation$relation, "incomparable")
})

test_that("a universal property restricts to a declared subset", {
  domain = 1:5
  subset = domain[2:3]
  expect_true(all(domain > 0) && all(subset > 0))
  parent = csdg_claim(id = "all", statement = "For every x in D, x is positive.")
  claim = csdg_claim(id = "subset", statement = "For every x in D_sub, x is positive.")
  context = same_context()
  context[["data"]] = "narrower"
  relation = csdg_claim_relation(
    parent, claim, context, "D_sub is a subset of D.",
    proposition_relation = "logical_weakening",
    proposition_rationale = "Under the declared subset inclusion, the universal parent entails the restriction.",
    revision_kind = "logical_weakening"
  )

  expect_identical(relation$relation, "narrower")
  expect_identical(relation$proposition_relation, "logical_weakening")
  expect_identical(relation$semantic_status, "declared_not_verified")
})

test_that("local and global explanation targets remain alternative questions", {
  parent = csdg_claim(id = "local", statement = "A local surrogate approximates this prediction.")
  claim = csdg_claim(id = "global", statement = "Marginal PFI increases held-out prediction loss.")
  context = same_context()
  context[c("quantity", "procedure")] = "incomparable"
  relation = csdg_claim_relation(
    parent, claim, context, "The local quantity and the global loss contrast differ.",
    proposition_relation = "incomparable",
    proposition_rationale = "Global reliance does not entail fidelity for this local surrogate.",
    revision_kind = "change_of_question"
  )

  expect_identical(relation$relation, "incomparable")
  expect_identical(relation$scope_relation, "incomparable")
  expect_error(csdg_claim_relation(parent, claim, context, "Different quantities.",
    proposition_relation = "same", proposition_rationale = "A declaration alone is not sufficient.",
    revision_kind = "change_of_question"), "different question")
})

test_that("claim lineage preserves provenance without retrospectively inheriting confirmation", {
  provenance = list(
    origin = "specified_before_results", date = "2026-01-10", time_basis = "Dated study protocol.",
    selection_basis = "Target fixed before result inspection.", evidence_ids = character()
  )
  parent = csdg_claim(id = "original", statement = "A planned proposition.", provenance = provenance,
    confirmatory = TRUE)
  revised = csdg_claim_revision(parent, "revised", "An exploratory proposition.", "S1", "narrower")

  expect_identical(parent$provenance, provenance)
  expect_null(revised$provenance)
  expect_false(revised$confirmatory)
  expect_identical(revised$parent_claim_id, parent$id)
  expect_error(csdg_claim_revision(parent, parent$id, "Changed meaning.", "S1", "narrower"), "new `id`")
  provenance$origin = "retrospective_exploratory"
  provenance$date = "2026-09-10"
  provenance$selection_basis = "Motivated by previously inspected evidence E1."
  provenance$evidence_ids = "E1"
  revised = csdg_claim_revision(parent, "revised_s1", "An exploratory proposition.", "S1", "narrower",
    provenance = provenance)
  expect_identical(revised$provenance, provenance)
  provenance$origin = "independently_confirmed"
  provenance$evidence_ids = character()
  expect_error(csdg_claim(provenance = provenance), "evidence_ids")
  provenance$evidence_ids = "independent_E2"
  provenance$date = "2026-02-30"
  expect_error(csdg_claim(provenance = provenance), "valid YYYY-MM-DD")
})

test_that("legacy element names, relation values, and revision kinds are mapped with warnings", {
  parent = csdg_claim(id = "legacy_parent", statement = "The parent claim.")
  claim = csdg_claim(id = "legacy_claim", statement = "The revised claim.")
  legacy = setNames(rep("same", 6L), mlr3autoiml:::.csdg_claim_coordinates)
  legacy[["target"]] = "alternative_or_incomparable"
  mlr3autoiml:::.csdg_reset_deprecations()
  relation = suppressWarnings(csdg_claim_relation(parent, claim, legacy, "Legacy declaration.",
    proposition_relation = "alternative_or_incomparable", proposition_rationale = "Different question.",
    revision_kind = "estimand_change"))
  expect_identical(relation$scope[element == "quantity", relation], "incomparable")
  expect_identical(relation$revision_kind, "change_of_question")
  expect_identical(relation$proposition_relation, "incomparable")
  expect_identical(relation$coordinates$coordinate, mlr3autoiml:::.csdg_claim_coordinates)
  mlr3autoiml:::.csdg_reset_deprecations()
  messages = character()
  withCallingHandlers(csdg_claim_relation(parent, claim, legacy, "Legacy declaration."),
    mlr3autoiml_deprecated = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("Scope element name \"target\"", messages, fixed = TRUE)))
  expect_true(any(grepl("alternative_or_incomparable", messages, fixed = TRUE)))
})

test_that("a restriction without entailment narrows the scope and does not follow from its predecessor", {
  parent = csdg_claim(id = "whole_sample", statement = "The mean is positive in the whole sample.")
  claim = csdg_claim(id = "subgroup_only", statement = "The mean is positive in the subgroup.")
  narrower = same_context()
  narrower[["data"]] = "narrower"
  expect_error(csdg_claim_relation(parent, claim, narrower, "The subgroup is part of the sample.",
    proposition_relation = "logical_weakening", proposition_rationale = "Declared entailment.",
    revision_kind = "restriction_without_entailment"), "does not follow")
  expect_error(csdg_claim_relation(parent, claim, same_context(), "Nothing narrows.",
    revision_kind = "restriction_without_entailment"), "narrows the scope")
  mlr3autoiml:::.csdg_reset_deprecations()
  expect_warning(
    relation <- csdg_claim_relation(parent, claim, narrower, "The subgroup is part of the sample.",
      revision_kind = "scope_restriction"),
    class = "mlr3autoiml_deprecated"
  )
  expect_identical(relation$revision_kind, "restriction_without_entailment")
  mlr3autoiml:::.csdg_reset_deprecations()
  expect_warning(
    relation <- csdg_claim_relation(parent, claim, narrower, "The subgroup is part of the sample.",
      revision_kind = "context_restriction"),
    class = "mlr3autoiml_deprecated"
  )
  expect_identical(relation$revision_kind, "restriction_without_entailment")
})
