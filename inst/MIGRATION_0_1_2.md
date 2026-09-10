# Migration to mlr3autoiml 0.1.2

Version 0.1.2 preserves diagnostic functions and historical claim records while separating the proposition from its
context. It does not reinterpret historical evidence as newly verified or introduce a score or universal threshold.

## Claim comparisons

A claim is `C = (phi, kappa)`. The existing `statement` field represents the inferential proposition `phi`; the six
coordinates represent the context `kappa`. A sentence may contain several propositions, and one proposition may need
more than one sentence. The package does not parse or prove natural-language statements.

The existing four positional arguments of `csdg_claim_relation()` still work. The intentional semantic correction is:

```r
relation = csdg_claim_relation(parent, claim, coordinate_relations, rationale)
relation$context_relation  # Previous coordinate-only aggregate
relation$relation          # "unchecked"
relation$semantic_status   # "unchecked"
```

Consumers previously using `$relation` as the context aggregate should use `$context_relation`. Do not replace an
unchecked proposition relation with the coordinate aggregate and describe it as logical equivalence or entailment.
Saved pre-0.1.2 relation objects without proposition fields remain historical declarations; their semantic comparison
must be treated as unchecked. The historical object need not be overwritten.

For an explicit comparison, supply `proposition_relation` and `proposition_rationale`. The allowed relations are
`unchecked`, `same`, `logical_weakening`, `logical_strengthening`, and `alternative_or_incomparable`. Weakening means
that the parent entails the revised proposition under the declared assumptions. The returned legacy-style `relation`
maps weakening to `narrower` and strengthening to `broader`. `semantic_status = "declared_not_verified"` makes the
software boundary explicit. `revision_kind` distinguishes `logical_weakening`, `context_restriction`, `estimand_change`,
and an unspecified kind without proving the declaration.

Equal contexts can contain opposite PFI inequalities. A positive population mean does not imply a positive subgroup
mean. By contrast, a universal property can restrict to a subset under an explicit subset assumption. A change from a
local surrogate target to global PFI is an alternative question, not necessarily a weaker proposition. These four
counterexamples are executable regression tests.

## Sensitivity and adjudication

Optional evidence fields are `varied_component`, `held_constant`, `same_estimand`, `same_estimand_rationale`,
`invariance_claimed`, `required_property`, `observation`, and `relevance_to_proposition`.

`same_estimand` and `invariance_claimed` accept `TRUE`, `FALSE`, or `NA`. A variation record requires the varied
components, held-constant components, and an estimand rationale; `NA` is not interpreted as sameness. If any part of the
property-observation-relevance chain is supplied, all three parts are required. New claim-constraining variation needs
that complete chain. If the estimand changes or remains uncertain, the proposition must explicitly claim the relevant
invariance before the variation can constrain it. The same guard applies to necessary requirements, preventing an
estimand change from bypassing the rule through a different evidence role.

```r
record = csdg_evidence_record(
  gate_id = "G4", applicable = TRUE, role = "descriptive_context",
  rationale = "The two explanations answer different scale-specific questions.",
  varied_component = "output_scale",
  held_constant = c("frozen model", "cases", "neighborhood", "weights"),
  same_estimand = FALSE,
  same_estimand_rationale = "Probability and logit errors have different targets and units.",
  invariance_claimed = FALSE
)
```

This record does not automatically materialize a defeater. Numerical seed variation at the same target is different
from changing a reference distribution, model, or population. Its relevance depends on the property actually asserted.

Legacy calls without these optional fields remain usable and return `variation_status = "not_recorded"` and
`proposition_linkage = "not_recorded"`. Their existing decision semantics are preserved, not newly certified. Fully
documented chains have status `documented_not_verified`: filling a rationale or chain is not empirical validation.
`csdg_adjudicate_claim()` rechecks record consistency and exposes whether linkage was recorded for all evidence.

## Claim history

Use a new claim identifier before changing its proposition. `csdg_claim_revision()` retains the parent identifier but
does not inherit provenance or the legacy `confirmatory` flag automatically. Supply new metadata explicitly:

```r
provenance = list(
  origin = "retrospective_exploratory",
  date = "2026-09-10",
  time_basis = "Dated revision after inspection of earlier results.",
  selection_basis = "The prior diagnostic contrast motivated the revised question.",
  evidence_ids = c("E01", "E02")
)
```

The other origins are `specified_before_results` and `independently_confirmed`. The latter requires evidence identifiers
but does not establish their independence. These are provenance categories, not ordered evidence levels. Recording a
revision does not correct selection bias, create preregistration, or establish a selective-inference method.

## Source access

`csdg_resolve_sources(registry, root)` accepts a list of entries with `id`, `access`, and `rationale`. Public sources
require an exact-case relative `path` to a file below `root`. Protected sources require an opaque `path` or `uri`;
external sources require a `uri`. Neither protected nor external sources are read. Optional `sha256` values are checked
against public file contents only.

`dependencies` and `available_aggregate_ids` are vectors of registered source identifiers. Aggregate targets must be
public. Cycles are checked separately for dependencies and aggregate substitutions, allowing a public aggregate to
depend on the protected source for which it is also a substitute. The returned `verified_public`,
`declared_protected`, and `declared_external` states concern source access, not scientific support. An aggregate cannot
be assumed to contain the paired individual information in a protected source merely because it is available.
