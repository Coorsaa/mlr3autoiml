"""Validate actual R fixture outputs and adversarial mutations using Draft 2020-12.

Usage: python tests/schema/test_claim_contract.py PACKAGE_ROOT EXPORTED_CASES_JSON
The exporter must be run first against the source or the installed delivered package.
"""

import copy
import json
from pathlib import Path
import sys
import unittest

from jsonschema import Draft202012Validator


ROOT = Path(sys.argv[1]).resolve()
EXPORTED = json.loads(Path(sys.argv[2]).read_text())
SCHEMAS = {
    key: json.loads((ROOT / "inst" / "schema" / f"csdg-{name}.schema.json").read_text())
    for key, name in (
        ("evidence", "evidence-record"),
        ("adjudication", "claim-adjudication"),
        ("report", "report-card"),
        ("card", "card"),
        ("relation", "claim-relation"),
    )
}
VALIDATORS = {key: Draft202012Validator(value) for key, value in SCHEMAS.items()}
CASES = {case["name"]: case for case in EXPORTED["cases"]}


class ClaimContractSchemaTests(unittest.TestCase):
    def test_schemas_are_valid(self):
        for schema in SCHEMAS.values():
            Draft202012Validator.check_schema(schema)

    def test_runtime_cases_are_schema_valid(self):
        self.assertEqual(len(CASES), 27)
        for name, case in CASES.items():
            with self.subTest(case=name):
                self.assertEqual(case["expected_decision"], case["adjudication"]["assessment"])
                self.assertEqual(case["adjudication"]["decision"], case["adjudication"]["assessment"])
                VALIDATORS["adjudication"].validate(case["adjudication"])
                for record in case["evidence"]:
                    VALIDATORS["evidence"].validate(record)

    def test_reference_fixtures_remain_legitimate_records(self):
        for name in ("module_inapplicable", "support_with_unresolved"):
            fixture = ROOT / "tests" / "schema" / "fixtures" / f"{name}.json"
            VALIDATORS["evidence"].validate(json.loads(fixture.read_text()))
        self.assertEqual(CASES["support_with_unresolved"]["adjudication"]["assessment"], "unresolved")
        self.assertEqual(CASES["in_scope_no_applicable_modules"]["adjudication"]["assessment"], "unresolved")

    def test_inconsistent_evidence_is_rejected(self):
        base = CASES["met"]["evidence"][0]
        mutations = [
            dict(base, applicable=False),
            dict(base, claim_consequence="unresolved", result_direction="challenges"),
            dict(base, role="descriptive_context", claim_consequence="unresolved"),
            dict(base, availability="incomplete"),
            dict(base, materiality="materialized"),
            dict(base, unexpected_field="not part of the contract"),
        ]
        for field in ("role", "availability", "claim_consequence", "criterion"):
            damaged = copy.deepcopy(base)
            del damaged[field]
            mutations.append(damaged)
        for instance in mutations:
            with self.subTest(instance=instance):
                self.assertFalse(VALIDATORS["evidence"].is_valid(instance))

    def test_scope_and_precedence_cannot_be_lost_in_serialization(self):
        base = CASES["met"]["adjudication"]
        mutations = [
            dict(base, claim_applicable=False),
            dict(base, decision="not_applicable"),
            dict(base, assessment="not_applicable"),
            dict(base, unresolved_gate_ids=["G2"]),
            dict(base, blocking_gate_ids=["G2"]),
            dict(base, decision="not_met"),
            dict(base, assessment="not_met"),
            dict(base, assessment="unresolved"),
            dict(base, blocking_gate_ids="G2"),
            dict(CASES["out_of_scope"]["adjudication"], applicability_source="legacy_default"),
        ]
        for field in ("claim_applicable", "applicability_source", "applicability_rationale"):
            damaged = copy.deepcopy(base)
            del damaged[field]
            mutations.append(damaged)
        for instance in mutations:
            with self.subTest(instance=instance):
                self.assertFalse(VALIDATORS["adjudication"].is_valid(instance))

    def test_report_card_rows_follow_the_property_rules(self):
        for name in ("met", "not_met", "support_with_unresolved", "mixed_revision"):
            source = CASES[name]["adjudication"]["properties"][0]
            evidence = CASES[name]["evidence"][0]
            row = dict(
                gate_id=source["gate_id"], gate_name="Fixture gate", area="Foundation of the claim",
                required=True, plan_role="required", evidence_role="required_property", status=source["status"],
                availability=evidence["availability"], result_direction=evidence["result_direction"], criterion="",
                criterion_source=None, criterion_rationale=None, rationale=source["rationale"],
                summary="Fixture evidence, not an aggregate decision.",
            )
            VALIDATORS["report"].validate([row])
            self.assertFalse(VALIDATORS["report"].is_valid([dict(row, evidence_role="context")]))
            self.assertFalse(VALIDATORS["report"].is_valid([dict(row, status="not_required")]))
            self.assertFalse(VALIDATORS["report"].is_valid([dict(row, status="met")]))
        supported = dict(
            gate_id="G2", gate_name="Procedure", area="Predictions, explanations, and decisions", required=True,
            plan_role="required", evidence_role="required_property", status="supported", availability="complete",
            result_direction="supports", criterion="", criterion_source=None, criterion_rationale=None,
            rationale="Fixture.", summary="Fixture.",
        )
        self.assertFalse(VALIDATORS["report"].is_valid([dict(supported, availability="incomplete")]))
        # A gate that the claim does not require is context and has no property status.
        context = dict(
            gate_id="G1", gate_name="Predictive performance", area="Foundation of the claim", required=False,
            plan_role="context", evidence_role="context", status="context", diagnostic_status="open",
            availability="complete", result_direction="descriptive", criterion="", criterion_source=None,
            criterion_rationale=None, rationale="Fixture.", summary="Fixture.",
        )
        VALIDATORS["report"].validate([context])
        VALIDATORS["report"].validate([dict(context, status="not_required", diagnostic_status="not_required")])
        self.assertFalse(VALIDATORS["report"].is_valid([dict(context, status="open")]))
        self.assertFalse(VALIDATORS["report"].is_valid([dict(context, required=True, plan_role="required")]))
        self.assertFalse(VALIDATORS["report"].is_valid([dict(supported, status="context")]))

    def test_new_roles_follow_the_decision_rule(self):
        expected = {
            "unresolved_threat_on_required_g1": ("unresolved", [], ["G1"]),
            "counterevidence_with_supported_properties": ("not_met", ["G2"], []),
            "context_with_legacy_unresolved_consequence": ("met", [], []),
            "plan_required_gate_without_record": ("unresolved", [], ["G6a"]),
            "causal_claim_without_causal_design": ("unresolved", [], ["CD"]),
            "causal_claim_with_causal_design": ("met", [], []),
        }
        for name, (decision, contradicted, open_ids) in expected.items():
            with self.subTest(case=name):
                adjudication = CASES[name]["adjudication"]
                self.assertEqual(adjudication["assessment"], decision)
                self.assertEqual(adjudication["contradicted_gate_ids"], contradicted)
                self.assertEqual(adjudication["open_gate_ids"], open_ids)

    def test_exported_cards_follow_the_card_schema(self):
        cards = EXPORTED["exported_cards"]
        VALIDATORS["card"].validate(cards)
        self.assertEqual(cards["claim"]["scope"]["model"], "learner")
        damaged = copy.deepcopy(cards)
        del damaged["claim"]["scope"]["model"]
        self.assertFalse(VALIDATORS["card"].is_valid(damaged))

    def test_revision_kinds_follow_the_article(self):
        kinds = SCHEMAS["relation"]["properties"]["revision_kind"]["enum"]
        self.assertEqual(
            kinds, ["unspecified", "logical_weakening", "restriction_without_entailment", "change_of_question"]
        )


if __name__ == "__main__":
    print(json.dumps(EXPORTED["identity"], indent=2))
    unittest.main(argv=[sys.argv[0]], verbosity=2)
