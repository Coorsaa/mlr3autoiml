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
    )
}
VALIDATORS = {key: Draft202012Validator(value) for key, value in SCHEMAS.items()}
CASES = {case["name"]: case for case in EXPORTED["cases"]}


class ClaimContractSchemaTests(unittest.TestCase):
    def test_schemas_are_valid(self):
        for schema in SCHEMAS.values():
            Draft202012Validator.check_schema(schema)

    def test_runtime_cases_are_schema_valid(self):
        self.assertEqual(len(CASES), 21)
        for name, case in CASES.items():
            with self.subTest(case=name):
                self.assertEqual(case["expected_decision"], case["adjudication"]["decision"])
                VALIDATORS["adjudication"].validate(case["adjudication"])
                for record in case["evidence"]:
                    VALIDATORS["evidence"].validate(record)

    def test_reference_fixtures_remain_legitimate_records(self):
        for name in ("module_inapplicable", "support_with_unresolved"):
            fixture = ROOT / "tests" / "schema" / "fixtures" / f"{name}.json"
            VALIDATORS["evidence"].validate(json.loads(fixture.read_text()))
        self.assertEqual(CASES["support_with_unresolved"]["adjudication"]["decision"], "unresolved")
        self.assertEqual(CASES["in_scope_no_applicable_modules"]["adjudication"]["decision"], "unresolved")

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
            dict(base, unresolved_gate_ids=["G2"]),
            dict(base, blocking_gate_ids=["G2"]),
            dict(base, decision="not_met"),
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

    def test_report_card_consequences_match_evidence_rules(self):
        for name in ("met", "not_met", "support_with_unresolved", "out_of_scope", "mixed_revision"):
            source = CASES[name]["evidence"][0]
            row = {key: value for key, value in source.items() if key in SCHEMAS["report"]["items"]["properties"]}
            row.update(evidence_role=source["role"], required=source["applicable"], criterion="",
                       status="unresolved", summary="Fixture evidence, not an aggregate decision.")
            VALIDATORS["report"].validate([row])
            if name == "support_with_unresolved":
                self.assertFalse(VALIDATORS["report"].is_valid([dict(row, applicable=False)]))
                self.assertFalse(VALIDATORS["report"].is_valid([dict(row, result_direction="challenges")]))
                self.assertFalse(VALIDATORS["report"].is_valid([dict(row, evidence_role="descriptive_context")]))


if __name__ == "__main__":
    print(json.dumps(EXPORTED["identity"], indent=2))
    unittest.main(argv=[sys.argv[0]], verbosity=2)
