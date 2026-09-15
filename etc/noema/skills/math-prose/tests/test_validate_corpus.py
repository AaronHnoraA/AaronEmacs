from __future__ import annotations

import copy
import importlib.util
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "validate_corpus", ROOT / "scripts" / "validate_corpus.py"
)
assert SPEC and SPEC.loader
validate_corpus = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(validate_corpus)


class CorpusValidationTests(unittest.TestCase):
    def boundary_fixture(self) -> list[dict]:
        source = {
            "_location": "test:1",
            "id": "src-test",
            "record_type": "source",
            "corpus_layer": "core",
            "source_role": "anchor",
            "source_genre": "textbook",
            "title": "Test",
            "year": 2026,
            "discipline": "pure-mathematics",
            "citation": "Test citation",
            "access": "test fixture",
            "influence_basis": "Test fixture anchor",
            "influence_source": "https://example.com",
        }
        observation = {
            "_location": "test:2",
            "id": "obs-test",
            "record_type": "observation",
            "corpus_layer": "core",
            "source_id": "src-test",
            "behavior": "B5",
            "secondary_behaviors": ["D5"],
            "discourse_position": "after-display",
            "formula_form": "optimization",
            "claim_strength": "proved-result",
            "section_role": "theorem-or-proof",
            "section_location": "appendix",
            "conditions": ["The target set is a nonempty closed convex set."],
            "locator": "fixture",
            "cue": "metric projection",
            "summary": "Fixture observation.",
            "quote_words": 0,
        }
        boundary = "Reserve orthogonal projection for a subspace."
        pattern = {
            "_location": "test:3",
            "id": "pat-test",
            "record_type": "pattern",
            "corpus_layer": "core",
            "behavior": "B5",
            "intent": "Fixture pattern.",
            "constructions": ["project ... onto ..."],
            "boundaries": [boundary],
            "boundary_cases": [
                {
                    "case_type": "near-synonym",
                    "boundary": boundary,
                    "challenge": "Calling every nearest-point map orthogonal.",
                    "safe_response": "Use metric projection for a general convex target.",
                    "observation_ids": ["obs-test"],
                    "evaluation_ids": ["CF4-1R3"],
                }
            ],
            "synthetic_example": "Project x onto C in the stated metric.",
            "source_ids": ["src-test"],
            "status": "provisional",
        }
        return [source, observation, pattern]

    def test_math_core_passes_readiness_gate(self) -> None:
        records, parse_errors = validate_corpus.read_jsonl(
            [str(ROOT / "references" / "corpora" / "math-core.jsonl")]
        )
        self.assertEqual(validate_corpus.validate(records, parse_errors), [])
        self.assertEqual(validate_corpus.validate_core_readiness(records), [])

    def test_small_core_fails_readiness_gate(self) -> None:
        records, _ = validate_corpus.read_jsonl(
            [str(ROOT / "references" / "corpora" / "math-core.jsonl")]
        )
        errors = validate_corpus.validate_core_readiness(records[:1])
        self.assertTrue(any("anchor sources" in error for error in errors))
        self.assertTrue(any("observations" in error for error in errors))
        self.assertTrue(any("anchor disciplines" in error for error in errors))

    def test_pattern_sources_require_matching_behavior_observations(self) -> None:
        source = {
            "_location": "test:1",
            "id": "src-test",
            "record_type": "source",
            "corpus_layer": "core",
            "source_role": "anchor",
            "source_genre": "textbook",
            "title": "Test",
            "year": 2026,
            "discipline": "pure-mathematics",
            "citation": "Test citation",
            "access": "test fixture",
            "influence_basis": "Test fixture anchor",
            "influence_source": "https://example.com",
        }
        observation = {
            "_location": "test:2",
            "id": "obs-test",
            "record_type": "observation",
            "corpus_layer": "core",
            "source_id": "src-test",
            "behavior": "A1",
            "discourse_position": "where-clause",
            "formula_form": "symbol-or-notation",
            "claim_strength": "definitional",
            "section_role": "notation-or-preliminaries",
            "locator": "fixture",
            "cue": "denotes",
            "summary": "Fixture observation.",
            "quote_words": 0,
        }
        pattern = {
            "_location": "test:3",
            "id": "pat-test",
            "record_type": "pattern",
            "corpus_layer": "core",
            "behavior": "A2",
            "intent": "Fixture pattern.",
            "constructions": ["define ... by ..."],
            "boundaries": ["Fixture boundary."],
            "synthetic_example": "Define x by x:=0.",
            "source_ids": ["src-test"],
            "status": "provisional",
        }

        errors = validate_corpus.validate([source, observation, pattern], [])
        self.assertTrue(any("without a matching observation" in error for error in errors))

    def test_source_genre_is_required_and_checked(self) -> None:
        source = {
            "_location": "test:1",
            "id": "src-test",
            "record_type": "source",
            "corpus_layer": "core",
            "source_role": "comparison",
            "source_genre": "lecture-notes",
            "title": "Test",
            "year": 2026,
            "discipline": "pure-mathematics",
            "citation": "Test citation",
            "access": "test fixture",
        }

        errors = validate_corpus.validate([source], [])
        self.assertTrue(any("unsupported source_genre" in error for error in errors))

    def test_summary_behavior_counts_include_observations_only(self) -> None:
        records = [
            {"record_type": "observation", "behavior": "C1"},
            {"record_type": "pattern", "behavior": "C1"},
            {"record_type": "pattern", "behavior": "E2"},
        ]

        behavior_line = next(
            line for line in validate_corpus.summarize(records).splitlines()
            if line.startswith("behaviors:")
        )
        self.assertEqual(behavior_line, "behaviors: C1=1")

    def test_summary_counts_pattern_inventory_only(self) -> None:
        records = [
            {
                "record_type": "pattern",
                "constructions": ["define ... by ...", "is called ... if ..."],
                "boundaries": ["Keep the definition stipulative."],
            },
            {
                "record_type": "observation",
                "constructions": ["must not be counted"],
                "boundaries": ["must not be counted"],
            },
        ]

        inventory_line = next(
            line for line in validate_corpus.summarize(records).splitlines()
            if line.startswith("pattern_inventory:")
        )
        self.assertEqual(
            inventory_line,
            "pattern_inventory: constructions=2, boundaries=1",
        )

    def test_pattern_inventory_rejects_normalized_duplicates(self) -> None:
        pattern = {
            "_location": "test:1",
            "id": "pat-test",
            "record_type": "pattern",
            "corpus_layer": "core",
            "behavior": "A0",
            "intent": "Fixture pattern.",
            "constructions": ["is continuous", "  Is   Continuous  "],
            "boundaries": ["State the domain.", "state   the domain."],
            "synthetic_example": "The map is continuous.",
            "source_ids": [],
            "status": "seed",
        }

        errors = validate_corpus.validate([pattern], [])
        self.assertTrue(
            any("constructions contains duplicate entries" in error for error in errors)
        )
        self.assertTrue(
            any("boundaries contains duplicate entries" in error for error in errors)
        )

    def test_legacy_pattern_without_boundary_cases_remains_valid(self) -> None:
        records = self.boundary_fixture()
        del records[2]["boundary_cases"]
        self.assertEqual(validate_corpus.validate(records, []), [])

    def test_boundary_case_accepts_observation_and_evaluation_evidence(self) -> None:
        self.assertEqual(
            validate_corpus.validate(self.boundary_fixture(), []),
            [],
        )

    def test_secondary_behavior_can_support_pattern_and_boundary_case(self) -> None:
        records = self.boundary_fixture()
        records[2]["behavior"] = "D5"
        self.assertEqual(validate_corpus.validate(records, []), [])

    def test_boundary_case_requires_parent_boundary_match(self) -> None:
        records = self.boundary_fixture()
        records[2]["boundary_cases"][0]["boundary"] = "An unrelated boundary."
        errors = validate_corpus.validate(records, [])
        self.assertTrue(any("exactly match one parent pattern boundary" in error for error in errors))

    def test_boundary_case_checks_observation_identity_behavior_and_source(self) -> None:
        unknown_records = self.boundary_fixture()
        unknown_records[2]["boundary_cases"][0]["observation_ids"] = ["obs-missing"]
        unknown_errors = validate_corpus.validate(unknown_records, [])
        self.assertTrue(any("unknown observation_id" in error for error in unknown_errors))

        behavior_records = self.boundary_fixture()
        behavior_records[1]["behavior"] = "D5"
        behavior_records[1]["secondary_behaviors"] = ["A3"]
        behavior_errors = validate_corpus.validate(behavior_records, [])
        self.assertTrue(any("expected 'B5'" in error for error in behavior_errors))

        source_records = self.boundary_fixture()
        other_source = copy.deepcopy(source_records[0])
        other_source["_location"] = "test:4"
        other_source["id"] = "src-other"
        source_records[1]["source_id"] = "src-other"
        source_errors = validate_corpus.validate(
            [source_records[0], other_source, source_records[1], source_records[2]],
            [],
        )
        self.assertTrue(
            any("not listed in the parent pattern source_ids" in error for error in source_errors)
        )

    def test_boundary_case_requires_evidence_channel(self) -> None:
        records = self.boundary_fixture()
        del records[2]["boundary_cases"][0]["observation_ids"]
        del records[2]["boundary_cases"][0]["evaluation_ids"]
        errors = validate_corpus.validate(records, [])
        self.assertTrue(any("at least one of observation_ids or evaluation_ids" in error for error in errors))

    def test_boundary_case_rejects_invalid_type_duplicate_ids_and_non_object(self) -> None:
        records = self.boundary_fixture()
        case = records[2]["boundary_cases"][0]
        case["case_type"] = "example"
        case["observation_ids"] = ["obs-test", "obs-test"]
        records[2]["boundary_cases"].append("not-an-object")
        errors = validate_corpus.validate(records, [])
        self.assertTrue(any("unsupported case_type" in error for error in errors))
        self.assertTrue(any("observation_ids contains duplicate entries" in error for error in errors))
        self.assertTrue(any("must be an object" in error for error in errors))

    def test_observation_optional_axes_are_checked(self) -> None:
        records = self.boundary_fixture()
        records[1]["secondary_behaviors"] = ["B5", "Z9"]
        records[1]["section_location"] = "back-matter"
        errors = validate_corpus.validate(records, [])
        self.assertTrue(any("unsupported secondary_behaviors" in error for error in errors))
        self.assertTrue(any("must not repeat the primary behavior" in error for error in errors))
        self.assertTrue(any("unsupported section_location" in error for error in errors))

    def test_boundary_evidence_summary_is_separate_from_pattern_inventory(self) -> None:
        summary = validate_corpus.summarize(self.boundary_fixture())
        inventory_line = next(
            line for line in summary.splitlines()
            if line.startswith("pattern_inventory:")
        )
        evidence_line = next(
            line for line in summary.splitlines()
            if line.startswith("boundary_evidence:")
        )
        self.assertEqual(
            inventory_line,
            "pattern_inventory: constructions=1, boundaries=1",
        )
        self.assertEqual(
            evidence_line,
            "boundary_evidence: patterns=1, cases=1, mapped_boundaries=1, "
            "unmapped_boundaries=0, observation_links=1, evaluation_links=1",
        )


if __name__ == "__main__":
    unittest.main()
