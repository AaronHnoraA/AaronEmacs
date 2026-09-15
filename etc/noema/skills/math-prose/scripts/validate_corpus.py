#!/usr/bin/env python3
"""Validate a Math Prose JSONL corpus using only the Python standard library."""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path
from typing import Any, Iterable


BEHAVIORS = {
    "A0", "A1", "A2", "A3", "A4", "A5",
    "B1", "B2", "B3", "B4", "B5", "B6",
    "C1", "C2", "C3", "C4", "C5",
    "D1", "D2", "D3", "D4", "D5",
    "E1", "E2", "E3", "E4", "E5",
    "F1", "F2", "F3", "F4",
}

CORE_DISCIPLINES = {
    "pure-mathematics",
    "applied-mathematics",
    "probability-statistics",
    "optimization-numerical-analysis",
}

CORE_MIN_ANCHORS = 24
CORE_MIN_OBSERVATIONS = 60
CORE_MIN_BEHAVIORS = 24

SOURCE_ROLES = {"anchor", "comparison"}
SOURCE_GENRES = {"research-article", "textbook", "research-monograph", "survey-or-guide"}
CORPUS_LAYERS = {"core", "domain"}

DISCOURSE_POSITIONS = {
    "before-display",
    "display-lead",
    "where-clause",
    "after-display",
    "derivation-step",
    "result-statement",
    "proof-step",
    "paragraph-transition",
}

FORMULA_FORMS = {
    "symbol-or-notation",
    "definition",
    "equality-or-identity",
    "membership-or-signature",
    "mapping-or-transform",
    "differential-equation",
    "recurrence-or-update",
    "optimization",
    "inequality-or-bound",
    "limit-or-asymptotic",
    "integral-or-sum",
    "piecewise-or-cases",
    "probability-or-expectation",
    "theorem-or-proposition",
    "algorithmic-relation",
}

CLAIM_STRENGTHS = {
    "descriptive",
    "definitional",
    "exact-algebraic",
    "approximate",
    "one-way-consequence",
    "equivalence",
    "proved-result",
    "empirical-observation",
}

SECTION_ROLES = {
    "notation-or-preliminaries",
    "problem-formulation",
    "method-or-model",
    "derivation-or-analysis",
    "theorem-or-proof",
    "algorithm",
    "experiment-or-results",
    "discussion-or-limitations",
}

SECTION_LOCATIONS = {"main-text", "appendix", "supplement"}

STATUSES = {"seed", "provisional", "validated"}

BOUNDARY_CASE_TYPES = {"mathematical-counterexample", "misuse", "near-synonym"}

REQUIRED = {
    "source": {
        "id", "record_type", "corpus_layer", "source_role", "source_genre",
        "title", "year", "discipline", "citation", "access",
    },
    "observation": {
        "id", "record_type", "corpus_layer", "source_id", "behavior",
        "discourse_position", "formula_form", "claim_strength", "section_role",
        "locator", "cue", "summary", "quote_words",
    },
    "pattern": {
        "id", "record_type", "corpus_layer", "behavior", "intent",
        "constructions", "boundaries", "synthetic_example", "source_ids",
        "status",
    },
}


def read_jsonl(paths: Iterable[str]) -> tuple[list[dict[str, Any]], list[str]]:
    records: list[dict[str, Any]] = []
    errors: list[str] = []
    paths = list(paths)
    if paths.count("-") > 1:
        return records, ["standard input may be specified only once"]
    for path in paths:
        stream = sys.stdin if path == "-" else Path(path).open("r", encoding="utf-8")
        label = "stdin" if path == "-" else path
        try:
            for line_number, raw_line in enumerate(stream, start=1):
                line = raw_line.strip()
                location = f"{label}:{line_number}"
                if not line or line.startswith("#"):
                    continue
                try:
                    record = json.loads(line)
                except json.JSONDecodeError as exc:
                    errors.append(f"{location}: invalid JSON: {exc.msg}")
                    continue
                if not isinstance(record, dict):
                    errors.append(f"{location}: each record must be a JSON object")
                    continue
                record["_location"] = location
                records.append(record)
        finally:
            if stream is not sys.stdin:
                stream.close()
    return records, errors


def require_string_list(
    record: dict[str, Any],
    field: str,
    errors: list[str],
    *,
    normalize_duplicates: bool = False,
) -> None:
    value = record.get(field)
    if not isinstance(value, list) or not all(isinstance(item, str) and item.strip() for item in value):
        errors.append(f"{record['_location']}: {field} must be a list of non-empty strings")
        return

    seen: dict[str, str] = {}
    for item in value:
        normalized = (
            " ".join(item.split()).casefold() if normalize_duplicates else item
        )
        if normalized in seen:
            errors.append(
                f"{record['_location']}: {field} contains duplicate entries "
                f"{seen[normalized]!r} and {item!r}"
            )
        else:
            seen[normalized] = item


def require_non_empty_string(record: dict[str, Any], field: str, errors: list[str]) -> None:
    value = record.get(field)
    if not isinstance(value, str) or not value.strip():
        errors.append(f"{record['_location']}: {field} must be a non-empty string")


def check_enum(record: dict[str, Any], field: str, allowed: set[str], errors: list[str]) -> None:
    if record.get(field) not in allowed:
        errors.append(
            f"{record['_location']}: unsupported {field}={record.get(field)!r}; "
            f"expected one of {sorted(allowed)}"
        )


def validate_boundary_cases(
    pattern: dict[str, Any],
    observations: dict[str, dict[str, Any]],
    errors: list[str],
) -> None:
    """Validate optional, pattern-local misuse and counterexample evidence."""
    if "boundary_cases" not in pattern:
        return

    cases = pattern.get("boundary_cases")
    if not isinstance(cases, list) or not cases:
        errors.append(
            f"{pattern['_location']}: boundary_cases must be a non-empty list of objects"
        )
        return

    boundaries = pattern.get("boundaries", [])
    source_ids = pattern.get("source_ids", [])
    seen_cases: set[tuple[str, str]] = set()
    for index, case in enumerate(cases, start=1):
        case_location = f"{pattern['_location']}: boundary_cases[{index}]"
        if not isinstance(case, dict):
            errors.append(f"{case_location} must be an object")
            continue

        required = {"case_type", "boundary", "challenge", "safe_response"}
        missing = sorted(required - case.keys())
        if missing:
            errors.append(
                f"{case_location} missing required fields: {', '.join(missing)}"
            )
        if case.get("case_type") not in BOUNDARY_CASE_TYPES:
            errors.append(
                f"{case_location}: unsupported case_type={case.get('case_type')!r}; "
                f"expected one of {sorted(BOUNDARY_CASE_TYPES)}"
            )
        for field in ("boundary", "challenge", "safe_response"):
            if not isinstance(case.get(field), str) or not case[field].strip():
                errors.append(f"{case_location}: {field} must be a non-empty string")

        boundary = case.get("boundary")
        if isinstance(boundaries, list) and boundary not in boundaries:
            errors.append(
                f"{case_location}: boundary must exactly match one parent pattern boundary"
            )
        if isinstance(boundary, str) and isinstance(case.get("challenge"), str):
            normalized_case = (
                " ".join(boundary.split()).casefold(),
                " ".join(case["challenge"].split()).casefold(),
            )
            if normalized_case in seen_cases:
                errors.append(f"{case_location}: duplicate boundary case")
            else:
                seen_cases.add(normalized_case)

        evidence_present = False
        for field in ("observation_ids", "evaluation_ids"):
            if field not in case:
                continue
            value = case.get(field)
            if (
                not isinstance(value, list)
                or not value
                or not all(isinstance(item, str) and item.strip() for item in value)
            ):
                errors.append(
                    f"{case_location}: {field} must be a non-empty list of non-empty strings"
                )
                continue
            evidence_present = True
            if len(value) != len(set(value)):
                errors.append(f"{case_location}: {field} contains duplicate entries")

        if not evidence_present:
            errors.append(
                f"{case_location}: at least one of observation_ids or evaluation_ids "
                "must provide evidence"
            )

        observation_ids = case.get("observation_ids", [])
        if isinstance(observation_ids, list):
            for observation_id in observation_ids:
                if not isinstance(observation_id, str):
                    continue
                observation = observations.get(observation_id)
                if observation is None:
                    errors.append(
                        f"{case_location}: unknown observation_id {observation_id!r}"
                    )
                    continue
                observation_behaviors = {observation.get("behavior")}
                secondary_behaviors = observation.get("secondary_behaviors", [])
                if isinstance(secondary_behaviors, list):
                    observation_behaviors.update(secondary_behaviors)
                if pattern.get("behavior") not in observation_behaviors:
                    errors.append(
                        f"{case_location}: observation {observation_id!r} has behaviors "
                        f"{sorted(behavior for behavior in observation_behaviors if behavior)!r}, "
                        f"expected {pattern.get('behavior')!r}"
                    )
                if (
                    isinstance(source_ids, list)
                    and observation.get("source_id") not in source_ids
                ):
                    errors.append(
                        f"{case_location}: observation {observation_id!r} comes from "
                        "a source not listed in the parent pattern source_ids"
                    )


def validate(records: Iterable[dict[str, Any]], initial_errors: list[str]) -> list[str]:
    records = list(records)
    errors = list(initial_errors)
    ids: dict[str, str] = {}
    sources: dict[str, dict[str, Any]] = {}
    observations: dict[str, dict[str, Any]] = {}
    observed_pairs: set[tuple[Any, Any]] = set()
    for record in records:
        if record.get("record_type") != "observation":
            continue
        observed_pairs.add((record.get("source_id"), record.get("behavior")))
        secondary_behaviors = record.get("secondary_behaviors", [])
        if isinstance(secondary_behaviors, list):
            observed_pairs.update(
                (record.get("source_id"), behavior)
                for behavior in secondary_behaviors
            )

    for record in records:
        location = record["_location"]
        record_type = record.get("record_type")
        if record_type not in REQUIRED:
            errors.append(f"{location}: record_type must be source, observation, or pattern")
            continue
        missing = sorted(REQUIRED[record_type] - record.keys())
        if missing:
            errors.append(f"{location}: missing required fields: {', '.join(missing)}")
        record_id = record.get("id")
        if not isinstance(record_id, str) or not record_id.strip():
            errors.append(f"{location}: id must be a non-empty string")
        elif record_id in ids:
            errors.append(f"{location}: duplicate id {record_id!r}; first used at {ids[record_id]}")
        else:
            ids[record_id] = location
            if record_type == "source":
                sources[record_id] = record
            elif record_type == "observation":
                observations[record_id] = record

    for record in records:
        record_type = record.get("record_type")
        if record_type not in REQUIRED:
            continue
        if record_type == "source":
            check_enum(record, "corpus_layer", CORPUS_LAYERS, errors)
            check_enum(record, "source_role", SOURCE_ROLES, errors)
            check_enum(record, "source_genre", SOURCE_GENRES, errors)
            require_non_empty_string(record, "discipline", errors)
            if record.get("corpus_layer") == "core":
                check_enum(record, "discipline", CORE_DISCIPLINES, errors)
                if "domain" in record:
                    errors.append(f"{record['_location']}: core sources must not set domain")
            elif record.get("corpus_layer") == "domain":
                require_non_empty_string(record, "domain", errors)
            year = record.get("year")
            if not isinstance(year, int) or year < 1500 or year > 2100:
                errors.append(f"{record['_location']}: year must be an integer from 1500 to 2100")
            if record.get("source_role") == "anchor":
                require_non_empty_string(record, "influence_basis", errors)
                require_non_empty_string(record, "influence_source", errors)

        elif record_type == "observation":
            check_enum(record, "corpus_layer", CORPUS_LAYERS, errors)
            check_enum(record, "behavior", BEHAVIORS, errors)
            check_enum(record, "discourse_position", DISCOURSE_POSITIONS, errors)
            check_enum(record, "formula_form", FORMULA_FORMS, errors)
            check_enum(record, "claim_strength", CLAIM_STRENGTHS, errors)
            check_enum(record, "section_role", SECTION_ROLES, errors)
            if "section_location" in record:
                check_enum(
                    record, "section_location", SECTION_LOCATIONS, errors
                )
            if "secondary_behaviors" in record:
                secondary_behaviors = record.get("secondary_behaviors")
                require_string_list(record, "secondary_behaviors", errors)
                if isinstance(secondary_behaviors, list):
                    unsupported = sorted(
                        behavior
                        for behavior in secondary_behaviors
                        if behavior not in BEHAVIORS
                    )
                    if unsupported:
                        errors.append(
                            f"{record['_location']}: unsupported secondary_behaviors: "
                            f"{', '.join(unsupported)}"
                        )
                    if record.get("behavior") in secondary_behaviors:
                        errors.append(
                            f"{record['_location']}: secondary_behaviors must not repeat "
                            "the primary behavior"
                        )
            if "conditions" in record:
                require_string_list(
                    record, "conditions", errors, normalize_duplicates=True
                )
            source = sources.get(record.get("source_id"))
            if source is None:
                errors.append(f"{record['_location']}: unknown source_id {record.get('source_id')!r}")
            elif source.get("corpus_layer") != record.get("corpus_layer"):
                errors.append(
                    f"{record['_location']}: observation corpus_layer must match its source"
                )
            quote = record.get("quote", "")
            quote_words = record.get("quote_words")
            if not isinstance(quote_words, int) or quote_words < 0:
                errors.append(f"{record['_location']}: quote_words must be a non-negative integer")
            elif quote_words > 25:
                errors.append(f"{record['_location']}: public-corpus quote exceeds 25 words")
            if quote:
                actual_words = len(str(quote).split())
                if quote_words != actual_words:
                    errors.append(
                        f"{record['_location']}: quote_words={quote_words} but quote contains {actual_words} words"
                    )

        elif record_type == "pattern":
            check_enum(record, "corpus_layer", CORPUS_LAYERS, errors)
            if record.get("corpus_layer") == "core":
                if "domain" in record:
                    errors.append(f"{record['_location']}: core patterns must not set domain")
            elif record.get("corpus_layer") == "domain":
                require_non_empty_string(record, "domain", errors)
            check_enum(record, "behavior", BEHAVIORS, errors)
            check_enum(record, "status", STATUSES, errors)
            require_string_list(
                record, "constructions", errors, normalize_duplicates=True
            )
            require_string_list(record, "boundaries", errors, normalize_duplicates=True)
            require_string_list(record, "source_ids", errors)
            source_ids = record.get("source_ids", [])
            if isinstance(source_ids, list):
                unknown = sorted(source_id for source_id in source_ids if source_id not in sources)
                if unknown:
                    errors.append(f"{record['_location']}: unknown source_ids: {', '.join(unknown)}")
                unsupported = sorted(
                    source_id for source_id in source_ids
                    if source_id in sources
                    and (source_id, record.get("behavior")) not in observed_pairs
                )
                if unsupported:
                    errors.append(
                        f"{record['_location']}: source_ids without a matching observation "
                        f"for behavior {record.get('behavior')}: {', '.join(unsupported)}"
                    )
                matching_anchor_source_ids = {
                    source_id for source_id in source_ids
                    if source_id in sources
                    and sources[source_id].get("source_role") == "anchor"
                    and sources[source_id].get("corpus_layer") == record.get("corpus_layer")
                    and (
                        record.get("corpus_layer") == "core"
                        or sources[source_id].get("domain") == record.get("domain")
                    )
                }
                if record.get("corpus_layer") == "core":
                    domain_sources = sorted(
                        source_id for source_id in source_ids
                        if source_id in sources and sources[source_id].get("corpus_layer") == "domain"
                    )
                    if domain_sources:
                        errors.append(
                            f"{record['_location']}: core patterns cannot depend on domain sources: "
                            f"{', '.join(domain_sources)}"
                        )
                if record.get("status") == "validated" and len(matching_anchor_source_ids) < 3:
                    errors.append(
                        f"{record['_location']}: validated patterns require at least three independent "
                        f"anchor sources from the same corpus layer"
                    )
                if record.get("status") == "provisional" and not matching_anchor_source_ids:
                    errors.append(
                        f"{record['_location']}: provisional patterns require at least one anchor source "
                        f"from the same corpus layer"
                    )
            validate_boundary_cases(record, observations, errors)

    return errors


def validate_core_readiness(records: Iterable[dict[str, Any]]) -> list[str]:
    """Check the machine-verifiable part of the core-corpus expansion gate."""
    records = list(records)
    core_sources = [
        record for record in records
        if record.get("record_type") == "source" and record.get("corpus_layer") == "core"
    ]
    core_anchors = [
        record for record in core_sources if record.get("source_role") == "anchor"
    ]
    core_observations = [
        record for record in records
        if record.get("record_type") == "observation" and record.get("corpus_layer") == "core"
    ]
    observed_behaviors = {
        record.get("behavior") for record in core_observations if record.get("behavior")
    }
    anchor_disciplines = {
        record.get("discipline") for record in core_anchors if record.get("discipline")
    }

    errors: list[str] = []
    if len(core_anchors) < CORE_MIN_ANCHORS:
        errors.append(
            f"core-readiness: requires at least {CORE_MIN_ANCHORS} anchor sources; "
            f"found {len(core_anchors)}"
        )
    if len(core_observations) < CORE_MIN_OBSERVATIONS:
        errors.append(
            f"core-readiness: requires at least {CORE_MIN_OBSERVATIONS} observations; "
            f"found {len(core_observations)}"
        )
    if len(observed_behaviors) < CORE_MIN_BEHAVIORS:
        errors.append(
            f"core-readiness: requires at least {CORE_MIN_BEHAVIORS} observed behavior codes; "
            f"found {len(observed_behaviors)}"
        )
    missing_disciplines = sorted(CORE_DISCIPLINES - anchor_disciplines)
    if missing_disciplines:
        errors.append(
            "core-readiness: missing anchor disciplines: " + ", ".join(missing_disciplines)
        )
    return errors


def summarize(records: Iterable[dict[str, Any]]) -> str:
    records = list(records)
    types = Counter(record.get("record_type", "unknown") for record in records)
    behaviors = Counter(
        record.get("behavior") for record in records
        if record.get("record_type") == "observation" and record.get("behavior")
    )
    disciplines = Counter(
        record.get("discipline") for record in records
        if record.get("record_type") == "source" and record.get("discipline")
    )
    source_roles = Counter(
        record.get("source_role") for record in records
        if record.get("record_type") == "source" and record.get("source_role")
    )
    source_genres = Counter(
        record.get("source_genre") for record in records
        if record.get("record_type") == "source" and record.get("source_genre")
    )
    corpus_layers = Counter(
        record.get("corpus_layer") for record in records
        if record.get("corpus_layer")
    )
    section_roles = Counter(
        record.get("section_role") for record in records
        if record.get("record_type") == "observation" and record.get("section_role")
    )
    section_locations = Counter(
        record.get("section_location") for record in records
        if record.get("record_type") == "observation" and record.get("section_location")
    )
    statuses = Counter(
        record.get("status") for record in records
        if record.get("record_type") == "pattern" and record.get("status")
    )
    construction_frames = sum(
        len(record.get("constructions", [])) for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("constructions"), list)
    )
    boundary_rules = sum(
        len(record.get("boundaries", [])) for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundaries"), list)
    )
    patterns_with_boundary_cases = sum(
        1 for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundary_cases"), list)
        and record.get("boundary_cases")
    )
    boundary_cases = sum(
        len(record.get("boundary_cases", [])) for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundary_cases"), list)
    )
    mapped_boundaries = sum(
        len({
            case.get("boundary")
            for case in record.get("boundary_cases", [])
            if isinstance(case, dict) and isinstance(case.get("boundary"), str)
        })
        for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundary_cases"), list)
    )
    observation_links = sum(
        len(case.get("observation_ids", []))
        for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundary_cases"), list)
        for case in record.get("boundary_cases", [])
        if isinstance(case, dict) and isinstance(case.get("observation_ids"), list)
    )
    evaluation_links = sum(
        len(case.get("evaluation_ids", []))
        for record in records
        if record.get("record_type") == "pattern"
        and isinstance(record.get("boundary_cases"), list)
        for case in record.get("boundary_cases", [])
        if isinstance(case, dict) and isinstance(case.get("evaluation_ids"), list)
    )
    type_summary = ", ".join(f"{key}={value}" for key, value in sorted(types.items())) or "none"
    behavior_summary = ", ".join(f"{key}={value}" for key, value in sorted(behaviors.items())) or "none"
    discipline_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(disciplines.items())
    ) or "none"
    source_role_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(source_roles.items())
    ) or "none"
    source_genre_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(source_genres.items())
    ) or "none"
    corpus_layer_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(corpus_layers.items())
    ) or "none"
    section_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(section_roles.items())
    ) or "none"
    section_location_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(section_locations.items())
    ) or "none"
    status_summary = ", ".join(
        f"{key}={value}" for key, value in sorted(statuses.items())
    ) or "none"
    return (
        f"records: {type_summary}\n"
        f"behaviors: {behavior_summary}\n"
        f"corpus_layers: {corpus_layer_summary}\n"
        f"source_roles: {source_role_summary}\n"
        f"source_genres: {source_genre_summary}\n"
        f"disciplines: {discipline_summary}\n"
        f"section_roles: {section_summary}\n"
        f"section_locations: {section_location_summary}\n"
        f"pattern_inventory: constructions={construction_frames}, boundaries={boundary_rules}\n"
        f"boundary_evidence: patterns={patterns_with_boundary_cases}, "
        f"cases={boundary_cases}, mapped_boundaries={mapped_boundaries}, "
        f"unmapped_boundaries={max(0, boundary_rules - mapped_boundaries)}, "
        f"observation_links={observation_links}, evaluation_links={evaluation_links}\n"
        f"pattern_statuses: {status_summary}"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "corpora", nargs="+", help="one or more JSONL corpus paths; use - once for standard input"
    )
    parser.add_argument(
        "--require-core-ready",
        action="store_true",
        help="also enforce the machine-verifiable core expansion gate",
    )
    args = parser.parse_args()

    records, parse_errors = read_jsonl(args.corpora)
    errors = validate(records, parse_errors)
    if args.require_core_ready and not errors:
        errors.extend(validate_core_readiness(records))
    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        print(f"FAIL: {len(errors)} validation error(s)", file=sys.stderr)
        return 1

    print("PASS")
    print(summarize(records))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
