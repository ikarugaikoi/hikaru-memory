# Known limitations and possible improvements

[中文](../limitations.md) · **English** · [日本語](../ja/limitations.md) · [Home](../../README.en.md)

Hikaru implements memory storage, revision, retrieval, association-trigger filtering, and autonomous-activity state updates. Evidence differs by module: synthetic checks verify specific program behavior, while historical replays observe usefulness. Neither substitutes for the other. This document separates observations, unresolved problems, and possible approaches to validation.

## Limits by module

| Part | Current limitation | Possible next direction |
|---|---|---|
| Clumping and general frequency | Shared expressions built from common words can look ordinary; language switching can also create concentration | Freeze statistical definitions and validate on later dates and different language proportions; track losses of shared expressions separately |
| Entity timeline | The same word need not refer to the same entity; the latest mention need not be the most useful | Consider small experiments with Combinatory Categorial Grammar (CCG) syntax–semantics and anaphora/presupposition in Dependent Type Semantics (DTS), to check referents and event roles |
| Delivery and usefulness | No reliable method yet for deciding whether a candidate would be useful in the current conversation; delivery or mention is not proof of usefulness | No established method yet |
| Active retrieval | Tool access does not ensure invocation when needed; similarity ranking does not equal usefulness | Consider experiments with compositional semantic representations and type-theoretic proof search to check predicate–argument structure, negation, and conditional relations between queries and candidates. Whether the model initiates retrieval remains separate |
| CA review and understanding revision | Programs check structures and consume judgments; context, evidential relationships, and scope still depend on the model | Consider experimental combinations of latent-cause learning and CA observations to explore when new experience should revise an expectation and when it should be treated as specific to a different context |
| Reflection and self-reinforcement | Deduplication prevents some double counting but cannot identify every mistaken support judgment | Needs more long-term operational data; no specific improvement plan yet |
| Compression and long-term retention | Protections and original records remain, but timely recovery of important compressed details is unverified | Needs more long-term operational data; no specific improvement plan yet |
| Autonomous activity | Interest/effort inputs come from the model; defaults are engineering choices without long-term effectiveness validation | Needs more long-term operational data; no specific improvement plan yet |
| Installation and integration | Primarily macOS and specific host logs; fresh-machine setup and some services are not fully verified | None currently planned |

The proposed directions for timelines and active retrieval involve CCG parsing and semantic composition, DTS discourse representations, and type checking/proof search. Timelines emphasize anaphoric referents and their contextual accessibility; active retrieval emphasizes checkable semantic relations between query and candidate. These are theoretical directions for possible experiments, not committed implementations.

Any experiment should preserve source positions, distinguish explicit information from inferred referents/roles, and compare against simple rules. Failed parses, unclear references, and missing proofs remain unknown. Formal results depend on the chosen parse; small Japanese samples do not establish performance on mixed Chinese/Japanese material. Adoption would depend on usefulness, false positives, latency, and maintenance. Neither retrieval path currently integrates these methods.

## Appendix: association-trigger experiments

### What was evaluated

One exploratory run used all archived conversation passages from 30 conversation days, counting both speakers. Word-level evaluation covered 305 entity aliases occurring at least three times:

- 165 labeled concrete names or shared expressions;
- 41 labeled purely everyday expressions;
- 99 undetermined, excluded from both class denominators.

One model supplied the labels, initially without seeing scores. The everyday-expression category was narrowed later. Segmentation, reference sample size, and thresholds were explored on the same material, with no independent holdout.

The candidate condition disabled an automatic trigger when occurrences > 6, general-frequency Zipf ≥ 4.0, and day-based clumping < 0.25. Older filtering also included high day coverage, shared aliases, and suffixes of other aliases. The final replay compared adding this condition to the older rules.

### Retained parameter comparisons

The interim summary retained the following rounded values. Each cell shows “concrete names/shared expressions retained / everyday expressions excluded”:

| General-frequency minimum | Clumping upper bound | By day | By session segment |
|---|---|---|---|
| 4.0 | 0.15 | 99% / 24% | 99% / 37% |
| 4.0 | 0.25 | 98% / 37% | 99% / 37% |
| 4.0 | 0.35 | 98% / 44% | 97% / 41% |
| 3.5 | 0.25 | 96% / 41% | 97% / 44% |

These values survive in the contemporary discussion summary. Full per-condition configurations, label versions, and count tables have not been recovered, so they cannot be merged directly with final combined-rule results. Day-based segmentation was chosen partly for which specific triggers it lost, not just aggregate rates.

Other explorations mainly survive as these notes:

| Parameter / treatment | Options tried | Retained conclusion |
|---|---|---|
| Document segmentation | Every 10, 20, 40, 80, 160 original passages; session segments; days | Preference for days; different boundaries lose different triggers |
| Reference string length | 2–4, 2–6, 3–8 characters | Report selected 2–4 |
| Reference size and seeds | 1,000, 3,000, 6,000; three random seeds | Report found relative stability at 3,000 and above; full tables not retained |
| Character-script background correction | None vs. script-based correction | Reduced some language-switch effects but also suppressed topics, so not adopted as a filtering criterion |
| Low-frequency exemption | Final condition exempts occurrences ≤ 6 | No comparisons across thresholds have been recovered; 6 is not established as a thoroughly optimized choice |

Later checks found that 0.25 versus 0.35 changed treatment of only four aliases, none of which triggered delivery in that replay. The less restrictive 0.25 was chosen; this does not show general superiority over 0.35.

### Alias-level evaluation

| Metric | Reported value | Interpretation |
|---|---:|---|
| Retained concrete names/shared expressions | 162 / 165, 98.2% | Most labeled entries in this class survived |
| Blocked everyday expressions | 15 / 41, 36.6% | Most everyday expressions were still not blocked |
| Undetermined aliases | 99 / 305 | Ambiguous items are outside the above rates |

**98.2% is retention within one trigger class, not overall classification accuracy or useful-memory recall.**

One condition label remains unresolved. The old report called the candidate a “combined rule” while reporting 161 / 165 retained under the old rules and 162 / 165 under the new condition. Adding exclusions to a fixed sample cannot increase retention. These remain historical exploratory values, not the current combined rule's retention rate. A future check needs the same alias set and configuration, reporting old rules, the new condition alone, and their combination separately.

### Actual delivery replay

The same historical material supplied 1,891 utterances comparing the old rules with the added condition. “Delivery” means placing a past passage into model context, not necessarily mentioning it in a reply.

| Metric | Old rules | With added condition |
|---|---:|---:|
| Utterances receiving material | 100 | 64 |
| Deliveries judged useful | About 39, estimated in the old report | 34, all 64 judged |
| Deliveries judged useless | About 61, derived from the old estimate | 30, all labeled useless but harmless |
| Useful share of deliveries | About 39% | 34 / 64, 53.1% |

Differences: neither delivers for 1,790 utterances; the same passage is delivered for 63; old-only delivery for 37; new-only delivery for 1. Total: 1,891.

**Total deliveries fell by 36% in this replay. Based on the old estimate, useless deliveries fell from about 61 to 30, roughly half.** The latter comparison uses an estimated old baseline, not two fully relabeled groups.

There was a cost: of the 37 removed deliveries, the old report judged about 27 useless and 4–5 useful, without fully classifying the remainder. The one new delivery was still useless. No harmful item was labeled among the new combination's 64 deliveries, but this is limited to that material and annotation.

### Conclusions supported so far

- Clumping can reduce some useless automatic associations, while losing some useful entry points.
- It does not solve incremental usefulness in context and does not assess truth or retention value.
- Only automatic association is restricted. Transcripts and active search remain available, but preservation does not prove the model will search at the right time.
- Thresholds, segmentation, and labels came from the same data; they cannot yet be generalized to other users, language mixtures, or longer periods.

See [models and parameters](mechanisms.md) for formulas and implementation. Only aggregate numbers are public here, not private conversations, alias lists, or item-level analysis.
