# Preprocessing review

## Analysis and reviewer

- Analysis identifier:
- Commit or archive checksum:
- Reviewer:
- Review date:
- Review status: `not_checked` / `author_reviewed` / `independently_reviewed`

Keep the status as `not_checked` until the checks below have been completed and an artifact records the review.

## Data roles

- Outcome:
- Model features:
- Audit-only variables:
- Setting/cluster variables:
- Sampling weights:
- Variables explicitly excluded from the model:

## Split integrity

- [ ] All rows from a cluster remain on one side of each train/assessment split.
- [ ] Outcome information from assessment rows does not enter preprocessing.
- [ ] Audit-only variables are not included as model features.
- [ ] Outcome realizations used for sensitivity analyses share documented,
      aligned splits where required.

## Fold-contained operations

For each learned operation, record where it is fitted and applied.

| Operation | Fitted on training rows only? | Object/function | Evidence |
|---|---:|---|---|
| Missing-data imputation | | | |
| Encoding/contrasts | | | |
| Scaling/normalization | | | |
| Feature filtering | | | |
| Feature selection | | | |
| Hyperparameter tuning | | | |
| Probability calibration | | | |
| Threshold selection | | | |

## Verification findings

- Leakage risks considered:
- Confirmed safeguards:
- Deviations:
- Remaining limitations:
- Files/lines reviewed:
