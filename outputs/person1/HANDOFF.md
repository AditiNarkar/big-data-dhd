# Person 1 handoff: data preparation and descriptive analysis

Run the reproducible pipeline from the project root:

```sh
Rscript scripts/01_prepare_data.R
```

The script uses base R and writes its derived files to `outputs/person1/`. It does not modify the source CSVs.

## Included files

- `analysis_person_level.csv`: one row per included person, with linked vehicle and crash context.
- `join_and_exclusion_audit.csv`: source/join row counts and exclusions.
- `injury_by_vehicle_group.csv`: person counts and serious/fatal percentages by group.
- `injury_by_vehicle_and_speed_zone.csv`: descriptive breakdown for later comparison.
- `injury_by_vehicle_group.png`: descriptive bar chart.

## Construction and definitions

- The observational unit is a **person involved in a crash**, keyed by `ACCIDENT_NO` and `PERSON_ID`.
- Person records are left joined to Vehicle on `ACCIDENT_NO` + `VEHICLE_ID`; Accident, weather, and road surface are joined on `ACCIDENT_NO`.
- Weather and surface tables can contain multiple rows for a crash. The script selects sequence 1; if it is absent, it falls back to sequence 0. This avoids multiplying person rows. Review this assumption against the dataset documentation before final submission.
- The binary outcome is 1 for `Serious injury` or `Fatality`, and 0 for `Other injury` or `Not injured`.
- Vehicle groups: **Car/light vehicle** = Car, Station Wagon, Utility, Panel Van, Taxi, and Light Commercial Vehicle <= 4.5 tonnes; **Motorcycle** = Motor Cycle, Motor Scooter, Moped; **Truck/heavy vehicle** = rigid trucks and prime movers; **Bus** = Bus/Coach and Mini Bus; **Other** = all remaining known vehicle descriptions. `Not Known` and `Not Applicable` are excluded from the comparison and counted in the audit.
- The analysis excludes people with no matched vehicle and people whose vehicle category is unknown/not applicable. It retains people in the remaining known groups, including “Other” vehicles.
- Missing predictor values are labelled `Unknown` in the analysis file. The source speed-zone codes (including 777, 888, and 999) are retained as character values; check the data dictionary and decide how to treat those codes before modelling.

## Results snapshot

The join retained all 425,352 source person rows without row multiplication and all had a matching accident record. 17,088 people had no matched vehicle; 20,669 rows were in an unknown/not-applicable vehicle group. The analysis dataset contains 404,683 people. No injury outcomes were unclassified.

| Vehicle group | People | Serious/fatal | Percent |
|---|---:|---:|---:|
| Car/light vehicle | 342,297 | 51,376 | 15.0% |
| Motorcycle | 26,941 | 12,537 | 46.5% |
| Truck/heavy vehicle | 9,016 | 722 | 8.0% |
| Bus | 4,923 | 310 | 6.3% |
| Other | 21,506 | 6,288 | 29.2% |

These are descriptive proportions among included people. They are not adjusted comparisons and should not be interpreted as causal effects. “Other” combines heterogeneous vehicle types; treat that percentage cautiously.

## Notes for Person 2

- Use the supplied analysis file as the starting point and review the grouping and code assumptions above before fitting the model.
- Because multiple people can belong to one crash, split training and test data by `ACCIDENT_NO`, not randomly by person, to avoid having people from the same crash in both sets.
- Consider whether `Unknown`/special speed-zone codes and sparse vehicle-by-zone groups need consolidation or exclusion; record the rule and show sample sizes.
- The report should state that comparisons are associations conditional on the measured variables, not evidence that vehicle type causes injury.
