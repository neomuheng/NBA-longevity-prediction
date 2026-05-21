# NBA Career Longevity Analysis

An R-based statistical analysis examining how **shooting efficiency (TS%)** and **usage rate (USG%)** predict NBA career length, using survival analysis and regression modeling on player data from 1976 to 2026.

---

## Project Overview

This project investigates whether more efficient or higher-volume players tend to have longer careers. It combines multiple player datasets, cleans and merges them, and fits a series of models — culminating in a Cox proportional hazards model with covariates for player quality, position, and debut age.

---

## Data

Five CSV files are required, placed in the working directory (`C:/Users/vipon/Downloads/Archive/` by default):

| File | Description |
|---|---|
| `Player Career Info.csv` | Career span, birth date, player ID |
| `Player Season Info.csv` | Season-level info (age, team, position) |
| `Player Totals.csv` | Per-season counting stats (pts, mp, g, etc.) |
| `Advanced.csv` | Advanced metrics (TS%, WS/48, BPM, USG%) |
| `Player Shooting.csv` | Detailed shooting splits |

Data is filtered to seasons **1976 and later**. Players are included in the final analysis if they have:
- Career length ≥ 3 seasons
- Average games played ≥ 40 per season
- Average minutes played ≥ 1,000 per season

---

## Dependencies

Install all required R packages before running:

```r
install.packages(c(
  "MASS", "tidyverse", "naniar", "scales",
  "car", "survival", "survminer", "gridExtra"
))
```

---

## Analysis Pipeline

### 1. Load & Merge Data
Raw CSVs are loaded and joined on `player_id` + `season`. Duplicate rows (players traded mid-season) are resolved by keeping the `TOT` (combined totals) row where available.

### 2. Missing Data Audit
A bar chart (`figure1_missing_data.png`) flags variables exceeding a **5% missingness threshold**, guiding imputation or exclusion decisions.

### 3. Player-Level Aggregation
Season-level records are collapsed to one row per player using career averages for all metrics.

### 4. Group Definitions

| Variable | Threshold | Groups |
|---|---|---|
| TS% | 53.9% | High Efficiency / Low Efficiency |
| USG% | 18.8% | High Volume / Low Volume |

Players still active in 2026 are treated as **right-censored** (`event = 0`) in survival models.

### 5. Models

| Model | Method | Key Output |
|---|---|---|
| Linear Regression | OLS | Baseline coefficients for TS% and USG% |
| Negative Binomial | GLM | Count-model alternative (AIC comparison) |
| Cox Basic | Survival | Hazard ratios for efficiency and volume |
| **Cox Enhanced** | **Survival** | **+ BPM, position, debut age (Concordance: 0.713)** |

### 6. Outputs

| File | Description |
|---|---|
| `figure1_missing_data.png` | Missing data bar chart |
| `linear_models.png` | Scatter plots: TS% and USG% vs. career length |
| `survival_efficiency_final.png` | Kaplan–Meier curve by TS% group (with p-value) |
| `survival_volume_final.png` | Kaplan–Meier curve by USG% group |

---

## Key Results

- **Model fit**: The enhanced Cox model achieves a concordance of **0.713**, outperforming the basic Cox (0.655) and linear regression.
- **Efficiency**: Higher TS% is associated with longer career survival, with the KM curves showing a statistically significant split between efficiency groups.
- **Volume**: USG% group differences are visualized but the p-value is suppressed in the final plot — interpret with caution.
- **Covariates**: BPM, position, and debut age all contribute meaningfully to the enhanced model.

---

## Notes

- The `select` function is explicitly namespaced to `dplyr::select` to avoid conflicts with the `MASS` package.
- `birth_date` is used to compute `debut_age`; ensure this column is parsed as a date type when loading `Player Career Info.csv`.
- The negative binomial model (`nb_model`) is referenced in the model comparison block but its fitting code should be added if not already present in your local script.
