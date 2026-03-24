# Changelog

## [Unreleased] — 2025-07-21

### Added
- **Xa prediction data integration**: Loaded ensemble-learning predictions (`PathoTracer_Xaprediction.csv`, `PathoTracer_Xaprediction_year.csv`) to replace the empirical `com_xa3` effectiveness estimates.
- **Per Region tab**: New top-level tab under Bacterial Blight → Disease Maps with six sub-tabs:
  - Map — leaflet map showing all isolates in the selected region.
  - Axoo Summary — bar charts of pathogen population distribution by country within the region.
  - Effective Rgenes — Highcharts column chart with 95% CI error bars for region-level gene effectiveness.
  - Varieties with Rgenes — filterable data table of varieties carrying selected resistance genes.
  - Genotyped Samples — data table of all genotyped isolates in the region.
  - Rgene Trends — Highcharts line chart with area-range 95% CI bands showing effectiveness over time.
- **Per Country → Rgene Trends tab**: New 6th sub-tab showing country-level Xa gene effectiveness trends over time with CI bands.
- **Cluster markers checkbox**: Added `checkboxInput("cluster_country")` and `checkboxInput("cluster_region")` to toggle marker clustering on Per Country and Per Region maps. When unclustering, jitter is increased to spread overlapping points.
- **Region-level aggregations**: `country_region_lookup`, `region_effectiveness`, and `region_effectiveness_year` data frames computed from `xa_prediction` for the Per Region tab.

### Changed
- **Country effectiveness**: Replaced empirical `com_xa3_data` aggregation with predicted effectiveness from the ensemble model, including confidence intervals (`ci_lower`, `ci_upper`).
- **Effective Rgenes chart (Per Country)**: Updated Highcharts column chart to show 95% CI error bars and predicted effectiveness percentages; tooltip now displays CI range.
- **Effective Rgenes info text**: Updated methodology description to reflect ensemble learning approach (random forest, XGBoost, neural networks stacked with Firth logistic regression).
- **Varieties with Rgenes info text**: Removed wrapping `info-box` div; info section now uses standalone `info-content` class for cleaner styling.
- **Per Country sidebar**: Added `selected` default to country `selectInput`; added cluster markers checkbox.
- **Per Country selectInput choices**: Sorted gene choices alphabetically.
- **CSS stylesheet reference**: Fixed `style.css` → `custom.css` in the HTML head.
- **custom.css**: Updated to Bootstrap 5 navbar selectors (`.nav-link`, `.dropdown-item`), added Google Fonts `@import`, added `.info-box`, `.info-content`, `.contact-card`, `.btn-view-chart`, `.card-custom` styles, and updated font families to use Merriweather consistently.

### Removed
- Dead `getClusterIcon()` function that was unused.
- Duplicate `rice_data$AxooPopn <- as.character(rice_data$AxooPopn)` line.
- Commented-out code blocks (selected genes limiter, test tab).

### Fixed
- Per Country map: removed stray `print(map_data)` debug statement.
