# PathoTracer v2
An informed-decision platform to reduce the risk of rice diseases

PathoTracer is a decision support system that integrates early-season pathogen diagnostics and disease resistance profiles intended for use by public and private enterprises in accurately defining breeding priorities and in implementing coordinated actions to manage crop diseases in real-time.

---

## Features

- **Overview** — Global map of rice bacterial blight isolate distribution with country-level pathogen population summaries
- **Per Country** — Country-level drill-down with isolate map, Axoo population summary, effective resistance gene predictions, variety recommendations, genotyped samples, and Rgene trends over time
- **Per Region** — Regional aggregation mirroring Per Country, with weighted gene effectiveness across countries within a region
- **Gene effectiveness predictions** — Ensemble learning model (random forest, XGBoost, deep learning, mixture of experts stacked via Firth logistic regression) predicting probabilities for seven Xa resistance genes per Xoo isolate
- **Interactive maps** — Leaflet maps with optional marker clustering and smart jitter for co-located points

## Tech Stack

| Component | Technology |
|-----------|-----------|
| App framework | R Shiny with bslib (Bootstrap 5) |
| Interactive charts | Highcharter |
| Maps | Leaflet |
| Data tables | DT |
| Styling | Custom CSS (`www/custom.css`) + Google Fonts |
| Theme | `bs_theme(primary = "#00AF4F", base_font = font_google("Merriweather"))` |

## Setup

1. Clone the repository
2. Install required R packages:
   ```r
   install.packages(c(
     "shiny", "bslib", "leaflet", "leaflet.extras",
     "highcharter", "DT", "dplyr", "ggplot2",
     "sf", "tidyr", "jsonlite"
   ))
   ```
3. Run the app:
   ```r
   shiny::runApp()
   ```

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for a full history of changes.

## License

© International Rice Research Institute (IRRI). All rights reserved.

