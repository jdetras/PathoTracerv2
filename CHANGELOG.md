# Changelog

All notable changes to PathoTracer v2 are documented here.

---

## [Unreleased] — 2026-03-24

### Added
- **Per Region tab expansion** — Mirrored the Per Country tab with 6 sub-tabs: Map, Axoo Summary, Effective Rgenes, Varieties with Rgenes, Genotyped Samples, and Rgene Trends
- **Cluster/uncluster toggle** — Checkbox on both Per Country and Per Region map sidebars to enable or disable marker clustering
- **Smart jitter** — When clustering is off, markers jitter by ~0.05 degrees (~5 km) to spread co-located points; when clustering is on, jitter is minimal (0.00002) to preserve accuracy
- **"No data available" messaging** — Trend charts (Per Country and Per Region) display an informative subtitle when no data exists for the selected filter combination
- **CSS utility classes** — Added `.about-section-header`, `.info-box`, `.info-content`, `.contact-card`, `.btn-view-chart`, and `.card-custom` to `www/custom.css`

### Fixed
- **CSS not loading** — Corrected `style.css` reference to the actual file `custom.css` in `tags$head()`
- **Google Fonts not loading** — Added `@import url(...)` for Poppins, Roboto, Merriweather, Roboto Slab, and Lora at the top of `custom.css`
- **Navbar styling conflict** — Removed Bootstrap 3 navbar CSS rules that conflicted with the bslib Bootstrap 5 theme
- **Font override conflict** — Removed `font-family: 'Poppins'` from the `body` rule; Merriweather is now correctly set via `bs_theme(base_font = font_google("Merriweather"))`
- **White text in info boxes** — Unwrapped `.info-content` from `.info-box` parent in Effective Rgenes and Varieties tabs (both Per Country and Per Region) to prevent `color: white` from cascading; added explicit `color: #000000`
- **Missing top border on info boxes** — Added `!important` to `.info-content` border declaration to ensure all four sides render against Bootstrap overrides

### Changed
- Banner header and text fonts standardized to Merriweather
- Navbar font set to Merriweather, 1rem, bold (700)
- `.about-section-header` and `.banner-text` centered with `text-align: center`
- `.info-content` updated to standalone style: full `border-radius: 4px`, `margin-top: 15px`, and explicit black text color

### Removed
- Explanatory info-box text below Rgene Trends charts in both Per Country and Per Region tabs
