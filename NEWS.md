# kurszuweisung 1.1.0

## New features
- Added gender and class balancing optimization constraints. Users can now ensure a more diverse distribution of students across courses via optional sliders and weightings.
- Added demographic visualizations in the dashboard, including satisfaction split by gender and course occupancy with m/w ratios.
- Native support for "Klasse" or "class" columns in input data.

## Bug fixes
- Fixed a bug where missing gender data would crash the dashboard evaluation. It now defaults to "k.A." gracefully.
- Fixed a syntax error in `server.R` involving orphaned code blocks.
- Fixed the Course Config table being empty in the UI.

## Refactoring
- Centralized Shiny reactivity: The dashboard evaluation is now a single reactive expression, improving app performance.
- Out-sourced visualization logic: Plotting functions moved from `server.R` to `R/plots.R` for better maintainability and reusability.
- Consolidated utilities: Shared operators and constants moved to `R/utils.R`.

## Data cleanup
- Removed legacy demo CSV files (`inst/extdata/*.csv`).
- Removed unused `verify_balance.R` script as requested.
