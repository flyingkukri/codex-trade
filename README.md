# Germany Trade Security Dashboard (R Shiny)

This dashboard compares **Germany** with selected partner countries and computes a trade security index using:

- **V-Dem democracy index** (partner democracy values by year)
- **CEPII BACI trade data** (Germany's exports by partner, product section, and year)

## Index logic implemented

For each selected year and product section:

1. Compute each partner's trade share in Germany's exports within that product section.
2. Multiply that share by the partner's democracy index.
3. Sum those weighted components to get Germany's country risk/security index.

## Run

```r
shiny::runApp()
```

Or from shell:

```bash
Rscript -e "shiny::runApp()"
```

## BACI CSV input schema

Upload a CSV with these columns:

- `exporter_iso3` (e.g., `DEU`)
- `importer_iso3` (ISO3 partner)
- `year` (integer)
- `product_section` (string)
- `trade_value` (numeric)

If no file is uploaded, the app uses built-in demo trade data so you can explore the UI immediately.
