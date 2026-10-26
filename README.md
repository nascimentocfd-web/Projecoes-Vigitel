[README.md](https://github.com/user-attachments/files/32634959/README.md)
# Predictive models for health risk behaviors and chronic disease prevalence in a Brazilian telephone-based population survey: Does one model fit all?

Code and aggregated data for the article:

> Nascimento CF, de Paula TI, Freitas PC, Gouvêa ECDP, Martinez OGE, Menezes-Júnior LAA, et al. Predictive models for health risk behaviors and chronic disease prevalence in a Brazilian telephone-based population survey: Does one model fit all?

This repository contains the R pipeline used to estimate annual prevalences of seven indicators from the Brazilian Vigitel survey (2006–2024), analyze temporal trends, fit five forecasting models (linear regression, Prais-Winsten, ARIMA, Holt's method and Prophet), and compare their predictive performance in 77 time series.

## Overview of the analysis

1. **Prevalences.** Annual weighted prevalences (with 95% CI) of obesity, smoking, leisure-time physical activity, recommended fruit and vegetable intake, heavy episodic drinking, self-reported hypertension and self-reported diabetes, for adults in the Brazilian state capitals and Federal District, overall and by region, sex and age group (7 indicators × 11 groups = 77 series). Subgroups are estimated as domains of the survey design. No data were collected in 2022; values for that year are linearly interpolated (mean of 2021 and 2023).
2. **Trends.** Annual percent change (APC) estimated by Prais-Winsten regression of the log10-transformed prevalence, 2006–2024.
3. **Forecasts.** Models fitted to data up to 2019 and used to project 2020–2024.
4. **Performance.** RMSE computed for the observed years of the test period (2020, 2021, 2023 and 2024; the interpolated 2022 value is excluded), coverage of 95% prediction intervals, Friedman and Nemenyi tests (Holm adjustment for the indicator-level tests), and a sensitivity analysis of the frequency of best performance.

## Repository structure

```
├── 00_config.R                  # packages, paths, parameters, indicator definitions, helper functions
├── 01_prevalencias.R            # reads the microdata and estimates annual prevalences (77 series)
├── 02_tendencia.R               # Prais-Winsten trends and APC (Table 1)
├── 03_predicoes.R               # fits the five models and projects 2020–2024
├── 04_metricas.R                # RMSE, coverage, ranks, Friedman/Nemenyi, sensitivity analysis
├── 05_tabelas.R                 # article and supplementary tables (Word)
├── 06_graficos.R                # figures (PNG and TIFF, 300 dpi)
├── 07_especificacoes_modelos.R  # model specifications selected in each series (S2 Table)
├── run_all.R                    # runs the whole pipeline in order
└── saidas/                      # outputs (aggregated data, tables, figures)
```

Comments inside the scripts are written in Portuguese.

## Data

### Vigitel microdata (not included)

The individual-level microdata are publicly available from the Brazilian Ministry of Health and are **not** redistributed here:

- Vigitel harmonized database 2006–2024 (CSV), data dictionary and analysis guidelines: https://svs.aids.gov.br/daent/cgdnt/vigitel/
- Version used in this study: harmonized database published in August 2026, accessed on September 2026.

Download and unzip the CSV file into the project folder. The pipeline uses the weighting variable `pesorake2025` and the indicator variables `obesid_i`, `fumante`, `ativo_livre`, `flvreco`, `alcabu`, `hart` and `diab`. It reads both the current unlabeled version of the database (numeric codes) and earlier labeled versions.

### Aggregated data (included)

The folder `saidas/` contains all aggregated results needed to reproduce the tables and figures without the microdata:

| File | Content |
|---|---|
| `prevalencias.csv` | Annual prevalence, standard error and 95% CI for the 77 series (2022 flagged as interpolated) |
| `tendencia.csv` | Prais-Winsten coefficients, APC, 95% CI and trend classification |
| `predicoes.csv` | Projections and 95% prediction intervals of each model for 2020–2024, with observed values |
| `metricas.csv` | RMSE, prediction-interval coverage and ranks of each model in each series |
| `especificacoes_modelos.csv` | ARIMA orders, Holt parameters, Prais-Winsten ρ and Prophet changepoints |
| `ranking_medio_por_indicador.csv` | Mean rank and frequency of best performance by indicator |
| `margens_melhor_modelo.csv` | Difference in RMSE between the first and second models in each series |
| `testes_friedman_nemenyi.txt` | Output of the Friedman and Nemenyi tests |


## Requirements

- R 4.6.1
- Main packages (versions used): survey 4.5, forecast 9.0.2, prophet 1.1.7, prais 1.1.4, ggplot2 4.0.3
- Other packages: data.table, dplyr, tidyr, purrr, flextable, officer, PMCMRplus

Missing packages are installed automatically by `00_config.R`. The exact versions of all packages are recorded in `saidas/versoes_pacotes.txt`.

## How to run

1. Place all scripts and the Vigitel CSV file in the same folder and open it as an RStudio project (or set it as the working directory).
2. If the CSV file has a different name, edit `ARQ_MICRODADOS` in `00_config.R`. If the weights are read as text, set `DEC_CSV <- ","`.
3. Run the full pipeline:

```r
source("run_all.R")
```

Only `01_prevalencias.R` uses the microdata; it is the slowest step. All subsequent scripts read the saved results in `saidas/` and can be run individually, for example:

```r
source("04_metricas.R", encoding = "UTF-8")
```

To reproduce the analyses from the aggregated data only, without downloading the microdata, skip `01_prevalencias.R` and run scripts `02` to `07`. They read the `.rds` versions of the aggregated files, which are included in `saidas/` alongside the `.csv` files.

## Correspondence between outputs and the article

| Output | Article |
|---|---|
| `saidas/tabelas_artigo.docx` | Table 1 and Table 2 |
| `saidas/figuras/fig_sintese_ranking_modelos` | Fig 1 |
| `saidas/figuras/fig_artigo_1_total_indicadores` | Fig 2 |
| `saidas/figuras/fig_01` to `fig_14` | S1–S14 Figs |
| `saidas/especificacoes_modelos.csv` | S2 Table |
| `saidas/tabelas_suplementares.docx` | S3 and S4 Tables |

## License

- Code: MIT License
- Aggregated data and outputs: Creative Commons Attribution 4.0 International (CC BY 4.0)

## Contact

Carla Ferreira do Nascimento – Department of Physical Therapy, Institute of Multidisciplinary Rehabilitation and Health, Federal University of Bahia, Brazil – e-mail: nascimento.cfd@ufba.br
