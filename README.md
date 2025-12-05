# Hong Kong Wage Distribution Analysis: Education vs Finance

[![Course](https://img.shields.io/badge/ECO4203-Applied%20Econometrics-blue)](https://example.university.edu/eco4203) [![Data Source](https://img.shields.io/badge/Source-Census%20HK%202024-green)](https://www.censtatd.gov.hk/)

## Project Overview

This repository contains the complete analysis for the **ECO4203 Applied Econometrics** group project: *Wage Distribution Analysis - Education vs Finance Industries by Hong Kong District*. 

The study analyzes median wage data from the **Hong Kong Census and Statistics Department (May-June 2024)** across Hong Kong's 18 districts, focusing on **Education/Public Administration** and **Financing/Insurance** sectors (both HK$32,800 median monthly wage). Key objectives:

- Quantify **CBD wage premium** (Central/Western, Wan Chai, Eastern: ~8.1%, p<0.01) [file:2]
- Compare wage spreads (Finance: HK$400 higher; CBD: HK$1,600 higher spread) [file:2]
- Test location-industry interactions via OLS models with heteroskedasticity-robust SEs [file:1]
- Avoid circular high/low industry classification fallacy [file:2]

**Key Findings:**
- Geographic centrality dominates industry effects
- Wage inequality amplifies in CBD Finance (HK$5,200 spread vs HK$3,200 non-CBD Education) [file:2]
- Models explain 82% wage variation (R²=0.824) [file:1][file:2]

## Files

| File | Description | Outputs Generated |
|------|-------------|-------------------|
| `HK_Wage_Analysis.R` | Full R script: data prep, 4 OLS models, diagnostics (BP, VIF, Shapiro-Wilk), robust SEs, 7 visualizations, LaTeX tables [file:1] | PDFs (residuals, boxplots, interaction, corr matrix, model comparison), `econometricsresults.RDS` |
| `ECO4203_FinalReport.pdf` | 30+ page report: methodology, regressions, diagnostics, policy implications [file:2] | Executive summary, tables, heatmaps, interpretations |

## Methodology

1. **Data**: 2024 median wages/employment by industry-district [file:1][file:2]
2. **Prep**: District wage effects (CBD: +8%, Core: +4%); log wages [file:1]
3. **Models**:
   | Model | Specification | R² | Key Insight |
   |-------|---------------|----|-------------|
   | M1 | log(wage) = β₀ + β₁Finance + β₂CBD | 0.824 | CBD: 7.8% premium [file:2] |
   | M2 | + Finance×CBD | 0.824 | No interaction [file:2] |
   | M3 | Wage spread ~ Finance + CBD | 0.809 | Finance +12.5% spread [file:2] |
   | M4 | Full: interactions + log(employment) [file:1] | Varies | Best fit per AIC/BIC [file:1] |
4. **Diagnostics**: Heteroskedasticity (BP p<0.001 → HC1 SEs), no multicollinearity (VIF<2) [file:1][file:2]

## Setup & Reproduction
Install packages (run once)
install.packages(c("tidyverse", "lmtest", "car", "sandwich", "stargazer", "ggplot2", "corrplot"))

## Run analysis
source("HK_Wage_Analysis.R")


**Requirements**: R 4.0+, listed packages. Generates PDFs and RDS in working directory. [file:1]

## Results Summary

- **CBD Premium**: 8.08% across industries (robust t=8.67, p<10⁻¹⁰) [file:2]
- **Industry Effect**: Insignificant (β=0, p=1.0) [file:2]
- **Inequality**: CBD amplifies spreads by 50% [file:2]
- **Policy**: Target peripheral talent retention, location-adjusted pay [file:2]

## Authors

- **Alima Nur BEGIMBAEVA** (4072257)
- **Danil TOMILOV** (4188862)

**Instructor**: CHEN Yiting, ECO4203 Applied Econometrics [file:2]

## License

MIT License - free for academic/research use. Cite original data source. [file:2]

## Citations

1. Hong Kong Census and Statistics Department (2024). *Annual Earnings and Hours Survey*. [file:2]
2. White, H. (1980). Heteroskedasticity-Consistent Covariance. *Econometrica*. [file:2]
