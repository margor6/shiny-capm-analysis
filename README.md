# CAPM & Market Timing Analysis (Shiny App)

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-007BC2?style=for-the-badge&logo=rstudio&logoColor=white)
![Status](https://img.shields.io/badge/Status-Maintained-success)

## Overview

This repository contains an interactive **R Shiny dashboard** designed to test the validity of the classic **Capital Asset Pricing Model (CAPM)** against the nonlinear **Treynor-Mazuy model**.

The application allows users to fetch real-time financial data, estimate econometric models, and statistically verify whether a specific asset exhibits "market timing" characteristics (convexity/concavity in returns).

## Methodology

The app compares two regression models using **AIC (Akaike Information Criterion)** selection:

1.  **Linear CAPM (Model A):**
    $$y = \alpha + \beta x + \epsilon$$
    Assumes a constant systematic risk ($\beta$).

2.  **Treynor-Mazuy Test (Model B):**
    $$y = \alpha + \beta x + \gamma x^2 + \epsilon$$
    Adds a quadratic term. A statistically significant $\gamma$ implies nonlinear market response (market timing).

## Key Features

* **Dynamic Data Fetching:** Retrives adjusted closing prices from Yahoo Finance via `quantmod`.
* **Model Selection:** Automated comparison of Linear vs. Quadratic models based on AIC.
* **Visual Analysis:** Interactive scatter plots with fitted regression curves using `ggplot2`.
* **Diagnostics:** Residual analysis including Shapiro-Wilk test and Q-Q plots.

## Tech

* **Core:** R
* **UI/Server:** Shiny
* **Finance:** quantmod
* **Visualization:** ggplot2
* **Reporting:** knitr, R Markdown

## Installation & Usage

To run this application locally:

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/margor6/shiny-capm-analysis.git](https://github.com/margor6/shiny-capm-analysis.git)
    ```
2.  **Install packages** in R/RStudio:
    ```r
    install.packages(c("shiny", "quantmod", "ggplot2", "knitr"))
    ```
3.  **Run the App:**
    Open `ui.R` or `server.R` in RStudio and click **Run App**, or run:
    ```r
    library(shiny)
    runApp()
    ```

## Project Structure

* `ui.R` - User Interface definition.
* `server.R` - Backend logic and computations.
* `presentation.html` - **Detailed financial analysis and case study results.**
* `presentation.Rmd` - Source code for the presentation.

---
**Author:** Marcin Górski
