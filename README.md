# k-NN Workshop Homework

## Project Overview

This repository contains the implementation of k-Nearest Neighbors (k-NN) regression using R and Rcpp. It also includes a Shiny application for interactive exploration of k-NN predictions and a benchmarking script to compare performance between pure R and Rcpp implementations.

The project is structured to support:

- S3 k-NN class with formula interface
- Multiple distance metrics (L1 and L2)
- Dynamic predictor selection in Shiny
- Downloading predictions from Shiny
- Benchmarking different sample sizes and feature counts

---

## Project Structure

```
knn_workshop_hw/
├── R/
│   ├── Exc1-knn-formula3.R    # S3 formula-interface implementation
│   ├── Exc1-knn.R             # S3 class and methods
│   └── exc3.R                 # Benchmarking script
├── app/
│   └── app.R                  # Shiny application
├── src/
│   └── knn_pred.cpp           # Rcpp implementation of k-NN
├── exc3_pdf.pdf               # One-page summary report
└── README.md                  # Project overview and instructions
```

---

## Getting Started

### Prerequisites

Install the necessary R packages:

```r
install.packages(c("shiny", "Rcpp", "devtools", "microbenchmark", "ggplot2"))
```

### Running the Shiny App

To run the interactive k-NN app:

```r
shiny::runApp("app")
```

- Select the number of neighbors (`k`)
- Choose the compute backend: `"R"` or `"cpp"`
- Select predictors dynamically
- Download predicted values as CSV

### Running Benchmarks

The benchmarking script compares the runtime of R vs. Rcpp implementations across different sample sizes and feature counts:

```r
Rscript R/exc3.R
```

Output:

- `benchmark_results.csv`: Raw benchmark data
- `benchmark_plot.png`: Visual comparison of runtimes

---

## PDF Report

A one-page summary of the benchmarks and key findings is included in the PDF file:

```
exc3_pdf.pdf
```

---

## Notes

- S3 methods included: `print()`, `summary()`, `fitted()`, `predict()`, `anova()`
- Distance metrics supported: `"sse"` (L2) and `"sad"` (L1)
- The Shiny app supports dynamic predictor selection and downloading of predictions
