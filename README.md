# Anomaly-Based Intrusion Detection System

This repository contains the R scripts and datasets necessary for anomaly detection in cyber-physical systems using Hidden Markov Models (HMMs). Our approach involves analyzing household electricity consumption data to detect anomalies, thereby enhancing the security of these systems.

## Research Report

A detailed research report is available in this repository that analyzes the results of the anomaly detection model. This report provides insights into the methodology, results, and implications of the findings.

## Getting Started

### Prerequisites

Ensure you have R installed on your machine along with the necessary packages used in the scripts. You can install R from [CRAN](https://cran.r-project.org/mirror-howto.html).

### Dataset

The dataset required for this project is included in the repository within a ZIP file named `dataset.zip`. Extract this file in a directory of your choice.

### Setting Up Your Workspace

1. **Set the Working Directory**: Before running the scripts, make sure to set the working directory to the location where you have your dataset. You can set this in each R script by modifying the `setwd()` function call at the beginning of the files.

    ```R
    setwd("path/to/your/dataset/directory")
    ```

    Replace `"path/to/your/dataset/directory"` with the path where you have extracted your dataset.

### Running the Scripts

Follow the steps below to execute the analysis:

1. **Feature Scaling**: Run the `featureScalling.R` script first. This script prepares the dataset by scaling the features, making them suitable for analysis.
   
    ```bash
    Rscript featureScalling.R
    ```

2. **HMM Training and Anomaly Detection**: After scaling the features, run the `HMMTrainingAnomalyDetection.R` script. This script will train the Hidden Markov Model and perform anomaly detection on the scaled data.
   
    ```bash
    Rscript HMMTrainingAnomalyDetection.R
    ```

## System Requirements

This software has been tested on the following system configurations:

- Operating System: Windows 10, Ubuntu 20.04
- R Version: 4.0.5
- RAM: 8GB minimum recommended

## Support

If you encounter any problems or have feedback, please open an issue in this repository.


