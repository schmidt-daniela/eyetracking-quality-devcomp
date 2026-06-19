# Eye-Tracking Data Quality in Developmental & Comparative Research 👶🐒

[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Project-blue)](https://osf.io/u3p8d/overview?view_only=a3bcb225f5b5497284727255a81095b6)
[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Preregistration_I-blue)](https://osf.io/8zer2/overview?view_only=4c2ca4e770554217953b81c7f5efabd9)
[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Preregistration_II-blue)](https://osf.io/xf6d4/overview?view_only=e956646f9cbe43039386e7d85cbbf13c)

> ⚠️ **Status:** This project is currently a work in progress.

## 📖 About The Project

This repository contains the analysis pipeline and preprocessed data for the upcoming paper:  
**Schmidt, Visser, Maurits & Thiele et al. (in prep).**

In the spirit of **Open Science**, the primary goal of this repository is to make our analysis fully transparent and reproducible. The project investigates eye-tracking data quality across different human (4-, 6-, 9-, 18-month-olds, and adults) and non-human (chimpanzees) samplesthrough three distinct experiments:
* **Experiment 1:** Includes chimpanzee (`chimp`) and human (`human`) samples.
RQ1: (How) Does Eye-Tracking Data Quality Vary Within and Between Groups?
RQ2: (How) Does Eye-Tracking Data Quality Change Over Time?
RQ3: Are Eye-Tracking Outcomes Confounded With Data Quality?
* **Experiment 2:** Includes chimpanzee (`chimp`) sample only.
RQ4: Can Modifications in the Calibration Procedure Improve Eye-Tracking Data Quality in Chimpanzees?
* **Experiment 3:** Includes infant (`infant`) samples only.
RQ5: Can Modifications in the Calibration Procedure Improve Eye-Tracking Data Quality in Human Infants?

## 📂 Repository Structure
The repository is structured by experiment. Each experiment folder (`exp1`, `exp2`, `exp3`) acts as a self-contained environment containing its respective scripts, helper functions, data, documentation, and plots.

```text
eyetracking-quality-devcomp/
├── exp1/                   # Experiment 1 (Chimpanzees & Humans)
│   ├── R/                  # Helper functions
│   ├── data/preproc/       # Processed data (Raw data is hosted on OSF)
│   ├── doc/                # Documents with participant informations
│   ├── img/                # Generated plots
│   └── exp1_*.R            # Numbered analysis scripts
├── exp2/                   # Experiment 2 (Chimpanzees)
│   ├── R/, data/, doc/, img/ 
│   └── exp2_*.R            # Numbered analysis scripts
├── exp3/                   # Experiment 3 (Infants)
│   ├── R/
│   └── exp3_*.R            # Numbered analysis scripts
├── renv/                   # R environment configuration folder
├── .gitignore              # Untracked files to ignore
├── .Rprofile               # R project profile
├── eyetracking-quality-devcomp.Rproj # RStudio project file
├── LICENSE                 # MIT License
├── README.md               # Project documentation
└── renv.lock               # Exact R package versions for reproducibility


