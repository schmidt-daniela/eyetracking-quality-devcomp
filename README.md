# Eye-Tracking Data Quality in Developmental & Comparative Research 👶🐒

[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Project-blue)](https://osf.io/u3p8d/overview?view_only=a3bcb225f5b5497284727255a81095b6)
[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Preregistration_I-blue)](https://osf.io/8zer2/overview?view_only=4c2ca4e770554217953b81c7f5efabd9)
[![Blinded OSF Project](https://img.shields.io/badge/OSF-Blinded_Preregistration_II-blue)](https://osf.io/xf6d4/overview?view_only=e956646f9cbe43039386e7d85cbbf13c)

> ⚠️ **Status:** This project is currently a work in progress.

## 📖 About The Project

This repository contains the analysis pipeline and preprocessed data for the upcoming paper:  
**[blinded for review].**

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
│   ├── R/, data/, doc/, img/
│   └── exp3_*.R            # Numbered analysis scripts
├── renv/                   # R environment configuration folder
├── .gitignore              # Untracked files to ignore
├── .Rprofile               # R project profile
├── eyetracking-quality-devcomp.Rproj # RStudio project file
├── LICENSE                 # MIT License
├── README.md               # Project documentation
└── renv.lock               # Exact R package versions for reproducibility
```

## 🚀 Getting Started

### Requirements

This project was developed using **R 4.5.1** and uses **renv** for dependency management.

After cloning the repository, restore the project environment:

```r
renv::restore()
```

### Reproducing the Main Analyses

The repository contains preprocessed datasets that allow users to reproduce the inferential analyses reported in the manuscript without starting from the raw eye-tracking data.

To reproduce the statistical analyses, run the corresponding analysis scripts within each experiment folder.

### Reproducing the Complete Data Processing Pipeline

To reproduce all preprocessing and analysis steps from the raw eye-tracking data, first download the raw datasets from the linked OSF project.

The downloaded files must be placed into the appropriate `data` directories before running the analysis pipeline.

#### Experiment 1

Required folder structure:

```text
data/
├── raw_1/
│   ├── 4m/
│   ├── 6m/
│   ├── 9m/
│   ├── 18m/
│   ├── adults/
│   └── chimps/
├── raw_2/
│   ├── 4m/
│   ├── 6m/
│   ├── 9m/
│   ├── 18m/
│   ├── adults/
│   └── chimps/
├── raw_clean/
│   ├── 4m/
│   ├── 6m/
│   ├── 9m/
│   ├── 18m/
│   ├── adults/
│   └── chimps/
├── raw_clean_blink/
│   ├── 4m/
│   ├── 6m/
│   ├── 9m/
│   ├── 18m/
│   ├── adults/
│   └── chimps/
└── preproc/
│   ├── 4m/
│   ├── 6m/
│   ├── 9m/
│   ├── 18m/
│   ├── adults/
│   └── chimps/
```

#### Experiment 2

Required folder structure:

```text
data/
├── raw_1/
│   ├── ape_calibration_2p/
│   ├── human_calibration_9p/
│   └── alex_calibration_5p/
├── raw_2/
│   ├── ape_calibration_2p/
│   ├── human_calibration_9p/
│   └── alex_calibration_5p/
├── raw_clean/
│   ├── ape_calibration_2p/
│   ├── human_calibration_9p/
│   └── alex_calibration_5p/
├── raw_clean_blink/
│   ├── ape_calibration_2p/
│   ├── human_calibration_9p/
│   └── alex_calibration_5p/
└── preproc/
    ├── ape_calibration_2p/
    ├── human_calibration_9p/
    └── alex_calibration_5p/
```

#### Experiment 3

Required folder structure:

```text
data/
├── raw_1/
│   ├── 4mo/
│   └── 6to18mo/
├── raw_included/
│   ├── 4mo/
│   └── 6to18mo/
├── raw_2/
│   ├── 4mo/
│   └── 6to18mo/
├── raw_clean/
│   ├── 4mo/
│   └── 6to18mo/
├── raw_clean_blink/
│   ├── 4mo/
│   └── 6to18mo/
└── preproc/
    ├── 4mo/
    └── 6to18mo/
```

### Running the Pipeline

Within each experiment, scripts should be executed sequentially according to their numbering:

```text
0_*   Initial preprocessing / data preparation
1_*   Data cleaning
2_*   Blink detection
3_*   Variable computation
```

For example:

```text
exp1_human_0_anonymize_data.R
exp1_human_1_clean_data.R
exp1_human_2_detect_blinks.R
exp1_human_3_compute_variables.R
```

The same principle applies to all experiment-specific pipelines. 

## 📊 Data Dictionary (`df_tot`)

The following table provides an overview of the variables included in the `df_tot` dataframe (generated in the `exp1_tot_infstats` R-script):

| Variable | Description |
| :--- | :--- |
| `group_id` | Participant ID and group (4-, 6-, 9-, 18-month-olds, adults, or chimpanzees). |
| `stimulus` | Name of the shown stimulus (checkflake = popflake, at = pinwheel, or object). |
| `position` | Stimulus position on screen (7 levels, e.g., center, top_left). |
| `accuracy` / `acc_visd` | Eye-tracking data quality metric: Accuracy in pixels / visual degrees. |
| `precrms` / `precrms_visd` | Eye-tracking data quality metric: Precision (RMS) in pixels / visual degrees. |
| `precsd` / `precsd_visd` | Eye-tracking data quality metric: Precision (SD) in pixels / visual degrees. |
| `robustness_ms` / `robustness_prop`| Eye-tracking data quality metric: Preregistered robustness per trial in ms / as a proportion. |
| `robustness_ms_2` | Eye-tracking data quality metric: Robustness in ms accounting for deviations from preregistration (see paper for details). |
| `robustness_prop_2` | `robustness_ms_2` Eye-tracking data quality metric: As above, but relative to total experiment duration. |
| `mean_fixation_duration` | Eye-tracking outcome: Mean duration of fixations. |
| `mean_fixation_number` | Eye-tracking outcome: Total number of fixations. |
| `latencies` | Eye-tracking outcome: Time between object onset and first gaze (object-trials only; humans only). |
| `congruence` | Stimulus details relevant for latency calculation. |
| `rel_gaze_in_aoi` / `rel_fix_in_aoi` | Eye-tracking outcome: Relative gaze / fixation time within Areas of Interest (AOIs). |
| `abs_gaze_in_aoi_duration` | Eye-tracking outcome: Absolute gaze duration within AOIs (ms). |
| `abs_gaze_out_aoi_duration` | Eye-tracking outcome: Absolute gaze duration outside AOIs (ms). |
| `abs_gaze_recorded_duration` | Eye-tracking outcome: Total recorded gaze duration (inside + outside AOIs). |
| `abs_fix_in_aoi_duration` | Eye-tracking outcome: Absolute fixation duration within AOIs (ms). |
| `abs_fix_out_aoi_duration` | Eye-tracking outcome: Absolute fixation duration outside AOIs (ms). |
| `abs_fix_recorded_duration` | Eye-tracking outcome: Total recorded fixation duration (inside + outside AOIs). |
| `excluded_100ms` | Inclusion of trial based on the < 100ms latency criterion. |
| `excluded_3sd` | Inclusion of trial based on the ± 3 SD latency deviation criterion. |
| `excluded_fixation` | Inclusion of trial based on missing fixation within the AOI + buffer. |
| `sex` | Participant sex (m/f). |
| `age` | Participant age (in days for infants, years for adults; humans only). |
| `order` | Counterbalancing information (humans only). |
| `experimenter` | Experimenter ID (humans only). |
| `no_siblings` | Number of siblings (human infants only). |
| `no_household` | Number of people living in the household (human infants only). |
| `multilingual` | Indicator for bilingual upbringing (human infants only). |
| `kindergarten_yn` | Indicator for kindergarten attendance (human infants only). |
| `tagesmutter_yn` | Indicator for childminder (*Tagesmutter*) attendance (human infants only). |
| `source_file` | Original filename. |
| `folder` | Subgroup folder (equals group: 4-, 6-, 9-, 18-month-olds, adults, or chimpanzees). |
| `session_trial` | Combined session and trial identifier (chimpanzees only). |
| `session` / `trial` | Session number (chimpanzees only) / Trial number. |
| `age_y` / `age_classification`| Age in years / Age group (adult/non-adult; chimpanzees only). |
| `species_group` | A or B group (chimpanzees only). |
| `time` | Trials (humans). |
| `time_1` | Trials (humans) & session timeline unit (chimpanzees). |
| `time_2` | Trials (humans) & trial within session (chimpanzees). |
| `time_3` | Trials (humans) & testing day sequence (chimpanzees). |
| `date` / `day` | Testing date / Testing day sequence (chimpanzees only). |

