# GC Flux Processing Pipeline (Peak Heights → ppm)

This repository contains an end-to-end workflow for processing Shimadzu LabSolutions GC `.txt` exports into tidy peak-height tables and calibrated gas concentrations (ppm). The pipeline is designed to be **batch-run across many GC run folders**, with **robust handling of inconsistencies in sample logs** and **QC-driven exclusion of bad standard injections**.

---

## What the pipeline produces

For each GC run (date folder), outputs are written to:

`flux-processing/data/tidy/<run_label>/`

Where `<run_label>` is `YYMMDD` parsed from the run folder name (e.g., `230718`).

### Key outputs (per run)
1. `*_gc_peak_heights.csv`  
   - Tidy table of extracted peak heights (CO2/CH4/N2O) joined to metadata from `sample_log.csv`.

2. `*_gc_standards.csv`  
   - Standards-only subset (based on STD detection rules).

3. `*_gc_standards_qc.csv`  
   - Standards with low-peak flags (threshold QC).

4. `*_std_heights.csv`  
   - Per-run-date standard means, precision metrics, counts used vs. dropped, and a `qc_flag`.

5. `*_ghg_ppm.csv`  
   - Non-standard samples with concentrations calculated using same-day standard means.

---

## Key recent changes (important)

### 1) More robust filename matching
Many failures were caused by mismatched filenames between `.txt` outputs and `sample_log.csv` (e.g., `08182023_001` vs `081823_001`).

The pipeline now normalizes filenames using `normalize_gc_filename()` to enforce a canonical form:
- `MMDDYY_###` (e.g., `081823_001`)
- Handles common variants such as:
  - `8/18/2023_001`
  - `08-18-23_001`
  - `08182023_001` (requires conversion to 6-digit date tokens)

### 2) Date parsing now supports multiple formats
`date_collected` parsing supports:
- `MM/DD/YYYY` (mdy)
- `YYYY-MM-DD` (ymd)
- `YYYYMMDD` (ymd after detection)

### 3) `gc_run_date` can be filled from GC `.txt` header when missing
If `gc_run_date` is blank in `sample_log.csv`, the script will extract it from the `.txt` content using the line:

`Acquired    5/24/2023 10:46:41 PM`

and parse the date portion into `gc_run_date`.

### 4) Standards QC and exclusion for ppm conversion
Standard injections are flagged as **low peak** and excluded from the standard mean (and thus calibration) if:

- `co2_height < 900000`
- `ch4_height < 8750`
- `n2o_height < 12500`

Only **non-flagged standard injections** are used to compute:
- mean standard heights (`mean_*_height`)
- precision (`sd/mean * 100`)
- counts used vs. dropped

---

## Folder conventions (required)

Each GC run folder must contain:
- GC `.txt` files (LabSolutions output)
- `sample_log.csv`

Example structure:
```
flux-processing/
  data/
    fluxes/
      gc-raw/
        2023/
          7.18.23/
            sample_log.csv
            071823_001.txt
            071823_002.txt
```

### `sample_log.csv` required headers
Your standardized column names should be:

- `date_collected`
- `site`
- `ecosystem`
- `chamber_no`
- `timepoint`
- `gc_run_date`  *(may be blank; can be filled from `.txt`)*
- `filename`
- `notes` *(optional)*

---

## How to run

### Step 01 — Extract peak heights and join metadata
This step:
- parses peak heights from each `.txt`
- joins to `sample_log.csv`
- writes `*_gc_peak_heights.csv` (+ standard subsets)

Run via your RMarkdown driver `gc-workflow.Rmd` or by calling:

```r
walk(run_dirs, ~process_gc_run(.x, tidy_root = "flux-processing/data/tidy"))
```

### Step 02 — Convert peak heights to ppm

This step:
	•	reads each *_gc_peak_heights.csv
	•	computes standard means (excluding low-peak STD rows)
	•	converts unknown samples to ppm using same-day calibration

Typical batch usage:

```
peak_files <- list.files(
  "flux-processing/data/tidy",
  pattern = "_gc_peak_heights\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

purrr::walk(peak_files, heights_to_ppm)
```
Outputs are written alongside the input peak heights file in the same run folder.

### QC guidance
	•	If *_ghg_ppm.csv is missing for a run, it usually means:
	•	no non-STD rows were detected (is_std logic too broad or metadata problem)
	•	standards were all excluded (too many low peaks)
	•	filenames did not join correctly (metadata becomes NA)
	•	Use the summary/QC helper chunk in your RMarkdown to identify:
	•	NAs in metadata columns
	•	NAs in peak heights
	•	missing standards

  ### Notes and assumptions
	•	Standard concentrations used for calibration are currently hard-coded in heights_to_ppm():
	•	CO2: 998
	•	CH4: 10.2
	•	N2O: 1

If tank concentrations change, update those constants.

### Suggested workflow for students - USE `gc-workflow.Rmd`
	1.	Place .txt exports and sample_log.csv into the correct run folders nested within correct directory structure.
	2.	Ensure sample_log.csv headers exactly match the required template.
	3.	Run Step 01 to generate peak heights.
	4.	Run Step 02 to generate ppm outputs.
	5.	Review *_std_heights.csv and *_gc_standards_qc.csv for QC issue
