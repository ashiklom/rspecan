# Dataset organization

Data for this project are stored in a database of plain text files organized as follows:
- The database root contains the following:
    - Folders for each project, named according to `project_code`
    - `species.csvy` -- Information about species
    - `instruments.yml` -- Information about instruments
    - `traits.yml` -- Information about traits and units
- Each project folder contains the following:
    - `metadata.csvy` -- Metadata about the spectra, which contains the following:
        * A YAML header with the following metadata (`*` indicates required data):
            - 
        * CSV data with the following columns (again, `*` indicates required columns):
    - `spectra/` -- Directory containing spectra
- Each `spectra` folder contains `<observation_id>.csvy` files
    - Each `observation_id` corresponds to observations made on a single leaf at a particular point in time, so each spectra within a file is treated as a single observation in the context of PROSPECT inversion.
    - The `spectra_type` field indicates the type of spectra; one of:
        * `R` - Reflectance
        * `T` - Transmittance
        * `PA` - Pseudo-absorbance (log10(1/R))
        * `CRR` - Continuum-removed reflectance
    - The data itself is CSV with a `wavelengths` column of wavelengths and remaining column names corresponding to observation IDs (which should all be the same)

# Analysis workflow

1. Prepare the inversion files with `scripts/01_prepare_inversion.R`
2. Run the inversion for a single observation with `scripts/02_run_inversion.R`. Use the submit script created in (1) to run these in array mode on an SGE cluster
3. Each result is stored in its own file, `results.csvy`. Create a list of all the results files with `find spectra_db -name results.csvy > results_files`.
4. Combine the results into a single file with `scripts/load_results.R`. Note that this can take a while. This creates the `spectra_db/all_results.csv` file.
5. Clean the results with `scripts/clean_results.R`. This removes unrealistic results and performs minor formatting revisions, and then stores the results in `spectra_db/cleaned_results.csv`.
6. Generate figures using the following scripts (in no particular order) (* indicates scripts that only depend on metadata and not the results):
    - `scripts/figures/data_map.R`* -- Map of the data
    - `scripts/figures/data_climate.R`* -- Plots of the data locations in climate (precipitation and temperature space)
    - `scripts/figures/project_table.R`* -- Table of projects and their descriptions and sample sizes
    - `scripts/figures/prospect_version_agreement.R` -- Agreement in parameter estimates among PROSPECT versions
    - `scripts/figures/validation.R` -- Agreement of PROSPECT estimates with observed traits
