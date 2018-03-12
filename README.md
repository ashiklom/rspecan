# Dataset organization

All of the data for this project is stored in the `extdata/specdb.h5` file.
The file has the following general structure:

```
specdb.h5:
    /<project_code*>
        /info
            /short_name
            /long_name
            /description
            /citation
        /spectra
            /<observation_id*>
                /spectra
                /spectra_type
                /wavelength
        /metadata
            /observation_id
            /species_code
```

# Working with the dataset

## Use cases

Grab data from a specific project

## General overview

Use the following functions to add data to an HDF5 file:

- `r2hdf(obj, h5)` -- Write a named R list (or `data.frame`) to an HDF5 file
    - `r2hdf.list`
    - `r2hdf.data.frame`
- `spectra2hdf(spectra, h5, type = "reflectance")` -- Add `spectra` to an HDF5 file

Use the following functions to read data from an HDF5 file:


- `select_project(h5, ...)` -- Chose projects
- `select_`
- `select(h5, ...)` -- Choose objects from within HDF5 using standard or non-standard evaluation. Analogous to `dplyr::select`.
    - `select.h5`
- `filter(h5, ...)` -- Index based on conditions
    - 

--------------------------------

All of the spectral data are stored in HDF5 format, in the file `data/spectra.h5`.
Metadata for this file are stored in the `data/metadata.csv`,
and additional information about each column in this table is in `data/metadata_info.yml`.
Each file is described in more detail below.

## Spectral data (`spectra.h5`)

The `spectra.h5` file has the following organization:

- **Top level**: Project code, matching `project_code` field in `metadata`
- **Second level**: Spectra type -- one of the following:
    - `reflectance`
    - `transmittance`
    - `pseudo-absorbance` -- 1 / log10(reflectance)
    - `continuum-removed reflectance` -- see `prospectr::continuumRemoval` function
- **Third level**: Spectral data
    - `spectra` -- matrix of spectra, wavelength x observation
    - `wavelength` -- numeric vector of wavelengths (_must_ match `nrow(spectra)`)
    - `spectra_id` -- character vector of spectra IDs (_must_ match `ncol(spectra)`). Unique to each spectrum.
    - `observation_id` -- character vector of observation IDs. Each `spectra_id` will only have one `observation_id`, but an `observation_id` can have multiple `spectra_id`s. Each `observation_id` has a unique row in the metadata table.

## Metadata (`metadata.csv`)

The `metadata.csv` file is a single, flat file containing metadata for each spectrum.
Each row corresponds to a single `observation_id` in the `spectra.h5` file.


# Datasets processed

Status codes:
- D - Downloaded
- P - Processed
- R - Running
- C - Completed

|Status|Data code|Data long name|URL|
|:-----|:--------|:-------------|:-------------|
|C|`ecosis_hawaii2000`|Hawaii 2000 vegetation species spectra|https://ecosis.org/#result/060d2822-f250-4869-b734-4a92450393f0|
|C|`ecosis_californiatraits`|Fresh Leaf Spectra to Estimate Leaf Traits for California Ecosystems|https://ecosis.org/#result/0fadcc45-f79e-4fd3-a6ca-8afaf26ae299|
|C|`ecosis_santamonica`|Santa Monica Mountains vegetation species spectra|https://ecosis.org/#result/3f17bdbf-2a5f-4155-8c4e-50e98edf9d04|
|C|`ecosis_missoulats`|Missoula Montana lodgepole pine & big sagebrush time series|https://ecosis.org/#result/42c9f642-60db-470b-abc4-0ce5ae3720bb|
|C|`ecosis_pvy_solanum`|Varietal Discrimination and Detection of PVY in Solanum tuberosum: Hawaii 2014|https://ecosis.org/#result/6f1f8aa6-93c0-4fb1-ab1a-b0eaab0b2252|
|C|`ecosis_spectral_variation_leafcanopy`|Spectral Variation Between Leaf-level and Canopy-level Measurements|https://ecosis.org/#result/94d80d22-4d9d-424a-973b-1d0667f8f8f5|
|C|`ecosis_cedarcreek_biodiversity`|2014 Cedar Creek ESR Grassland Biodiversity Experiment: Leaf-level Contact Data: Trait Predictions|https://ecosis.org/#result/acbe2150-2fa7-467c-bd56-889ba7284d00|
|R|`ecosis_cornvarieties`|Spectral Characterization of Multiple Corn Varieties: West Madison Agricultural Station 2014|c0e238ea-5b23-452c-bc40-f0cfe2c6f032|
|D|`ecosis_couture2016`|Couture et al. 2016 MME data|d5445eb9-f334-4ee7-90a9-1fe07e67a20c|
|R|`ecosis_milkweed_stress`|Common Milkweed Leaf Responses to Water Stress and Elevated Temperature|9425d5b2-7633-45b5-9c07-6ec3323499a0|
|D|`ecosis_serbin_ntforests`|Fresh Leaf Spectra to Estimate Leaf Morphology and Biochemistry for Northern Temperate Forests|4a63d7ed-4c1e-40a7-8c88-ea0deea10072|
|R|`ecosis_soybean_aphid`|Productivity and Characterization of Soybean Foliar Traits Under Aphid Pressure|cdbb6b09-b481-4022-a0da-ad95a8b085d8|
|R|`ecosis_pepper`|Fresh and Dry Pepper Leaf Spectra with Associated Potassium and Nitrogen Measurements|a67925bf-f715-449a-939c-3cb000fb7889|
