## Zhihui Wang data
## :PROPERTIES:
## :header-args: :comments both :tangle scripts/process_spectra/zwang.R
## :header-args: :results silent
## :Bibtex: cite:wang_2015_leaf 
## :END:

## Wang, Z., Skidmore, A. K., Darvishzadeh, R., Heiden, U., Heurich, M., & Wang, T., Leaf nitrogen content indirectly estimated by leaf traits derived from the prospect model, IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 8(6), 3172–3182 (2015).  http://dx.doi.org/10.1109/jstars.2015.2422734 


## [[file:~/Projects/prospect-traits/rspecan/prospect_traits.org::*Zhihui%20Wang%20data][Zhihui Wang data:1]]
import::from(drake, drake_plan, drake_config, make)
import::from(magrittr, "%>%")

pkgconfig::set_config("drake::strings_in_dots" = "literals")

read_raw_data <- function(file, ...) {
  import::from(readxl, read_excel, .into = "")
  import::from(dplyr, mutate, select, everything, .into = "")

  read_excel(file, ...) %>%
    mutate(observation_id = sprintf("zwang_%02d", sample_id)) %>%
    select(observation_id, everything(), -sample_id)
}

extract_metadata <- function(dat, project_metadata) {
  import::from(dplyr, select, matches, transmute, .into = "")
  import::from(metar, add_column_metadata, add_metadata)
  dat %>%
    select(-matches("^[[:digit:]]+$")) %>%
    transmute(
      observation_id = observation_id,
      species_data_code = `Latin_name`,
      leaf_N_per_area = `N_area, g/cm2`,
      leaf_water_thickness = `Cw, g/cm2`,
      leaf_mass_per_area = `Cm, g/cm2`,
      leaf_chltot_per_area = `Cab, ug/cm2`
    ) %>%
    add_column_metadata(
      leaf_N_per_area = list(data_unit = "g cm-2"),
      leaf_water_thickness = list(data_unit = "g cm-2"),
      leaf_mass_per_area = list(data_unit = "g cm-2"),
      leaf_chltot_per_area = list(data_unit = "ug cm-2")
    ) %>%
    add_metadata(!!!project_metadata)
}

extract_spectra <- function(dat, spectra_type) {
  import::from(dplyr, select, matches, mutate, .into = "")
  import::from(tidyr, gather, .into = "")

  dat %>%
    select(observation_id, matches("^[[:digit:]]+$")) %>%
    gather(wavelength, value, -observation_id, na.rm = TRUE) %>%
    mutate(
      wavelength = as.numeric(wavelength),
      spectra_type = !!spectra_type,
      spectra_id = paste(observation_id, spectra_type, sep = "__")
    )
}

zwang_plan <- drake_plan(
  project_metadata = list(
    project_code = "zwang",
    short_name = "Wang et al. 2015",
    long_name = "Wang et al. 2015 Leaf Nitrogen Content Indirectly Estimated By Leaf
                    Traits Derived From the Prospect Model",
    URL = "https://doi.org/10.1109/jstars.2015.2422734",
    site_description = "Bavarian Forest National Park, Germany"
  ),
  raw_data_file = file_in("raw_data/zhihui_wang_protein/Bavaria_dataset_ZW.xlsx"),
  raw_refl = read_raw_data(raw_data_file, sheet = 1),
  raw_trans = read_raw_data(raw_data_file, sheet = 2),
  metadata = extract_metadata(raw_refl, project_metadata),
  refl_spectra = extract_spectra(raw_refl, "R"),
  trans_spectra = extract_spectra(raw_trans, "T"),
  all_spectra = dplyr::bind_rows(refl_spectra, trans_spectra)
)

zwang_plan_config <- drake_config(zwang_plan)
make(zwang_plan)
## Zhihui Wang data:1 ends here
