austraits_weighted_means <- function(austraits, traits) {
  
  library(austraits)
  
  # any data that is a mean, median or raw, create a site mean
  data_means <- 
    (austraits %>% austraits::join_location_coordinates())$traits %>% 
    dplyr::filter(trait_name %in% traits) %>%
    dplyr::filter(value_type %in% c("mean", "raw", "median")) %>%
    dplyr::mutate(
      replicates = 1,
      log_value = ifelse(!is.na(value) & value > 0, log10(as.numeric(value)), NA)
      ) %>%
    dplyr::group_by(taxon_name, trait_name, dataset_id, location_id) %>% 
    dplyr::summarise(
      mean = mean(as.numeric(value)),
      min = min(as.numeric(value)),
      max = max(as.numeric(value)),
      median = median(as.numeric(value)),
      `latitude (deg)` = first(`latitude (deg)`),
      `longitude (deg)` = first(`longitude (deg)`),
      location_name = first(location_name),
      all_replicates = sum(replicates),
      geom_mean = mean(log_value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(taxon_name, trait_name, dataset_id, location_id, mean, min, max, median, geom_mean, all_replicates, location_name, `latitude (deg)`, `longitude (deg)`) %>%
    dplyr::mutate(
      location_replicates = 1,
      flora_replicates = 0,
      geom_mean = 10^(geom_mean)
    )
  
  # any data that is a max or a min (range) and basically from a flora, create a mean value
  flora_means <-
    austraits$traits %>%
    dplyr::filter(trait_name %in% traits) %>%
    dplyr::filter(value_type %in% c("minimum", "maximum"), basis_of_record %in% c("preserved_specimen", "literature"))
  
  if (nrow(flora_means > 0)) {
    flora_means <- 
      flora_means %>%
        dplyr::group_by(taxon_name, trait_name, dataset_id, observation_id, original_name) %>% 
        dplyr::summarise(
          mean = mean(as.numeric(value)),
          min = min(as.numeric(value)),
          max = max(as.numeric(value))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(taxon_name, trait_name, dataset_id, observation_id, original_name, mean, min, max) %>%
        dplyr::mutate(
          location_replicates = 0,
          flora_replicates = 1,
          all_replicates = 1,
          meadian = NA
        )
      }
  
  means <-
    if (nrow(flora_means > 0)) {
      dplyr::bind_rows(
        data_means %>% dplyr::mutate(across(dplyr::any_of(c("mean", "min", "max")), ~as.numeric(.x))),
        flora_means %>% dplyr::mutate(across(dplyr::any_of(c("mean", "min", "max")), ~as.numeric(.x)))
      )
    } else {
      data_means %>% dplyr::mutate(across(dplyr::any_of(c("mean", "min", "max")), ~as.numeric(.x)))
    }
  
  means <- means %>%
    dplyr::mutate(
      log_value = ifelse(!is.na(mean) & mean > 0, log10(as.numeric(mean)), NA)
    ) %>%
    dplyr::group_by(taxon_name, trait_name) %>%
    dplyr::summarise(
      mean = mean(mean),
      min = min(min),
      max = max(max),
      median = median(median),
      geom_mean = mean(log_value),
      all_replicates = sum(all_replicates),
      location_replicates = sum(location_replicates),
      flora_replicates = sum(flora_replicates)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()  %>%
    dplyr::mutate(
      geom_mean = 10^(geom_mean)
    )
  
  means
}

categorical_summary <- function(austraits, trait_names) {
  
tmp <-  
  austraits$traits %>%
    dplyr::filter(trait_name %in% trait_names) %>%
    dplyr::select(dplyr::all_of(c("dataset_id", "taxon_name", "trait_name", "location_id", "observation_id", "value"))) %>%
    dplyr::mutate(value_tmp = stringr::str_split(value, " "))
  
tmp <-
  tmp %>%
    tidyr::unnest_longer(value_tmp) %>%
    dplyr::mutate(
      replicates = 1
    ) %>%
  dplyr::group_by(taxon_name, trait_name, value_tmp) %>%
    summarise(
      replicates = sum(replicates),
      value_tmp = first(value_tmp)
      ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    tmp_summary = paste0(value_tmp, " (", replicates, ")")
  ) %>%
  dplyr::group_by(taxon_name, trait_name) %>%
  dplyr:: mutate(
      value_summary = paste0(tmp_summary, collapse = "; ")  
    ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("value_tmp", "replicates", "tmp_summary"))) %>%
  dplyr::distinct()

  tmp
}

categorical_summary_by_value <- function(austraits, trait_names) {
  
  tmp <-  
    austraits$traits %>%
    dplyr::filter(trait_name %in% trait_names) %>%
    dplyr::select(dplyr::all_of(c("dataset_id", "taxon_name", "trait_name", "location_id", "observation_id", "value"))) %>%
    dplyr::mutate(value_tmp = stringr::str_split(value, " "))
  
  tmp <-
    tmp %>%
    tidyr::unnest_longer(value_tmp) %>%
    dplyr::mutate(
      replicates = 1
    ) %>%
    dplyr::group_by(taxon_name, trait_name, value_tmp) %>%
    dplyr::summarise(
      replicates = sum(replicates),
      value_tmp = first(value_tmp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  tmp
}


merge_entity_types <- function(dataset_id) {
  tmp <-
    (austraits %>%
    austraits::extract_dataset(dataset_id))$traits 
  
  if(any(as.numeric(tmp$method_context_id) > 1) && !all(is.na(tmp$method_context_id))) {
    
    tmp_columns <-
      tmp %>%
        dplyr::filter(as.numeric(method_context_id) > 1) %>%
        dplyr::distinct(trait_name)
    
    tmp <- 
      tmp %>% 
        dplyr::mutate(trait_name = ifelse(trait_name %in% tmp_columns$trait_name,
                                   paste(trait_name, "method_context_id", method_context_id, sep = "_"),
                                   trait_name))
  }
  
  if(any(as.numeric(tmp$method_id) > 1) && !all(is.na(tmp$method_id))) {
    
    tmp_columns <-
      tmp %>%
        dplyr::filter(as.numeric(method_id) > 1) %>%
        dplyr::distinct(trait_name)
    
    tmp <- 
      tmp %>% 
        dplyr::mutate(trait_name = ifelse(trait_name %in% tmp_columns$trait_name,
                                   paste(trait_name, "method_id", method_id, sep = "_"),
                                   trait_name))
  }
  
  tmp <- 
    tmp %>% 
      dplyr::select(-dplyr::all_of(c("method_id", "method_context_id", "unit", "measurement_remarks", "replicates")))
  
  tmp <- 
    tmp %>%
      dplyr::group_by(trait_name, dataset_id, observation_id, repeat_measurements_id) %>%
      dplyr::mutate(
        count = length(trait_name),
        value = ifelse(count > 1, paste0(value, " (", value_type, ")"), value),
        value = paste(value, collapse = "; ")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-value_type) %>%
      dplyr:: distinct()
  
  for (i in c("source_id", "repeat_measurements_id", "plot_context_id", "treatment_context_id",
              "temporal_context_id", "entity_context_id")) {
    if(all(is.na(tmp[[i]]))) {
      tmp <- tmp %>% dplyr::select(-i)
    }
  }

  if(length(unique(tmp$original_name)) == length(unique(tmp$taxon_name))) {
    
    tmp <- 
      tmp %>%
        dplyr::select(-dplyr::all_of(c("original_name")))
  }

  for (i in c("basis_of_record", "life_stage", "value_type")) {
    
    if(length(unique(tmp[[i]])) == 1) {
      tmp <- tmp %>% dplyr::select(-i)   
    }
    
  }

  species_values <- 
    tmp %>%
    dplyr::filter(entity_type == "species") %>%
    dplyr::select(dplyr::any_of(c("taxon_name", "trait_name", "original_name", "source_id", "value", "basis_of_record", 
                                  "value_type", "temporal_context_id"))) %>%
    tidyr::pivot_wider(names_from = trait_name, values_from = value)
  
  population_values <- 
    tmp %>%
    dplyr::filter(entity_type == "population") %>%
    dplyr::select(dplyr::any_of(c("taxon_name", "trait_name", "value", "original_name", "source_id", "population_id", "basis_of_record", 
           "plot_context_id", "treatment_context_id", "temporal_context_id", "entity_context_id", "location_id", "method_context_id", "method_id"))) %>%
    tidyr::pivot_wider(names_from = trait_name, values_from = value)
  
  individual_values <- 
    tmp %>%
    dplyr::filter(entity_type == "individual") %>%
    tidyr::pivot_wider(names_from = trait_name, values_from = value)
  
  if(nrow(individual_values) != 0 & nrow(population_values) != 0 & nrow(species_values) != 0) {
    merged <- individual_values %>%
      dplyr::left_join(population_values) %>%
      dplyr::left_join(species_values)
  }
  
  if (nrow(individual_values) != 0 & nrow(population_values) != 0 & nrow(species_values) == 0) {
    merged <-   individual_values %>%
      dplyr::left_join(population_values)
  }
  
  if (nrow(individual_values) != 0 & nrow(population_values) == 0 & nrow(species_values) != 0) {
    merged <-   individual_values %>%
      dplyr::left_join(species_values)
  }  
  
  if (nrow(individual_values) == 0 & nrow(population_values) != 0 & nrow(species_values) != 0) {
    merged <-   population_values %>%
      dplyr::left_join(species_values)
  }
  
  if(nrow(individual_values) == 0 & nrow(population_values) == 0 & nrow(species_values) != 0) {
    merged <- species_values
  }
  
  if(nrow(individual_values) == 0 & nrow(population_values) != 0 & nrow(species_values) == 0) {
    merged <- population_values
  }
  
  if(nrow(individual_values) != 0 & nrow(population_values) == 0 & nrow(species_values) == 0) {
    merged <- individual_values
  }
  
  merged <-
    merged %>%
    dplyr::select(-dplyr::any_of(c("observation_id", "entity_type")))
  
  merged
  
}
