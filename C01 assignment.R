read_observationsnew = function(
    scientificname = "Prionace glauca",
    minimum_year = 1970,
    drop_na_individualCount = TRUE,
    allowed_basis = NULL
) {
  
  obs = read_obis(scientificname)
  
  # Add filters
  obs = obs |>
    filter(!is.na(eventDate)) |>
    filter(year >= minimum_year)
  
  # Add other filters
  if (drop_na_individualCount) {
    obs = obs |>
      filter(!is.na(individualCount))
  }
  
  if (!is.null(allowed_basis)) {
    obs = obs |>
      filter(basisOfRecord %in% allowed_basis)
  }
  
  # Filtering data by month 
  obs = obs |>
    mutate(month = factor(month, levels = month.abb))
  
  # Brickman mask filtering
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  
  mask = read_brickman(db)
  
  hitOrMiss = extract_brickman(mask, obs)
  
  obs = obs |>
    filter(!is.na(hitOrMiss$value))
  
  return(obs)
}

source("setup.R")
obs = read_observationsnew()
dim(obs)