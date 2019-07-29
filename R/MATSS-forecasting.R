## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
    c("database", "dataset", "id", "map", "read.csv", "species", 
      "target", "value", "ward_database", "year", ".", "mse", 
      "ID", "Value", "binomial", "country", "family", "fw_biome", "fw_realm", 
      "genus", "latitude", "location", "longitude", "m_biome", "m_ocean", 
      "m_realm", "method", "num_obs", "region", "subspecies", "t_biome", 
      "t_realm"))