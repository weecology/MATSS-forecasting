#' @title Read in a specific database from Ward et al. 2014
#'
#' @param database_name the name of the database
#' @param ward_RDS_file the location of the RDS file with the combined datasets
#'   that is produced as a result of running \code{\link{reshape_ward_data}}
#'
#' @return the dataset object
#'
#' @export
#'
get_ward_data <- function(database_name, ward_RDS_file)
{
    readRDS(ward_RDS_file) %>%
        dplyr::filter(.data$database == database_name) %>%
        dplyr::pull(.data$dataset) %>%
        dplyr::first()
}

#' @title Reshape the processed data file from Ward et al. 2014
#'
#' @description The master data file from Ward et al. 2014 has 8 datasets 
#'   combined together in long form. We do some preprocessing here to correct 
#'   dates, shift all variable names to lowercase characters, and construct a 
#'   separate data object for each dataset. The format follows that of MATSS 
#'   (browse the data formats vignette) The resulting object has the names of 
#'   the datasets and the data objects.
#'
#' @param data_file the path to the raw CSV file from Ward et al. 2014. (The 
#'   default is bundled with this package.)
#' @inheritParams get_ward_data
#'
#' @return a character vector with the names of the database
#'
#' @export
#'
reshape_ward_data <- function(data_file = system.file("extdata", "processed_data", "masterDat_2019-06-12.csv",
                                                      package = "MATSSforecasting", mustWork = TRUE), 
                              ward_RDS_file = here::here("analysis", "data", "ward_fish_data.RDS"))
{
    dat <- utils::read.csv(data_file)
    
    reshape_data <- function(df)
    {
        temp_data <- df %>%
            dplyr::select(c("id", "year", "species", "class", "value")) %>%
            dplyr::mutate(year = ifelse(.data$year < 1800, .data$year + 1900, .data$year))
        
        species_table <- temp_data %>%
            dplyr::select(c("id", "species", "class")) %>%
            dplyr::distinct()
        
        temp_data <- temp_data %>%
            dplyr::select(c("id", "year", "value")) %>%
            tidyr::spread(.data$id, .data$value)
        
        covariates <- dplyr::select(temp_data, .data$year)
        abundance <- dplyr::select(temp_data, -.data$year)
        
        list(abundance = abundance,
             covariates = covariates,
             metadata = list(species_table = species_table, timename = "year"))
    }
    
    dat <- dat %>%
        dplyr::rename_all(tolower) %>%
        dplyr::group_by(.data$database) %>% # split datasets by source
        tidyr::nest() %>%
        dplyr::mutate(dataset = purrr::map(.data$data, reshape_data)) %>% # reshape int common data format
        dplyr::ungroup() %>%
        dplyr::select(c("database", "dataset")) %>%
        dplyr::mutate(database = sub("\\.", "_", .data$database))
    
    ## save resulting tibble
    saveRDS(dat, file = ward_RDS_file)
    
    ## return names of databases
    return(dplyr::pull(dat, .data$database))
}
