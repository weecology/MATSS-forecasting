#' @title Get taxonomy data for all organisms
#'
#' @description Imports taxonomic information for all organisms that are forecasted
#'  relying on functions of the `taxize` package
#' 
#' @return a dataframe with all taxonomic ranks found for a particular species
#'
#' @export
#' 
get_taxonomy <- function()
{

drake::loadd(full_results, cache=cache)    
    
# get list of all species and check for duplicate species
species_table_list <- lapply(1:nrow(full_results), function(x) full_results[[x, "metadata"]]$species_table %>% select(species_binomial) %>% distinct(species_binomial) )

    
# translate salmon common names to scientific names
#scinames <- comm2sci(c("Chinook salmon", "Pink salmon", "Chum salmon", "Rainbow trout", "Sockeye salmon"), ask = F)

species_table <- bind_rows(species_table_list) %>% 
    mutate(species_binomial = case_when(
        species_binomial == "Chinook" ~ "Oncorhynchus tshawytscha",
        species_binomial == "Pink" ~ "Oncorhynchus gorbuscha",
        species_binomial == "Chum" ~ "Oncorhynchus keta",
        species_binomial == "Steelhead" ~ "Oncorhynchus mykiss",
        species_binomial == "Sockeye" ~ "Oncorhynchus nerka",
        TRUE ~ as.character(species_binomial))
    )

# check whether species is in database, extract ID
uids <- get_uid(as.character(species_table$species_binomial), ask = F)

# use ID to get full taxnomic information on all species
taxonomic_classes <- classification(uids)
#lapply(taxonomic_classes, head, 100)

# remove NAs
taxonomic_classes2 <- taxonomic_classes[unlist(lapply(taxonomic_classes, is.data.frame))]

# combine taxonomic information into one df
taxo_df <- bind_rows(
    lapply(1:length(taxonomic_classes2), function(i) 
    {pivot_wider(as.data.frame(taxonomic_classes2[[i]]) %>% dplyr::select(-id) %>% filter(rank != "no rank"), names_from = "rank", values_from = "name")})
)

return(taxo_df)

}






