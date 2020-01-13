#' @title Get life history data for all organisms
#'
#' @description Imports life history information from online databases, i.e. rfishbase and traits
#' 
#' @return a dataframe with life history information for all species of a particular group of organisms
#'
#' @export
#'

pull_fishbase <- function(taxonomy_df){

drake::loadd(taxonomy, cache=cache)    
    
    
# select only fish from taxonomy table
fish <- taxonomy_df %>% filter(class %in% c("Actinopteri", "Chondrichthyes"))

# get life histoy information from rfishbase
fishbase_species <- species(fish$species, field = c("Species", "LongevityWild", "Length", "Weight"))
fishbase_ecology <- ecology(fish$species, fields=c("Species", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))

# find all species
fishbase_meta <- merge(fish, fishbase_species, by.x = ("species"), by.y = c("Species"), all = FALSE)
fishbase_meta <- merge(fishbase_meta, fishbase_ecology, by = ("species"), by.y = c("Species"), all = FALSE)

return(fishbase_meta)

}



pull_traits <- function(taxonomy_df){
    
drake::loadd(taxonomy, cache=cache)
    
# get trait information from traits package
    
# mammals, birds and reptiles
mammals_birds_herps <- taxonomy_df %>% filter(class %in% c("Mammalia", "Aves", "Reptilia")) %>% dplyr::select(species)
    
ernest_res <- tr_ernest()
trait_data <- ernest_res$data
    
# find all species
trait_data <- trait_data %>% mutate(species_name = paste0(genus, " ", species))
mammals_birds_herps_meta <- merge(mammals_birds_herps, trait_data, by.x = ("species"), by.y = c("species_name"))

return(mammals_birds_herps_meta)
    
}
