#devtools::install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
library(rlpi)
library(here)
library(tidyverse)

file.copy(from=system.file("extdata", "example_data.zip", package = "rlpi"), to=getwd())
unzip("example_data.zip")

dd <- read_csv("example_data/LPI_LPR2016data_public.csv")
dd[dd == "NULL"] = NA

dd2 <- dd %>% mutate_at(vars(contains("19")), as.numeric)

dd3 <- dd2 %>% gather("Year", "Value", 29:94)
dd3$Year <- as.numeric(dd3$Year)
dd3$Value <- as.numeric(dd3$Value)
dd3$Database <- "LPI"
dd3$Spcode <- dd3 %>% group_indices(ID)
dd3$Species_name <- paste(dd3$Genus, " ", dd3$Species)
dd3$ID <- dd3 %>% group_indices(ID)
dd3$ID <- dd3$ID + 2647

dd4 <- dd3 %>% filter(!is.na(Value)) %>% group_by(ID) %>% mutate(N_obs = n()) %>% arrange(ID, Year)

species_table <- dd4 %>% filter(N_obs > 25) %>% select(ID, Database, Spcode, Year, Species_name, Class, Value) %>% arrange(ID)
metadata_table <- dd4 %>% filter(N_obs >= 25) %>%  select(-Value, -Year) %>% distinct(ID, .keep_all = T)

write.csv(species_table, "species_table.csv")
write.csv(metadata_table, "metadata_table.csv")
