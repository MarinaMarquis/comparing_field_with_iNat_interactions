## Data preparation R script
## In this script we are harmonizing the datasets from Marquis et al. (2026) and Francis et al. (2026) (preprints). 

## The former is a professionally collected data set of plant-pollinator interactions, and the latter
## is a datset of plant-pollintor interactions that were uploaded to iNat and then tagged by the authors, manually.

##############################################################################################################################

## Let's load packages

library(tidyverse)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)


## Now let's read in the data: 

## First Marina's field data
field_dat <- read.csv("Data/Field Observation Data/interaction_data_clean.csv")

## Now iNat data from the same parks
inat <- readRDS("Data/iNat_Data/interactions_data_annotated.RDS")

##############################################################################################################################

# filter the field dataset to meet the pollinator definition of the iNaturalist dataset

# to do this, we got subfamily and superfamily taxonomic information to join to field data
taxonomic_chart <- read.csv("Data/Field Observation Data/taxonomy_chart.csv") %>%
  select(Insect_ID, subfamily, superfamily)

field_dat <- left_join(field_dat, taxonomic_chart, by="Insect_ID")

# now we need to filter according to this definition: 
# superfamily Apoidea, family Bombyliidae, subfamily Cetoniinae, order Lepidoptera, and subfamily Lepturinae
field_dat_filtered <- field_dat %>%
  filter((Insect.Order == "Lepidoptera") |
           (Insect.Order == "Diptera" & Insect.Family == "Bombyliidae") |
           (Insect.Order == "Hymenoptera" & superfamily == "Apoidea") |
           (Insect.Order == "Coleoptera" & Insect.Family == "Scarabaeidae" & subfamily == "Cetoniinae") |
           (Insect.Order == "Coleoptera" & Insect.Family == "Scarabaeidae" & subfamily == "Lepturinae"))

# we also need to remove pollinators not identified to species
field_dat_filtered <- field_dat_filtered %>%
  filter(complete.cases(Insect.Species),
         Insect.Species != "")

nrow(field_dat)
nrow(field_dat_filtered)

nrow(field_dat_filtered)/nrow(field_dat)*100

saveRDS(field_dat_filtered, "Data/Field Observation Data/interaction_data_clean_filtered.RDS")

##############################################################################################################################

# determine how many species in the inat dataset have are in the field dataset. This denotes that they 
# meet the definition of pollinator that was set up for that project. All other pollinators will need to be 
# mannually examined to determine if they meet the same definition of pollinator set for the field data.

inat_sp <- unique(inat$Taxon.name)
field_sp <- unique(field_dat_filtered$Insect_ID)

# remove subspecies from inat data
inat_sp <- inat_sp %>%
  str_trim() %>%
  .[str_count(., "\\S+") >= 2] %>%  # remove the one genus-level species
  word(1, 2) %>%                    # remove subspecies designation
  unique()

inat_sp <- unique(inat_sp)

# remove extra spaces from field_sp
field_sp <- field_sp %>%
  str_squish()

species_not_in_field <- setdiff(inat_sp, field_sp)
species_not_in_field

species_in_both <- intersect(inat_sp, field_sp)
species_in_both

length(field_sp)
length(inat_sp)
length(species_not_in_field)
length(species_in_both)

# save the species that were not found in the field. We will use it to manually determine if they are pollinators
species_not_in_field_df <- data.frame(species=species_not_in_field)

write_csv(species_not_in_field_df, "Data/Pollinator Definition/species_only_in_inat_dataset.csv")

##############################################################################################################################

# Read in the excel file where we manually determined the definition of pollinators for those that were not found
# in the field

pollinator_def <- read_csv("Data/Pollinator Definition/species_only_in_inat_dataset_QAQC.csv")

# select only species that did not qualify as pollinators
not_pollinators <- pollinator_def %>%
  filter(pollinator_QAQC=="N")

# remove those species from the iNaturalist dataset
inat_filtered <- inat %>%
  # remove subspecies
  mutate(
    Taxon.name = Taxon.name %>%
      str_trim() %>%
      word(1, 2)
  ) %>%
  filter(str_count(Taxon.name, "\\S+") >= 2) %>%
  filter(!Taxon.name %in% not_pollinators$species)

# how many rows of data did this remove 
nrow(inat)-nrow(inat_filtered)
# only 4

saveRDS(inat_filtered, "Data/iNat_Data/inat_cleaned_w_annotation.RDS")


##############################################################################################################################


inat_filtered <- readRDS("Data/iNat_Data/inat_cleaned_w_annotation.RDS")

inat_annotated_on_inat <- inat_filtered %>%
  filter(!taxon.name == "NA")

## We'll need to harmonize these data sets

field_filt <- field_dat_filtered %>%
  select(-Plot.Identifier, -Notes, -Second.iNat.link, -Month, -Year) %>%
  rename(URL = iNat.Link)
#Save for use in other scripts
write_csv(field_filt, "Data/Field Observation Data/filtered_field_data.csv")


inat_filt <- inat_filtered %>%
  select(-id, -Observed.on, -Notes, -Number.of.observation.photo, -Image.number)

inat_filt_annot <- inat_annotated_on_inat %>%
  select(-id, -Observed.on, -Notes, -Number.of.observation.photo, -Image.number)

# Need to make a list of the parks with both sets of names

list(unique(field_filt$Park.Name))

list(unique(inat_filt$Park.name))

parks <- data.frame(
  Park.Name = c("Military Trail", "Helene Klein", "Markham", "CB Smith", "Long Key", "Vista View", "Highlands Scrub", "Quiet Waters"),
  Park.name = c("Broward_Military Trail Nature Area", "Broward_Helene Klein Pineland Preserve", "Broward_Markham Park", "Broward_CB Smith Park",
                "Broward_Long Key Natural Area and Nature Center", "Broward_Vista View Park", "Broward_Highlands Scrub Natural Area", "Broward_Quiet Waters Park")
)


### Join to the iNat data frame.
### Also removing interactions where we don't have the plant species
### Make sure that none of the iNat obs match the ones uploaded by Marina
### from her professional data set

inat_new <- inat_filt %>%
  left_join(parks, by = "Park.name") %>%
  select(-Park.name) %>%
  filter(Park.Name %in% field_filt$Park.Name, !URL %in% field_filt$URL) %>%
  filter(!Flower_species == "NA", !Flower_species == ".")

inat_new <- inat_new %>%
  mutate(Plant_ID = paste(Flower_Genus, Flower_species),
         Interaction.ID = paste(Plant_ID, Taxon.name, sep = " | "))
# Save for use in other scripts
write_csv(inat_new, "Data/iNat_Data/filtered_and_harmonized_iNat_data.csv")


### Now do this with the annotated data set
inat_annot_new <- inat_filt_annot %>%
  left_join(parks, by = "Park.name") %>%
  select(-Park.name) %>%
  filter(Park.Name %in% field_filt$Park.Name, !URL %in% field_filt$URL) %>%
  filter(!Flower_species == "NA", !Flower_species == ".")

inat_annot_new <- inat_annot_new %>%
  mutate(Plant_ID = paste(Flower_Genus, Flower_species),
         Interaction.ID = paste(Plant_ID, Taxon.name, sep = " | "))
# Save for use in other scripts
write_csv(inat_annot_new, "Data/iNat_Data/filtered_and_harmonized_annotated_iNat_data.csv")


#### Now let's see if we can find the interactions that are unique to each park
#### from both data sets 

# Summarize interaction richness and abundance per park from iNat dataset
inat_cols <- inat_new %>%
  select(Park.Name, Interaction.ID) %>%
  mutate(dataset = "iNaturalist") %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(n = n()) %>% 
  ungroup() %>%
  group_by(Park.Name) %>%
  mutate(park_int_distinct = n_distinct(Interaction.ID), #interaction richness in each park (how many UNIQUE interactions)
         park_int_total = sum(n)) %>%  #abundance of interactions in each park (how many interactions total)
  distinct(Park.Name, Interaction.ID, .keep_all = TRUE) %>%
  ungroup()

# Summarize interaction richness and abundance per park from Marina dataset
field_cols <- field_filt %>%
  select(Park.Name, Interaction.ID) %>%
  mutate(dataset = "Field Collection") %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(Park.Name) %>%
  mutate(park_int_distinct = n_distinct(Interaction.ID), #interaction richness in each park (how many UNIQUE interactions)
         park_int_total = sum(n)) %>% #abundance of interactions in each park (how many interactions total)
  distinct(Park.Name, Interaction.ID, .keep_all = TRUE) %>%
  ungroup()

#inat sub-category that were annotated on iNat
inat_annot_cols <- inat_annot_new %>%
  select(Park.Name, Interaction.ID) %>%
  mutate(dataset = "iNaturalist annotated") %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(n = n()) %>% 
  ungroup() %>%
  group_by(Park.Name) %>%
  mutate(park_int_distinct = n_distinct(Interaction.ID), #interaction richness in each park (how many UNIQUE interactions)
         park_int_total = sum(n)) %>%  #abundance of interactions in each park (how many interactions total)
  distinct(Park.Name, Interaction.ID, .keep_all = TRUE) %>%
  ungroup()

# Combine them
combined_df <- bind_rows(inat_cols, field_cols, inat_annot_cols) %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(
    park_overlap = case_when(
      all(c("iNaturalist", "Field Collection") %in% dataset) ~ "shared",
      "iNaturalist" %in% dataset ~ "iNat only",
      "Field Collection" %in% dataset ~ "field exclusive"
    )
  ) %>%
  ungroup() %>%
  group_by(Park.Name, park_overlap) %>%
  mutate(park_overlap_count = n_distinct(Interaction.ID)) %>% #number of unique interactions that are unique to iNat, unique to field, 
  #or shared for each park 
  ungroup()

saveRDS(combined_df, "Data/combined_interaction_data.RDS")

##############################################################################################################################



