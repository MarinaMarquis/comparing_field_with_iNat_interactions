## Data preparation R script
## In this script we are harmonizing the datasets from Marquis et al. (2026) and Francis et al. (2026) (preprints). 

## The former is a professionally collected data set of plant-pollinator interactions, and the latter
## is a datset of plant-pollintor interactions that were uploaded to iNat and then tagged by the authors, manually.


## let's load packages

library(tidyverse)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)

## first marina's field data
field_dat <- read.csv("Data/Field Observation Data/interaction_data_clean.csv")

## iNat data from the same parks
inat <- read.csv("Data/iNat_Data/interactions_data_4_4_2025.csv")

## we'll need to harmonize these data sets

field_filt <- field_dat %>%
  select(-Plot.Identifier, -Notes, -Second.iNat.link, -Month, -Year) %>%
  rename(URL = iNat.Link)

inat_filt <- inat %>%
  select(-id, -Observed.on, -Notes, -Number.of.observation.photo, -Image.number)

# need to make a list of the parks with both sets of names

list(unique(field_filt$Park.Name))

list(unique(inat_filt$Park.name))

parks <- data.frame(
  Park.Name = c("Military Trail", "Helene Klein", "Markham", "CB Smith", "Long Key", "Vista View", "Highlands Scrub", "Quiet Waters"),
  Park.name = c("Broward_Military Trail Nature Area", "Broward_Helene Klein Pineland Preserve", "Broward_Markham Park", "Broward_CB Smith Park",
                "Broward_Long Key Natural Area and Nature Center", "Broward_Vista View Park", "Broward_Highlands Scrub Natural Area", "Broward_Quiet Waters Park")
)


### join to the iNat data frame
### also removing interactions where we don't have the plant species
### make sure that none of the iNat obs match the ones uploaded by Marina
### from her professional data set

inat_new <- inat_filt %>%
  left_join(parks, by = "Park.name") %>%
  select(-Park.name) %>%
  filter(Park.Name %in% field_filt$Park.Name, !URL %in% field_filt$URL) %>%
  filter(!Flower_species == "NA", !Flower_species == ".")

inat_new <- inat_new %>%
  mutate(Plant_ID = paste(Flower_Genus, Flower_species),
         Interaction.ID = paste(Plant_ID, Taxon.name, sep = " | "))


#### Now let's see if we can find the interactions that are unique to each park
#### from the 

inat_cols <- inat_new %>%
  select(Park.Name, Interaction.ID) %>%
  mutate(dataset = "iNaturalist") %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(Park.Name) %>%
  mutate(park_int_distinct = n_distinct(Interaction.ID),
         park_int_total = sum(n)) %>%
  distinct(Park.Name, Interaction.ID, .keep_all = TRUE) %>%
  ungroup()

field_cols <- field_filt %>%
  select(Park.Name, Interaction.ID) %>%
  mutate(dataset = "Field Collection") %>%
  group_by(Park.Name, Interaction.ID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(Park.Name) %>%
  mutate(park_int_distinct = n_distinct(Interaction.ID),
         park_int_total = sum(n)) %>%
  distinct(Park.Name, Interaction.ID, .keep_all = TRUE) %>%
  ungroup()


combined_df <- bind_rows(inat_cols, field_cols) %>%
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
  mutate(park_overlap_count = n_distinct(Interaction.ID)) %>%
  ungroup()


## quick density plots for total interactions
ggplot(combined_df, aes(x = n, fill = dataset, color = dataset)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  facet_wrap(~ Park.Name, scales = "free_y") +
  labs(
    x = "Interaction abundance",
    y = "Density",
    fill = "Dataset",
    color = "Dataset"
  ) +
  theme_classic()


## now trying the plot that Corey wanted
park_overlap_plot <- combined_df %>%
  distinct(Park.Name, park_overlap, park_overlap_count)

ggplot(park_overlap_plot, aes(
  x = Park.Name,
  y = park_overlap_count,
  fill = park_overlap
)) +
  geom_col() +
  labs(
    x = "Park",
    y = "Number of unique interactions",
    fill = "Overlap category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )  
