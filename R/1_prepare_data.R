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
inat <- read.csv("Data/iNat_Data/interactions_data_4_4_2025.csv")

##############################################################################################################################


## We'll need to harmonize these data sets

field_filt <- field_dat %>%
  select(-Plot.Identifier, -Notes, -Second.iNat.link, -Month, -Year) %>%
  rename(URL = iNat.Link)

inat_filt <- inat %>%
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

# Combine them
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
  mutate(park_overlap_count = n_distinct(Interaction.ID)) %>% #number of unique interactions that are unique to iNat, unique to field, 
  #or shared for each park 
  ungroup()

##############################################################################################################################
#### Time for figures


## Quick density plots for total interactions
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


## Now trying the plot that Corey wanted
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

## Another option: flipped axes 
ggplot(park_overlap_plot, aes(x = Park.Name, y = park_overlap_count, fill = park_overlap)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Park",
    y = "Unique Interaction Richness",
    fill = "Overlap category") +
  theme_bw()


### Figure: Accumulation of interaction richness documented by iNat photos compared to interaction richness documented in the field


## Option 1: Assuming each row (observation) is a photo. We may want to re-label the x-axis to say Number of iNaturalist Observations
#            later, since each observation can have many photos but many of the photo numbers are missing from our data set so we
#             can't use this metric. No Randomization of interactions in this first figure. 

# Make columns into photo numbers
inat_curve <- inat_new %>%
  mutate(Photo_Number = row_number())

# Cumulative interaction richness from iNat
inat_curve <- inat_curve %>%
  mutate(
    Cumulative_Interaction_Richness =
      sapply(
        seq_along(Interaction.ID),
        function(i)
          n_distinct(Interaction.ID[1:i])
      )
  )

# Field-collected interaction richness 
field_interaction_richness <- field_filt %>%
  distinct(Interaction.ID) %>%
  nrow()
field_interaction_richness

# Plot it
ggplot(inat_curve, aes(x = Photo_Number, y = Cumulative_Interaction_Richness)) +
  geom_line(aes(color = "iNaturalist richness"), linewidth = 1) +
  geom_hline(
    aes(yintercept = field_interaction_richness, color = "Field richness"),
    linetype = "dashed",
    linewidth = 1) +
  scale_color_manual(
    name = "",
    values = c(
      "iNaturalist richness" = "black",
      "Field richness" = "red")) +
  labs(
    x = "Number of iNaturalist Photos",
    y = "Cumulative Interaction Richness",
    title = "Interaction Richness Accumulation from iNaturalist Observations") +
  theme_classic()



## Option 2: The same as the previous figure, but now observations are randomly reordered before calculating interaction richness
            

# Re-order observations before calculating cumulative interaction richness. Many randomizations (reshuffles). Build function here. 
accum_fun <- function(df) {
  df_rand <- df %>%
    slice_sample(prop = 1)
  data.frame(
    Observation_Number = seq_len(nrow(df_rand)),
    Richness = sapply(
      seq_len(nrow(df_rand)),
      function(i) {
        n_distinct(df_rand$Interaction.ID[1:i])
      }
    )
  )
}

set.seed(123)
n_reps <- 500   #500 randomizations (reshuffles)

# Run the function. 
accum_results <- bind_rows(
  lapply(
    seq_len(n_reps),
    function(x) {
      
      accum_fun(inat_new) %>%
        mutate(Replicate = x)
      
    }
  )
)

# Calculate mean richness and confidence intervals
accum_summary <- accum_results %>%
  group_by(Observation_Number) %>%
  summarise(
    Mean_Richness = mean(Richness),
    Lower_CI = quantile(Richness, 0.025),
    Upper_CI = quantile(Richness, 0.975),
    .groups = "drop"
  )

# Plot it
ggplot(accum_summary, aes(x = Observation_Number, y = Mean_Richness)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2) +
  geom_line(aes(color = "iNaturalist richness"), linewidth = 1) +
  geom_hline(
    aes(
      yintercept = field_interaction_richness,
      color = "Field richness"
    ),
    linetype = "dashed",
    linewidth = 1) +
  scale_color_manual(
    name = "",
    values = c(
      "iNaturalist richness" = "black",
      "Field richness" = "red"
    )) +
  labs(
    x = "Number of iNaturalist Observations",
    y = "Interaction Richness",
    title = "Interaction Accumulation Curve for iNaturalist Observations") +
  theme_classic()









