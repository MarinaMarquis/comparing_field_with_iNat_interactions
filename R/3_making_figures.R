#### Making figures ####
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpattern)

## let's load in our combined data frame that contians all of our data
combined_df <- readRDS("Data/combined_interaction_data.RDS")


#### SUMMARY STATISTICS ###

#total interactions
sum(unique(combined_df$park_int_total))

# unique interactions
length(unique(combined_df$Interaction.ID))

#unique interactions per dataset
tot_unique <- combined_df %>%
  group_by(park_overlap) %>%
  summarize(tot_uni = length(unique(Interaction.ID)))


# total interactions within each dataset
totals <- combined_df %>%
  group_by(dataset) %>%
  summarize(total_n = sum(unique(park_int_total)))

totals_combined <- combined_df %>%
  group_by(park_overlap)%>%
  summarize(total_n = sum(unique(park_int_total)))

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

## How about a sub-plot that looks at the iNat data only, and the difference between those annotated on 
## iNat already and those that Thomas et al. had to go through
inat_only <- combined_df %>%
  filter(dataset == "iNaturalist" | dataset == "iNaturalist annotated") %>%
  distinct(Park.Name, dataset, .keep_all = TRUE) %>%
  select(-Interaction.ID)

###create seperate dataframes for each
inat_no <- inat_only %>%
  filter(dataset == "iNaturalist")

inat_annot <- inat_only %>%
  group_by(Park.Name) %>%
  filter(dataset == "iNaturalist annotated") %>%
  rename(park_int_uni = park_int_distinct) %>%
  select(Park.Name, park_int_uni)

inat_no <-  inat_no %>%
  group_by(Park.Name) %>%
  left_join(inat_annot, by = "Park.Name") %>%
  replace_na(list(park_int_uni = 0)) %>%
  mutate(num_no_annot = park_int_distinct - park_int_uni)

### now finally making a pivot_longer situation
plot_df <- inat_no %>%
  select(Park.Name, park_int_uni, num_no_annot) %>%
  pivot_longer(
    cols = c(park_int_uni, num_no_annot),
    names_to = "category",
    values_to = "interaction_richness"
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c("num_no_annot", "park_int_uni"),
      labels = c("iNaturalist",
                 "iNaturalist, annotated")
    )
  )

### now we need to create a new column that makes 
### inat + inat_annot a zero sum game

ggplot(plot_df, aes(x = Park.Name, y = interaction_richness, fill = category, pattern = category)) +
  geom_col_pattern(
    color = "white",
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45,
    pattern_density = 0.35,
    pattern_spacing = 0.04,
    pattern_key_scale_factor = 0.6
  ) +
  scale_fill_manual(values = c(
    "iNaturalist" = "#00BA38",
    "iNaturalist, annotated" = "#00BA38"
  )) +
  scale_pattern_manual(values = c(
    "iNaturalist" = "none",
    "iNaturalist, annotated" = "stripe"
  )) +
  labs(
    x = "Park",
    y = "Number of unique interactions",
    fill = "Category",
    pattern = "Category"
  ) +
  theme_minimal()


ggplot(inat_no, aes(
  x = Park.Name,
  y = park_int_distinct,
  fill = dataset
)) +
  geom_col() +
  labs(
    x = "Park",
    y = "Number of unique interactions",
    fill = "Categorization"
  ) +
  scale_fill_manual(values = c(
    "iNaturalist" = "#00BA38",
    "iNaturalist annotated" = "lightgreen"
  )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
