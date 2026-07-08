#### Making figures ####

#############################################################################
### Load packages 

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpattern)

#############################################################################




#############################################################################
### Read in data

# Combined data frame that interaction richness and abundance per park for 
# each data set
combined_df <- readRDS("Data/combined_interaction_data.RDS")

# Filtered and harmonized field data 
field_filt <- read.csv("Data/Field Observation Data/filtered_field_data.csv")

# Filtered and harmonized iNat data annotated in lab
inat_new <- read.csv("Data/iNat_Data/filtered_and_harmonized_iNat_data.csv")

# Filtered and harmonized iNat data annotated on iNat
inat_annot_new <- read.csv("Data/iNat_Data/filtered_and_harmonized_annotated_iNat_data.csv")

#############################################################################




#############################################################################
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

#############################################################################






#############################################################################
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





### Figure: Accumulation of interaction richness documented by iNat photos (tagged
#   in lab and tagged in iNat) compared to interaction richness documented in the 
#   field. Observations are randomly reordered before calculating interaction 
#   richness

# Field-collected interaction richness 
field_interaction_richness <- field_filt %>%
  distinct(Interaction.ID) %>%
  nrow()
field_interaction_richness

# Combine iNat and field interactions
combined_interactions <- bind_rows(
  field_filt %>% select(Interaction.ID),
  inat_new %>% select(Interaction.ID)
)

# Write a function to re-order observations before calculating cumulative 
# interaction richness. 500 randomizations (reshuffles). 
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

# Function to run separate accumulation curves for each dataset
run_accumulation <- function(df, dataset_name, n_reps = 500){
  accum_results <- bind_rows(
    lapply(
      seq_len(n_reps),
      function(x){
        accum_fun(df) %>%
          mutate(
            Replicate = x,
            Dataset = dataset_name
          )
      }
    )
  )
  
  accum_results %>%
    group_by(Dataset, Observation_Number) %>%
    summarise(
      Mean_Richness = mean(Richness),
      Lower_CI = quantile(Richness, 0.025),
      Upper_CI = quantile(Richness, 0.975),
      .groups = "drop"
    )
}

# Run the function for iNat data annotated in lab 
accum_results_iNat_annotation_in_lab <- bind_rows(
  lapply(
    seq_len(n_reps),
    function(x) {
      accum_fun(inat_new) %>%
        mutate(Replicate = x)
    }
  )
)


# Run for all four datasets: field data only, iNat data annotated in lab only, 
# iNat data annotated in iNat only, and iNat/field combined
field_summary <- run_accumulation(
  field_filt,
  "Field Data")

inat_lab_summary <- run_accumulation(
  inat_new,
  "Lab Annotated iNaturalist Data")

inat_online_annotation_summary <- run_accumulation(
  inat_annot_new,
  "iNaturalist Annotated Data")

combined_summary <- run_accumulation(
  combined_interactions,
  "Combined")

# Combine results for accumulation curves for all datasets 
accum_summary_all <- bind_rows(
  field_summary,
  inat_lab_summary,
  inat_online_annotation_summary,
  combined_summary)

# Subset of data with just iNat accumulation curves
accum_summary_iNat <- bind_rows(
  inat_lab_summary,
  inat_online_annotation_summary
)

# Plot accum curves for iNat annotated in lab and annotated on iNat, with a single
# line for the interaction richness recorded in field 
ggplot(accum_summary_iNat, aes(x = Observation_Number, y = Mean_Richness)) +
  geom_ribbon(
    aes(ymin = Lower_CI, ymax = Upper_CI, fill = Dataset, group = Dataset),
    alpha = 0.2,
    color = NA) +
  geom_line(aes(color = Dataset, group = Dataset), linewidth = 1) +
  geom_hline(
    aes(yintercept = field_interaction_richness, color = "Field Data"),
    linetype = "dashed",
    linewidth = 1) +
  scale_color_manual(
    breaks = c(
      "Lab Annotated iNaturalist Data",
      "iNaturalist Annotated Data",
      "Field Data"),
    values = c(
      "Lab Annotated iNaturalist Data" = "black",
      "iNaturalist Annotated Data" = "steelblue",
      "Field Data" = "red"))+
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linetype = c("solid", "solid", "dashed"),
        linewidth = 1
      )))+
  scale_fill_manual(
    values = c(
      "Lab Annotated iNaturalist Data" = "black",
      "iNaturalist Annotated Data" = "steelblue")) +
  labs(
    x = "Number of Observations",
    y = "Interaction Richness",
    color = "") +
  theme_classic()



### Figure: Four rarefaction curves, one for field data, one for iNat data 
#   annotated in lab, one for iNat data annotated in iNat, and one for 
#   iNat and field data combined

ggplot(accum_summary_all, aes(x = Observation_Number, y = Mean_Richness)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),alpha = 0.2) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dataset, nrow = 1, scales = "free_x") +
  labs(x = "Number of Observations", y = "Interaction Richness") +
  theme_classic()


# More colorful option: 
ggplot(accum_summary_all, aes(x = Observation_Number, y = Mean_Richness)) + 
  geom_ribbon( aes( ymin = Lower_CI, ymax = Upper_CI ), 
               fill = "steelblue", alpha = 0.2, color = NA ) +  
  geom_line( linewidth = 1, color = "black" ) +  
  facet_wrap( ~ Dataset, nrow = 1, scales = "free_x" ) + 
  labs( x = "Number of Observations", y = "Interaction Richness" ) + 
  theme_classic()



### Figure: Interaction richness for each order in each data set. 


