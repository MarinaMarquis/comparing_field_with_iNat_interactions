# Analyze data and create figures

# Load packages 
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(httr)
library(jsonlite)
library(purrr)


# Read in data ------------------------------------------------------------

# Combined data frame that interaction richness and abundance per park for 
# each data set
combined_df <- readRDS("Data/combined_interaction_data.RDS")

# Filtered and harmonized field data 
field_filt <- read.csv("Data/Field Observation Data/filtered_field_data.csv")

# Filtered and harmonized iNat data annotated in lab
inat_new <- read.csv("Data/iNat_Data/filtered_and_harmonized_iNat_data.csv")

# Filtered and harmonized iNat data annotated on iNat
inat_annot_new <- read.csv("Data/iNat_Data/filtered_and_harmonized_annotated_iNat_data.csv")


# Summary statistics ------------------------------------------------------

#total interactions
sum(unique(combined_df$park_int_total))

# unique interactions
length(unique(combined_df$Interaction.ID))

#unique interactions per dataset
tot_unique <- combined_df %>%
  group_by(park_overlap) %>%
  summarize(tot_uni = length(unique(Interaction.ID)))
tot_unique

# total interactions within each dataset
totals <- combined_df %>%
  group_by(dataset) %>%
  summarize(total_n = sum(unique(park_int_total)))
totals

totals_combined <- combined_df %>%
  group_by(park_overlap)%>%
  summarize(total_n = sum(unique(park_int_total)))
totals_combined

# Figures -----------------------------------------------------------------

## Quick density plots for total interactions
ggplot(combined_df, aes(x = n, fill = dataset, color = dataset)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  facet_wrap(~ Park.Name, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "iNaturalist" = "green3",
      "Field Collection" = "blue2",
      "iNaturalist annotated" = "red"
    )
  ) +
  scale_color_manual(
    values = c(
      "iNaturalist" = "green3",
      "Field Collection" = "blue2",
      "iNaturalist annotated" = "red"
    )
  ) +
  labs(
    x = "Interaction abundance",
    y = "Density",
    fill = "Dataset",
    color = "Dataset"
  ) +
  theme_classic()


## Now trying the plot that Corey wanted
park_overlap_plot <- combined_df %>%
  distinct(Park.Name, park_overlap, park_overlap_count) %>%
  mutate(park_overlap=factor(park_overlap, levels=c("iNaturalist only", "Shared", "Fieldwork only")))

ggplot(park_overlap_plot, aes(
  x = Park.Name,
  y = park_overlap_count,
  fill = park_overlap
)) +
  scale_fill_manual(
    values = c(
      "iNaturalist only" = "#33A02C",  # green
      "Fieldwork only" = "#3567D7",    # blue
      "Shared" = "#19D4D9"             # cyan/teal
    )
  ) +
  geom_col() +
  labs(
    x = "Greenspace",
    y = "Number of unique interactions",
    fill = "Source"
  ) +
  theme_bw(base_size=18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid=element_blank()
  )

ggsave("Figures/inat_shared_field_comparison_by_park.png", height=6, width=8, units="in")

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
      labels = c("iNaturalist manually\ncurated dataset",
                 "iNaturalist annotations")
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
    "iNaturalist manually\ncurated dataset" = "#33A02C",
    "iNaturalist annotations" = "#33A02C"
  )) +
  scale_pattern_manual(values = c(
    "iNaturalist manually\ncurated dataset" = "none",
    "iNaturalist annotations" = "stripe"
  )) +
  labs(
    x = "Park",
    y = "Number of unique interactions",
    fill = "Category",
    pattern = "Category"
  ) +
  theme_bw(base_size=18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid=element_blank()
  )

ggsave("Figures/inat_and_inat_annotated_comparison_by_park.png", height=7, width=9, units="in")





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
  "Fieldwork Data")

inat_lab_summary <- run_accumulation(
  inat_new,
  "iNaturalist Manually\nCurated Dataset")

inat_online_annotation_summary <- run_accumulation(
  inat_annot_new,
  "iNaturalist Annotations")

combined_summary <- run_accumulation(
  combined_interactions,
  "Combined")

# Combine results for accumulation curves for all datasets 
accum_summary_all <- bind_rows(
  field_summary,
  inat_lab_summary,
  inat_online_annotation_summary,
  combined_summary) %>%
  mutate(
    Dataset = factor(
      Dataset,
      levels = c(
        "Combined",
        "Fieldwork Data",
        "iNaturalist Manually\nCurated Dataset",
        "iNaturalist Annotations"
      )
    )
  )

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
    aes(yintercept = field_interaction_richness, color = "Fieldwork Data"),
    linetype = "dashed",
    linewidth = 1) +
  scale_color_manual(
    breaks = c(
      "iNaturalist Manually\nCurated Dataset",
      "iNaturalist Annotations",
      "Fieldwork Data"),
    values = c(
      "iNaturalist Manually\nCurated Dataset" = "black",
      "iNaturalist Annotations" = "steelblue",
      "Fieldwork Data" = "red"))+
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linetype = c("solid", "solid", "dashed"),
        linewidth = 1
      )))+
  scale_fill_manual(
    values = c(
      "iNaturalist Manually\nCurated Dataset" = "black",
      "iiNaturalist Annotations" = "steelblue")) +
  labs(
    x = "Number of Observations",
    y = "Interaction Richness",
    color = "") +
  theme_classic()



### Figure: Four rarefaction curves, one for field data, one for iNat data 
#   annotated in lab, one for iNat data annotated in iNat, and one for 
#   iNat and field data combined

ggplot(accum_summary_all, aes(x = Observation_Number, y = Mean_Richness, color=Dataset)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill=Dataset),alpha = 0.2, color=NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Combined" = "#19D4D9",
      "Fieldwork Data" = "#3567D7",
      "iNaturalist Annotations" = "#E4A924",
      "iNaturalist Manually\nCurated Dataset" = "#33A02C"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Combined" = "#19D4D9",
      "Fieldwork Data" = "#3567D7",
      "iNaturalist Annotated Data" = "#E4A924",
      "iNaturalist Manually\nCurated Dataset" = "#33A02C"
    )
  ) +
  facet_wrap(~ Dataset, nrow = 1) +
  labs(x = "Number of Observations", y = "Interaction Richness") +
  theme_bw(base_size=18) +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none")

ggsave("Figures/rarefraction_curves_option1.png", height=5, width=10, units="in")


# More colorful option: 
ggplot(accum_summary_all, aes(x = Observation_Number, y = Mean_Richness)) + 
  geom_ribbon( aes( ymin = Lower_CI, ymax = Upper_CI ), 
               fill = "steelblue", alpha = 0.2, color = NA ) +  
  geom_line( linewidth = 1, color = "black" ) +  
  facet_wrap( ~ Dataset, nrow = 1) + 
  labs( x = "Number of Observations", y = "Interaction Richness" ) + 
  theme_classic()



ggplot(accum_summary_all, 
       aes(x = Observation_Number, 
           y = Mean_Richness, 
           color = Dataset)) +
  geom_ribbon(
    aes(ymin = Lower_CI, ymax = Upper_CI, fill = Dataset),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    values = c(
      "Combined" = "#19D4D9",
      "Fieldwork Data" = "#3567D7",
      "iNaturalist Annotations" = "#E4A924",
      "iNaturalist Manually\nCurated Dataset" = "#33A02C"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Combined" = "#19D4D9",
      "Fieldwork Data" = "#3567D7",
      "iNaturalist Annotated Data" = "#E4A924",
      "iNaturalist Manually\nCurated Dataset" = "#33A02C"
    )
  ) +
  labs(
    x = "Number of Observations",
    y = "Interaction Richness",
    color = "Dataset",
    fill = "Dataset"
  ) +
  guides(fill = "none") +
  theme_bw(base_size=18) +
  theme(panel.grid = element_blank())


ggsave("Figures/rarefraction_curves_option2.png", height=6, width=8, units="in")







### Figure: Interaction richness for each pollinator order in each data set. 

## First we need to create order columns for all pollinator species in both iNat
#  data sets

# Obtain species names from iNat data sets 
species <- bind_rows(
  inat_new %>% select(Taxon.name),
  inat_annot_new %>% select(Taxon.name)
) %>%
  distinct() %>%
  arrange(Taxon.name)

# Run a function to get insect orders (for each species in our data set) 
# from iNaturalists 

get_order <- function(species_name){
  search_call <- paste0(                         # Search for the species
    "https://api.inaturalist.org/v1/taxa?q=",
    URLencode(species_name),
    "&rank=species&per_page=1"
  )
  
  search <- GET(search_call) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE)

  if(length(search$results) == 0){             # NA if no match 
    return(
      tibble(
        Taxon.name = species_name,
        Insect.Order = NA_character_
      )
    )
  }
  
  ancestor_ids <- search$results[[1]]$ancestor_ids
  
  ancestor_call <- paste0(
    "https://api.inaturalist.org/v1/taxa/",
    paste(ancestor_ids, collapse = ",")
  )
  
  ancestors <- GET(ancestor_call) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = TRUE)
  
  order <- ancestors$results %>%
    as_tibble() %>%
    filter(rank == "order") %>%
    pull(name)
  
  tibble(
    Taxon.name = species_name,
    Insect.Order = if(length(order) == 0) NA_character_ else order[1]
  )
}


order_lookup <- map_dfr(
  species$Taxon.name,
  get_order
)
print(order_lookup)

# Save it
write_csv(
  order_lookup,
  "Data/iNat_Data/order_lookup.csv"
)

# Run it back in 
order_lookup <- read_csv("Data/iNat_Data/order_lookup.csv")


# Join order info with the iNat datasets 
inat_new <- inat_new %>%
  left_join(order_lookup, by = "Taxon.name")

inat_annot_new <- inat_annot_new %>%
  left_join(order_lookup, by = "Taxon.name")


# Make sure it worked 
table(inat_new$Insect.Order, useNA = "ifany")
table(inat_annot_new$Insect.Order, useNA = "ifany")

# New combined df with iNat and field interactions
combined_order_df <- bind_rows(
  field_filt %>%
    select(Interaction.ID, Insect.Order),
  
  inat_new %>%
    select(Interaction.ID, Insect.Order),
  
  inat_annot_new %>%
    select(Interaction.ID, Insect.Order)
) %>%
  distinct()

# Function to summarize interaction richness by insect order (how many interactions
# with each insect order)
summarize_order_richness <- function(df, dataset_name){
  df %>%
    distinct(Interaction.ID, Insect.Order) %>%
    group_by(Insect.Order) %>%
    summarise(
      Interaction_Richness = n(),
      .groups = "drop"
    ) %>%
    mutate(Dataset = dataset_name)
}

# Summarize each data set
field_order <- summarize_order_richness(
  field_filt,
  "Fieldwork Data"
)

inat_lab_order <- summarize_order_richness(
  inat_new,
  "iNaturalist Manually Curated Dataset"
)

inat_annotation_order <- summarize_order_richness(
  inat_annot_new,
  "iNaturalist Annotations"
)

combined_order <- summarize_order_richness(
  combined_order_df,
  "Combined"
)

# Plot function
plot_order_richness <- function(df, fill_color){
  ggplot(df, aes(x = Insect.Order, y = Interaction_Richness) ) +
    geom_col(fill = fill_color) +
    labs(
      x = "Pollinator Order",
      y = "Pollinator Interaction Richness") +
    theme_bw(base_size = 18) +
    theme(panel.grid = element_blank())
}


# Plot the figures
field_order_plot <- plot_order_richness(
  field_order,
  "#3567D7"
)
field_order_plot

inat_lab_plot <- plot_order_richness(
  inat_lab_order,
  "#33A02C"
)
inat_lab_plot

inat_annotation_plot <- plot_order_richness(
  inat_annotation_order,
  "#E4A924"
)
inat_annotation_plot

combined_plot <- plot_order_richness(
  combined_order,
  "#19D4D9"
)
combined_plot

## Place all these plots in one figure: 

# Combine them 
order_summary_all <- bind_rows(
  field_order,
  inat_lab_order,
  inat_annotation_order,
  combined_order
) %>%
  mutate(
    Dataset = factor(
      Dataset,
      levels = c(
        "Fieldwork Data",
        "iNaturalist Manually Curated Dataset",
        "iNaturalist Annotations",
        "Combined"
      )
    )
  )

# Combined plot 
ggplot(
  order_summary_all,
  aes(
    x = Insect.Order,
    y = Interaction_Richness,
    fill = Dataset
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Dataset, nrow = 2) +
  scale_fill_manual(
    values = c(
      "Fieldwork Data" = "#3567D7",
      "iNaturalist Manually Curated Dataset" = "#33A02C",
      "iNaturalist Annotations" = "#E4A924",
      "Combined" = "#19D4D9"
    )
  ) +
  labs(
    x = "Pollinator Order",
    y = "Pollinator Interaction Richness"
  ) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Save it 
ggsave(
  "Figures/pollinator_order_interaction_richness.png",
  height = 8,
  width = 10,
  units = "in"
)
