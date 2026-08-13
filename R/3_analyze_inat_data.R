# Analyze iNaturalist dataset

# load packages
library(tidyverse)

# read in data
inat_raw <- readRDS("Data/iNat_Data/inat_cleaned_w_annotation.RDS")

inat_raw <- read_csv("Data/iNat_Data/filtered_and_harmonized_iNat_data.csv")

# filter to only plant identifications, where we were over 80% confident
inat <- inat_raw %>%
  filter(FLW_ID_Conf %in% c(">80%", "100%"))

# what percentage was filtered out?
paste("Percentage of observations with uncertain plant ID:", paste(round(100-(nrow(inat)/nrow(inat_raw)*100), 3), "%", sep=""), spe=" ")

# Summary statistics ------------------------------------------------------

# how many parks were examined?
length(unique(inat$Park.Name))
# 8

# how many iNaturalist observations
nrow(inat)

# how many of the observations had annotations on iNaturalist?
paste("Number with annotations:", nrow(inat %>% filter(complete.cases(taxon.name))), sep=" ")
paste("Percentage with annotations:", 
      paste(round(nrow(inat %>% filter(complete.cases(taxon.name)))/nrow(inat)*100, 3), "%", sep=""), 
      sep=" ")

# what percentage of manual annotation had plants identified to species?
paste("Percentage of plants identified to species manually:", 
      paste(round(nrow(inat_raw %>% filter(complete.cases(Flower_species),
                                           FLW_ID_Conf %in% c(">80%", "100%")))/nrow(inat)*100, 3), "%", sep=""), 
      sep=" ")

# what percentage of iNaturalist annotations were identified to species?
paste("Percentage of plants identified to species by iNaturalist users:", 
      paste(round(nrow(inat %>% filter(taxon.rank=="species"))/nrow(inat %>% filter(complete.cases(taxon.rank)))*100, 3), "%", sep=""), 
      sep=" ")

# how many users are contributing to iNaturalist annotations
inat %>%
  filter(complete.cases(taxon.name)) %>%
  group_by(user.id) %>% 
  summarize(count=n(),
            percentage=n()/nrow(inat %>%
                                  filter(complete.cases(taxon.name)))*100,
            user.name=first(user.name)) %>%
  arrange(desc(count))

# What species are being annotated by iNaturalist users.
# 1. Unique species names
inat %>% 
  filter(taxon.rank == "species") %>% 
  pull(taxon.name) %>% 
  unique()

inat %>% 
  filter(taxon.rank == "species") %>% 
  pull(taxon.preferred_common_name) %>% 
  unique()

# how many of the plant species that iNaturalist users identified agree with our manual identification
inat_user_manu <- inat %>%
  filter(complete.cases(taxon.rank),
         complete.cases(Flower_species),
         taxon.rank=="species") %>%
  mutate(manual_plant_id=paste(Flower_Genus, Flower_species, sep=" "),
         agreement=ifelse(manual_plant_id==taxon.name, "Yes", "No"))

paste("Precentage of species with identification agreement:", 
      paste(round(nrow(inat_user_manu %>% filter(agreement=="Yes"))/nrow(inat_user_manu)*100, 3), "%", sep=""), sep=" ")

# examine the observations where there was disagreements
disagreements <- inat_user_manu %>% 
  filter(agreement=="No") %>%
  select(taxon.name, manual_plant_id, URL) %>%
  rename(`Identification from iNaturalist Users`=taxon.name,
         `Identification from Manual Effort` = manual_plant_id)
disagreements

write_csv(disagreements, "Data/Output_Tables/plant_identification_disagreements.csv")

# Figures -----------------------------------------------------------------

# Figure of count of plant species by professionals versus iNaturalist users

inat_annotations_count <- inat %>%
  filter(complete.cases(taxon.rank),
         taxon.rank=="species") %>%
  count(taxon.name, name = "count") %>%
  mutate(
    percentage = 100 * count / sum(count),
    group = "iNaturalist Users"
  ) %>%
  rename(taxon=taxon.name)

# expert count
inat_expert_count <- inat %>%
  filter(complete.cases(Flower_species)) %>%
  mutate(taxon = paste(Flower_Genus, Flower_species, sep = " ")) %>%
  count(taxon, name = "count") %>%
  mutate(
    percentage = 100 * count / sum(count),
    group = "Expert"
  ) 

inat_expert_annotations_comp <- rbind(inat_expert_count, inat_annotations_count)

# compare which group had the higher percentage
plot_dat <- inat_expert_annotations_comp %>%
  select(taxon, group, percentage) %>%
  pivot_wider(
    names_from = group,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(group=ifelse(`iNaturalist Users`==0, "Plants Identified\nby Manual Effort Only", "Plants Identified\nby Both Groups"))

ggplot(plot_dat,
       aes(x = Expert,
           y = `iNaturalist Users`,
           color=group,
           shape=group)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey50",
    lwd=1.5
  ) +
  geom_point(size=4) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  coord_equal() +
  scale_color_manual(
    values = c(
      "Plants Identified\nby Manual Effort Only" = "blue3",
      "Plants Identified\nby Both Groups" = "green4"
    )
  ) +
  scale_shape_manual(
    name = "Plant identification source",
    values = c(
      "Plants Identified\nby Manual Effort Only" = 17,  # triangle
      "Plants Identified\nby Both Groups" = 16     # circle
    )
  ) +
  guides(
    color = guide_legend(override.aes = list(shape = c(16, 17))),
    shape = "none"
  ) +
  theme_classic(base_size = 18) +
  labs(
    x = "Manual Annotation %",
    y = "iNaturalist Annotation %",
    color= ""
  )

ggsave("Figures/iNat_vs_manual_annotation.jpeg", height=6, width=9, units="in")

# how many species were only identified by experts
paste("Species only identified by experts:", length(plot_dat %>% filter(`iNaturalist Users`==0) %>% pull(taxon) %>% unique()), sep=" ")

# max count of plant identified only by expert
count_dat <- inat_expert_annotations_comp %>%
  select(taxon, group, count) %>%
  pivot_wider(
    names_from = group,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(group=ifelse(`iNaturalist Users`==0, "Plants Identified\nby Manual Effort Only", "Plants Identified\nby Both Groups"))


paste("Maximum count of plants identified by expert but not iNaturalist user:", count_dat %>% filter(`iNaturalist Users`==0) %>% pull(Expert) %>% max(), sep=" ")
paste("Maximum percentage of plants identified by expert but not iNaturalist user:", plot_dat %>% filter(`iNaturalist Users`==0) %>% pull(Expert) %>% max(), sep=" ")
