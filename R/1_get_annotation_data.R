# Get annotation data 
# 16 June 2026

library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)

# read in annotation data
inat <- read.csv("Data/iNat_Data/interactions_data_4_4_2025.csv")

# get list of observation ID's
inat$id

# split IDs into groups of 200
id_chunks <- split(
  inat$id,
  ceiling(seq_along(inat$id) / 200)
)

get_obs_batch <- function(ids){
  
  call <- paste0(
    "https://api.inaturalist.org/v1/observations?",
    "id=", paste(ids, collapse = ","),
    "&per_page=200"
  )
  
  GET(call) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    pluck("results") %>%
    as.data.frame()
}

obs_list <- map(id_chunks, get_obs_batch)

obs <- bind_rows(obs_list)

ofvs_df <- map2_dfr(
  obs$id,
  obs$ofvs,
  ~{
    if(is.null(.y) || nrow(.y) == 0) return(NULL)
    
    .y %>%
      mutate(observation_id = .x)
  }
)

# are there any observations with more than one observation field?
nrow(ofvs_df)
length(unique(ofvs_df$id))
# no

# now left join with annotation data
inat_annotated <- left_join(inat, ofvs_df, by=c("id"="observation_id"))

# export annotation data
saveRDS(inat_annotated, "Data/iNat_Data/interactions_data_annotated.RDS")
