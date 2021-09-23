## Necessary Stuff
library(readr)   # load files fast & in Tibble format
library(dplyr)   # tidy data wrangling
library(tidyr)   # work with table - esp. sep. rows (like Excel text-to-column)
library(stringr) # manipulate strings

# WD for Saves
setwd("~/Master Thesis/IP_Similarity_Thesis")

# Load as Tibble (not DF) for Tidyverse
elkamet_full <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_lens_13.csv")


## Explore
str(elkamet_full)
skimr::skim(elkamet_full)

## Cleaning
elkamet_full <- elkamet_full %>% 
  # Adjust naming; refer to non-syntactic column names with backticks ``
  rename(`us_pat_no` = `Display Key`) %>% 
  # Use original US PatNo. for match with similarity
  mutate(`us_pat_no` = str_replace_all(`us_pat_no`,"[A-Z]{2}\\s([0-9]+)\\s[A-Z]2?","\\1"))

# CPC: generate seperate dataset
elkamet_cpc <- elkamet_full %>%
  select(`us_pat_no`,`Publication Date`,`CPC Classifications`,`IPCR Classifications`) %>% 
  # split classification column into multiple rows
  separate_rows(`CPC Classifications`, sep = ";;") %>% 
  # add two versions of details for cpc and move before
  mutate(`CPC4_subclass` = str_sub(`CPC Classifications`, end = 4),.before = `CPC Classifications`) %>% 
  mutate(`CPC6_group` = str_sub(`CPC Classifications`, end = 6),.before = `CPC Classifications`) %>% 
  # clean the group-level string
  mutate(`CPC6_group` = str_remove(`CPC6_group`, "/")) %>% 
  # adjust column naming
  rename(`CPC_max_subgroup` = `CPC Classifications`) %>% 
  rename(`granted` = `Publication Date`) %>% 
  rename(`IPC_classes` = `IPCR Classifications`)


# Seperate Assignees -- for what? why?
# separate_rows()


## Test: Match with other CSV
elkamet_alt <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_google_18.csv",
                        skip = 1) # skip the link to Google query result
head(elkamet_alt)

# Bring "id" into US PatNo. format for match
elkamet_alt <- elkamet_alt %>% 
  mutate(`id` = str_replace(`id`,"[A-Z]{2}-([0-9]+)-[A-Z]2?","\\1")) %>% 
  # mutate(`id` = str_sub(`id`, start = 4, end = -4)) %>% -- doesn't work for pre-2000 patents
  rename(`us_pat_no` = `id`)

# Compare patent vectors to each other
skimr::skim(elkamet_alt)
skimr::skim(elkamet_cpc)

ifelse(elkamet_alt$us_pat_no == unique(elkamet_cpc$us_pat_no),"Yes","No")

for (i in 1:length(elkamet_alt$us_pat_no)){
  if(elkamet_alt$us_pat_no[i] %in% elkamet_cpc$us_pat_no == FALSE){print(elkamet_alt$us_pat_no[i])}
} 
# Result A: 7 patents in Google; but not in Lens DB 
# - newest 5 of which are > Nov. 2019 granted; Not applicable
# - 8991893 (Profile element connecting a vehicle pane to a water draining chamber) = Same Patent Family / "Related Patent" to 8628137
# - 9399393 (Molding element for joining a part to a fixed window pane of a vehicle) = Same Patent Family / "Related Patent" to 9073421
## --> CHECK WITH PATENT LAWYER IF INCLUDE; for now out

for (i in 1:length(elkamet_cpc$us_pat_no)){
  if(elkamet_cpc$us_pat_no[i] %in% elkamet_alt$us_pat_no == FALSE){print(elkamet_cpc$us_pat_no[i])}
} 
# Result B: 3 patents in Lens; but not in the Google DB:
# - 9259994 (Automotive glazing): cites Elkamet in description
# - 8033711 (Field bendable line voltage track lighting system): Cites Elkamet in "other Publications" 
## --> BOTH TO BE EXCLUDED

## Export Core Table to CSV for matching with similarity score file
elkamet_cpc  <- elkamet_cpc[!(elkamet_cpc$us_pat_no==9259994 | elkamet_cpc$us_pat_no==8033711),]
elkamet_cpc %>% 
  write_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_clean_cpc.csv",
            col_names = T)

elkamet_full <- elkamet_full[!(elkamet_full$us_pat_no==9259994 | elkamet_full$us_pat_no==8033711),]
elkamet_full %>% 
  select(`us_pat_no`,`Kind`,`Title`,`Abstract`,`Publication Date`,`Application Date`,
         `Priority Numbers`,`Owners`,`Inventors`,`Applicants`,
         `Cites Patent Count`,`Cited by Patent Count`,`Simple Family Size`) %>% 
  write_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_clean_full.csv",
            col_names = T)


## Visualize CPC Patent Portfolio
# View CPC levels
print(elkamet_cpc %>% count(`CPC4_subclass`, sort = T), n= 25)
print(elkamet_cpc %>% count(`CPC_group`, sort = T), n= 25)
elkamet_cpc

# Use only individual CPC classes per patent
elkamet_cpc %>% 
  select(`us_pat_no`,`CPC4_subclass`) %>% 
  group_by(`us_pat_no`) %>% 
  count(`CPC4_subclass`) %>% 
  select(-`n`) -> elkamet_CPC4_summary

# Overly complicated approach
# unique(elkamet_CPC4_summary$CPC4_subclass)
# for (i in length(elkamet_CPC4_summary$us_pat_no)){
#   str_count(elkamet_CPC4_summary$CPC4_subclass[i], pattern = group[i])
# }

CPC4_freq <- as.data.frame(table(elkamet_CPC4_summary["CPC4_subclass"]))

# Visulaize portfolio as basic treemap (https://www.r-graph-gallery.com/235-treemap-with-subgroups.html)
library(treemap)

group <- CPC4_freq$Var1
value <- CPC4_freq$Freq
data <- data.frame(group,value)

# treemap
treemap(data,
        index="group",
        vSize="value",
        type="index"
)

# More detailed, interactive treemap

# For installation, dev access & dependencies are needed
# devtools::install_github("timelyportfolio/d3treeR")
# install.packages("viridisLite")
# install.packages("XML")

library(d3treeR)
CPC4_freq <- CPC4_freq %>% 
  mutate(`group` = str_sub(`Var1`,1,3))

group <- CPC4_freq$group
subgroup <- CPC4_freq$Var1
data <- data.frame(group,subgroup,value)

p <- treemap(data,
             index=c("group","subgroup"),
             vSize="value",
             type="index",
             title = "Elkamet US Patent Portfolio - CPC Split",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)            

# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2( p ,  rootname = "Elkamet US Patent Portfolio" )
