## Goal: Collect patent portfolio information for target company
## Approach: Compare Google Patent and Lens.org meta data

library(readr)   # load files fast & in Tibble format
library(dplyr)   # tidy data wrangling
library(tidyr)   # work with table - esp. sep. rows (like Excel text-to-column)
library(stringr) # manipulate strings


# Load manually downloaded target firm portfolio data
company_full <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/company_lens.csv")


## Explore
skimr::skim(company_full)

## Cleaning
company_full <- company_full %>% 
  # Adjust naming; refer to non-syntactic column names with backticks ``
  rename(`us_pat_no` = `Display Key`) %>% 
  # Use original US PatNo. for match with similarity
  mutate(`us_pat_no` = str_replace_all(`us_pat_no`,"[A-Z]{2}\\s([0-9]+)\\s[A-Z]2?","\\1"))

# CPC: generate seperate dataset
company_cpc <- company_full %>%
  select(`us_pat_no`,`Publication Date`,`CPC Classifications`,`IPCR Classifications`) %>% 
  # split classification column into multiple rows
  # IMPORTANT FOR SWITCHING between condence and long format of CPC classes
  # See also unwrap_cols() function in add_meta_data.R
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
company_alt <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/company_google_18.csv",
                        skip = 1) # skip the link to Google query result
head(company_alt)

# Bring "id" into US PatNo. format for match
company_alt <- company_alt %>% 
  mutate(`id` = str_replace(`id`,"[A-Z]{2}-([0-9]+)-[A-Z]2?","\\1")) %>% 
  # mutate(`id` = str_sub(`id`, start = 4, end = -4)) %>% -- doesn't work for pre-2000 patents
  rename(`us_pat_no` = `id`)

# Compare patent vectors to each other
skimr::skim(company_alt)
skimr::skim(company_cpc)

ifelse(company_alt$us_pat_no == unique(company_cpc$us_pat_no),"Yes","No")

for (i in 1:length(company_alt$us_pat_no)){
  if(company_alt$us_pat_no[i] %in% company_cpc$us_pat_no == FALSE){print(company_alt$us_pat_no[i])}
} 
# Result A: 7 patents in Google; but not in Lens DB 
# - newest 5 of which are > Nov. 2019 granted; Not applicable
# - 8991893 (Profile element connecting a vehicle pane to a water draining chamber) = Same Patent Family / "Related Patent" to 8628137
# - 9399393 (Molding element for joining a part to a fixed window pane of a vehicle) = Same Patent Family / "Related Patent" to 9073421
## --> CHECK WITH PATENT LAWYER IF INCLUDE; for now out

for (i in 1:length(company_cpc$us_pat_no)){
  if(company_cpc$us_pat_no[i] %in% company_alt$us_pat_no == FALSE){print(company_cpc$us_pat_no[i])}
} 
# Result B: 3 patents in Lens; but not in the Google DB:
# - 9259994 (Automotive glazing): cites company in description
# - 8033711 (Field bendable line voltage track lighting system): Cites company in "other Publications" 
## --> BOTH TO BE EXCLUDED

## Export Core Table to CSV for matching with similarity score file
company_cpc  <- company_cpc[!(company_cpc$us_pat_no==9259994 | company_cpc$us_pat_no==8033711),]
company_cpc %>% 
  write_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/company_clean_cpc.csv",
            col_names = T)

company_full <- company_full[!(company_full$us_pat_no==9259994 | company_full$us_pat_no==8033711),]

# TODO: Adjust with new patents provided -- 
# [ATTN] MANUALLY CHANGED FILE IN EXCEL - ADDED CPC Classes + 4-digit distinct count  
company_full %>% 
  select(`us_pat_no`,`Kind`,`Title`,`Abstract`,`Publication Date`,`Application Date`,
         `Priority Numbers`,`Owners`,`Inventors`,`Applicants`,`CPC Classifications`,
         `Cites Patent Count`,`Cited by Patent Count`,`Simple Family Size`) %>% 
  write_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/company_clean_full.csv",
            col_names = T)


## Visualize CPC Patent Portfolio
# View CPC levels
print(company_cpc %>% count(`CPC4_subclass`, sort = T), n= 25)
print(company_cpc %>% count(`CPC_group`, sort = T), n= 25)
company_cpc

# Use only individual CPC classes per patent
company_cpc %>% 
  select(`us_pat_no`,`CPC4_subclass`) %>% 
  group_by(`us_pat_no`) %>% 
  count(`CPC4_subclass`) %>% 
  select(-`n`) -> company_CPC4_summary

### Save/Load Point ###
write_csv(as.data.frame(company_CPC4_summary),"~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_cpc_split2.csv")
company_CPC4_summary <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_cpc_split2.csv")

# Put file (that was overwritten) into proper format
company_CPC4_summary %>% 
  # Remove extra one (NEW STEP): 15 -> 14
  filter(patent_no != 5921663) %>% 
  # Adjust with following naming 
  rename(Var1 = port_cpc_group_id) %>% 
  # Create 3-digit CPC class
  mutate(group = str_sub(Var1,1,3)) %>%   
  # Add Freq
  group_by(group, Var1) %>%
  count(Var1) %>%  
  rename(Freq = n) -> CPC4_freq

## Desript. Analysis
# portfolio stats
company_CPC4_summary %>% 
  # Remove extra one (NEW STEP): 15 -> 14
  filter(patent_no != 5921663) %>% 
  # Adjust with following naming 
  rename(Var1 = port_cpc_group_id) %>% 
  # Create 3-digit CPC class
  mutate(group = str_sub(Var1,1,3)) %>%   
  # Add Freq
  group_by(patent_no) %>%
  summarize(class3 = length(unique(group)),
            subclass4 = length(unique(Var1))) -> portfolio_stats
            # Var1 = count(Var1))
summary(portfolio_stats)

# Overview
company_CPC4_summary %>% 
  # Remove extra one (NEW STEP): 15 -> 14
  filter(patent_no != 5921663) %>% 
  # Adjust with following naming 
  rename(Var1 = port_cpc_group_id) %>% 
  # Create 3-digit CPC class
  mutate(group = str_sub(Var1,1,3)) %>%   
  # Add Freq
  group_by(patent_no) %>%
  summarize(class3 = length(unique(group)),
            subclass4 = length(unique(Var1))) -> portfolio_stats

## Plot It ####
# CPC4_freq <- as.data.frame(table(company_CPC4_summary["CPC4_subclass"]))
# Visulaize portfolio as basic treemap (https://www.r-graph-gallery.com/235-treemap-with-subgroups.html)
library(treemap)

group <- CPC4_freq$Var1 # 4-digit class
value <- CPC4_freq$Freq # no. of occurences
data <- data.frame(group,value) # combined matrix

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

group <- CPC4_freq$group
subgroup <- CPC4_freq$Var1
data <- data.frame(group,subgroup,value)

p <- treemap(data,
             index=c("group","subgroup"),
             vSize="value",
             type="index",
             title = "Target Firm - US Patent Portfolio - CPC Split",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)            
write_csv(as.data.frame(data),"~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_cpc_split.csv")

## Print it with CPC3-Class Description // some duplication problems
CPC4_freq <- CPC4_freq %>% 
  mutate(group = str_replace(group,"B29 - Working of plastics - Vehicles in general","B29 - Working of plastics")) %>% 
  mutate(group = str_replace(group,"B60 - Vehicles in general - Working of plastics","B60 - Vehicles in general"))

group <- CPC4_freq$group
data <- data.frame(group,subgroup,value)

p <- treemap(data,
             index=c("group","subgroup"),
             vSize="value",
             type="index",
             title = "Target Firm - US Patent Portfolio - CPC Split",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)    

# make it interactive ("rootname" becomes the title of the plot):
library(d3treeR)
CPC4_freq <- CPC4_freq %>% 
  mutate(`group` = str_sub(`Var1`,1,3))
inter <- d3tree2( p ,  rootname = "Company US Patent Portfolio" )
