## Goal: Add meta data (CPC, Assingee) for "most similar" patents of portfolio
## Approach: Access USPTO's patentview API via R package

#install.packages("patentsview")
library(patentsview)
library(readr)
library(dplyr)
library(tidyr)
library(unheadr) # un-wrapping columns; opposite of seperate_rows() to change 
                 # between condence and long format of CPC classes


# Read output from B_semantic_sim.R 
sim_pats <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_unique.csv")


#### 1. Extract Unique patent no. ####

# Add our portfolio & also get meta data for these (if missing)
target     <- c(10000000,
                10000001
)

target <- tibble(target)
colnames(target) <- "similar_no"
sim_full <- rbind(sim_pats1[,3],sim_pats2[,3],target)

# Filter unique patents for metadata search
sim_unique <- sim_full %>%
  select(`similar_no`) %>% 
  unique()

# Normally: 865 unique; with target: 872 -> 7 that are not similar to any
target_not_sim <- as.data.frame(sim_unique$similar_no[866:872])

# Save prelimiary results
write_csv(sim_unique, "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_unique.csv",
          col_names = T) #, append = F)
write_csv(target_not_sim, "~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_notin_mostsim.csv",
          col_names = T) #, append = F)


#### Get META information seperately // Extendable for other categories ####
## Define empty objects for results
# Assignee: Empty list for API results
asgn <- as.list(rep(NA, length(sim_pats$similar_no)))
names(asgn) <- sim_pats$similar_no

# Assignee: Emptry matrix for dplr results
asgn_res <- as.data.frame(matrix(ncol=2, nrow=length(sim_pats$similar_no)))
asgn_res[1] <- sim_pats$similar_no
colnames(asgn_res) <- c("similar_no","assignee")

# CPC: Empty list for API results
cpc_api <- as.list(rep(NA, length(sim_pats$similar_no)))
names(cpc_api) <- sim_pats$similar_no

# CPC: Emptry matrix for dplr results
cpc <- as.data.frame(matrix(ncol=3, nrow=length(sim_pats$similar_no)))
cpc[1] <- sim_pats$similar_no
colnames(cpc) <- c("similar_no","no_distinct_cpc_sim","cpc_subsection_id")
# All colnames: c("similar_no","no_distinct_cpc_sim","cpc_group_title","cpc_group_id","cpc_subgroup_id")


# Date: List API
date_api <- as.list(rep(NA, length(sim_pats$similar_no)))
names(date_api) <- sim_pats$similar_no

# Date: Matrix dplr
date <- as.data.frame(matrix(ncol=2, nrow=length(sim_pats$similar_no)))
date[1] <- sim_pats$similar_no
colnames(date) <- c("similar_no","date")

# No. 450 problem
# grep("3137670",sim_pats$similar_no)

## Assignee Only: Request from API
for (i in 1:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = as.character(sim_pats$similar_no[i]))
  fields <- c("assignee_organization")
  asgn[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}

## Careful of double assignees
# asgn[[451]]$patents$assignees[[1]]$assignee_organization

# Loop over list of patents (runs fast)
for (i in 1:length(sim_pats$similar_no)){
  # Test if more than 1 assignee for given patent
  if(length(asgn[[i]]$patents$assignees[[1]]$assignee_organization) > 1){
    # Define function to combine double assignees: Paste both individuals, IF more than 1
    a <- asgn[[i]]$patents$assignees[[1]]$assignee_organization[1]
    b <- asgn[[i]]$patents$assignees[[1]]$assignee_organization[2]
    # Target matrix: [patent index, column index]
    asgn_res[[i,2]] <- paste(a,b, sep = ";;")
  } else 
    asgn_res[[i,2]] <- asgn[[i]]$patents$assignees[[1]]$assignee_organization
}


## CPC Only: Request from API // Sub-Group doesnt work any more
# TODO: Run for sub-section - went until 449; removed 450; run again from 450
# 1529sec until 449; 1400 sec until 
system.time({
  for (i in 450:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = as.character(sim_pats$similar_no[i]))
  fields <- c("cpc_subsection_id")
  cpc_api[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}
})


#### Call the APi on my own; not really successful ####
library(httr)
library(jsonlite)

GET("https://api.patentsview.org/patents/query?q={"_gte":{"patent_date":"2007-01-04"}}&f=["patent_number","patent_date"]")

link <- "https://api.patentsview.org/patents/query"
body <- "{"q":{"_gte":{"patent_number":p}},"f":["patent_number","patent_date"]}"
p <- 10315342
q <- q={"_gte":{"patent_number":"p"}}&f=["cpc_group_id"]

paste(link,)

github_api <- function(path) {
  url <- modify_url("https://api.patentsview.org/patents/query?", path = path)
  GET()
}

resp <- github_api("/repos/hadley/httr")
resp

call <- paste(link,q)

test_cpc <- GET()

with_qfuns(query, envir = parent.frame())
#####

## Explore: How many CPCs are there?
cpc_len <- as.data.frame(matrix(ncol=1, nrow=length(sim_pats$similar_no)))
for (i in 1:length(sim_pats$similar_no)){
  cpc_len[i,1] <- length(cpc_api[[i]]$patents$cpcs[[1]]$cpc_subsection_id)
}
max(cpc_len)  # V2: up to 75 sub-group (8-digit) classifications
              # V3: up to 8 subsections (3-digit)
mean(cpc_len$V1) # 1.92
median(cpc_len$V1) # 2


# TODO: rewrite for only "subsection" input
# v2: Loop over list of patents (somewhat fast)
# for (i in 1:length(sim_pats$similar_no)){
#   cpc_api[[i]]$patents %>% 
#     # turn into DF, each field is a column
#     unnest(cols = cpcs) %>% 
#     #  # add distinct classes indicated // only later at one row=
#     #  mutate(no_distinct_cpc_sim = length(unique(cpc_group_id)),.after = cpc_group_id) %>%   
#     # group by CPC group 
#     group_by(cpc_group_id) %>% 
#     summarize(cpc_group_title = first(cpc_group_title),
#               cpc_subgroup_id = paste(cpc_subgroup_id, collapse = ";;")) %>% 
#     # add distinct classes indicatot
#     mutate(no_distinct_cpc_sim = length(cpc_group_id),.after = cpc_group_id) %>% 
#     # Concenate all CPC classes into one row
#     group_by(no_distinct_cpc_sim) %>%
#     summarize(cpc_group_id    = paste(cpc_group_id, collapse = ";;"),
#               cpc_group_title = paste(cpc_group_title, collapse = ";;"),
#               cpc_subgroup_id = paste(cpc_subgroup_id, collapse = ";;")) -> cpc[i,2:5]
# }

# V3 Do the same, but for only 1 category
for (i in 1:length(sim_pats$similar_no)){
  cpc_api[[i]]$patents %>% 
    # turn into DF, each cpc becomes a row
    unnest(cols = cpcs) %>% 
    #  add distinct classes indicator = no. of rows
    mutate(no_distinct_cpc_sim = length(cpc_subsection_id)) %>% 
    # Concenate all CPC classes into one row
    group_by(no_distinct_cpc_sim) %>%
    summarize(cpc_subsection_id = paste(cpc_subsection_id, collapse = ";;")) -> cpc[i,2:3]
}



## Date
# Request from API
for (i in 1:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = as.character(sim_pats$similar_no[i]))
  fields <- c("patent_date")
  date_api[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}

# Date put into matrix runs fast)
for (i in 1:length(sim_pats$similar_no)){
   date[[i,2]] <- date_api[[i]]$patents$patent_date[1]
}

## check if it's really the same stuff // confirmed by internet check
#View(cpc_api$'10480924') # one of the few CPC NA's
#View(asgn$'10480924') # Assignee = CASE WESTERN RESERVE UNIVERSITY
#View(cpc_api$'10479031') # B29C, B29K
#View(asgn$'10479031') # Assignee = 2x TyssenKrupp Organizations

#### END META INFORMATION SEPERATELY ####
#### COMBINE WITH PORTFOLIO FILES & Generate Matrix // V1 with 400; V2 with 300/clean ####

# # Add to existing Similarity DF with patents // version 1
# patcols <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_v1.csv",
#                      col_names = T)
# comb1 <- merge(patcols, similar_meta, by = "similar_no", all.y = TRUE)

## Combine Assignee & Date / Version 2
# V2: since CPC didn't work -> use file for 4 port. patents where we have it already
meta_old <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_meta_v1.csv",
                     col_names = T)
# V2: Match patent with existing full file
meta_300 <- meta_old %>%
  # Inner join with big meta file; 
  merge(.,similar_meta, by="similar_no") %>% 
  # Only patents within <20 years = valid
  filter(date > "2000-01-01")

# V2: Combine Sim_Meta with Portfolio
meta_full_300 <- merge(meta_300,portfolio_cpc, by = "patent_no")
colnames(meta_full_300)



### V3: Merge SIM META DATA into 1
similar_meta <- merge(asgn_res, cpc, by = "similar_no")
similar_meta2 <- merge(similar_meta, date, by = "similar_no")

## SAVE-POINT ##
write_csv(similar_meta2, "~/Master Thesis/IP_Similarity_Thesis/data_company/sim_meta_v3.csv",
          col_names = T)

#### Analyse Target Portfolio ####

# V2: Add target portfolio 4-digit CPC split: UNIQUE CLASSES per portfolio patent
# V3: PORTFOLIO Add the 14 patents with 3-digit CPC
portfolio <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_cpc_split3.csv",
                          col_names = T)
portfolio_cpc <-  portfolio %>%
  # group into 1 row per port patent
  group_by(patent_no) %>% 
  # add distinct classes indicator
  mutate(port_no_distinct_cpc = length(unique(port_cpc_group_id))) %>% 
  summarize(port_cpc_group_id = paste(unique(port_cpc_group_id), collapse = ";;"),
            port_cpc_old = paste(port_cpc_old, collapse = ";;"),
            port_no_distinct_cpc = first(port_no_distinct_cpc)) %>% 
  # remove 1st patent that shouldn't be in there
  slice(-c(1))

# Make it pretty / anonymized for paper
portfolio_anon <- portfolio %>% 
  # group into 1 row per port patent
  group_by(patent_no) %>% 
  # add distinct classes indicators
  #mutate(CPC3_Class_No = length(unique(port_cpc_group_id))) %>% 
  #mutate(CPC4_Subclass_No = length(unique(port_cpc_old))) %>%
  summarize(CPC3_Class = paste(unique(port_cpc_group_id), collapse = "; "),
            CPC4_Subclass = paste(port_cpc_old, collapse = "; "),
            #CPC3_Class_No = first(CPC3_Class_No),
            #CPC4_Subclass_No = first(CPC4_Subclass_No)
            ) %>% 
  # remove 1st patent that shouldn't be in there & Columns
  slice(-c(1)) %>% 
  # Add index instead of pat_no
  mutate(Portfolio_Index = seq.int(nrow(.))) %>% 
  select(-c(patent_no)) %>% 
  #relocate(CPC3_Class_No, .after = CPC3_Class) %>% 
  relocate(Portfolio_Index)



#### V3: MATCH PORT and SIM patents ####
pat_match <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_full.csv",
                    col_names = T)

port_detail <- merge(pat_match, portfolio_cpc, by = "patent_no")
meta_full_1400 <- merge(port_detail, similar_meta2, by = "similar_no")
colnames(meta_full_1400)

similar_meta %>% 
  filter(!is.na(assignee)) %>%
  filter(!is.na(no_distinct_cpc_sim)) -> similiar_filtered
  

# Clean, Re-name & Re-order
meta_full <- meta_full_1400 %>% 
  # Clean the target patent portfolio order
  relocate(portfolio_id) %>%
  relocate(port_cpc_group_id, .after = patent_no) %>%
  relocate(port_no_distinct_cpc, .after = port_cpc_group_id) %>%
  rename(port_id = portfolio_id,
         port_no = patent_no,
         port_cpc3 = port_cpc_group_id,
         port_disc_cpc = port_no_distinct_cpc,
         port_cpc4 = port_cpc_old) %>% 
  # Clean the Similarity patents
  # TODO: adjust for CPC8 / Title in there or not
  rename(sim_no = similar_no,
         sim_asgn = assignee,
         sim_cpc3 = cpc_subsection_id,
         sim_disc_cpc = no_distinct_cpc_sim) %>%
  relocate(sim_asgn, .after = sim_no) %>% 
  relocate(sim_cpc3, .after = sim_asgn) %>%
  relocate(sim_disc_cpc, .after = sim_cpc3) %>%
  relocate(cosine, .after = date) %>%
  # Sort by Portfolio ID
  arrange(port_id)

rm(meta_full_1400)

## SAVE-POINT ## --- ## V3:EX/ V2:IMPORT ##
write_csv(meta_full, "~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_meta_v3.csv",
          col_names = T)
meta_full_300 <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_meta_v2.csv",
                          col_names = T)
## --- ## --- ##

#### Final Steps // V2 with 300 and CPC4, V3 with all 1400 patents, but CPC3 classification #### 
# (i) filter NAs & only "unique" asgn, (ii) combine doubled, (iii) compare to Target firm
## (i) 
# V1: Von insgesamt 400 Patenten sind ... //V2: 300 -> 142 st. 2000 ... // 866
meta_full %>% 
  
  # ... 369 "assigned" und bestehen aus ... // V2: 134
  filter(!is.na(sim_asgn)) %>% 
  # ... 287 "unique" Assignee (0.71) // V2: 108 (0.76) // V3: 450
  distinct(sim_asgn) %>% 
  # .... // V3: 340 nach 2000
  dplyr::filter(date > "2000-01-01") %>% 
  arrange(sim_asgn) -> asgn_unique

## (ii)
# Clean my names (problems in csv); pot. to lowercase
stringi::stri_replace_all_fixed(
  asgn_unique$sim_asgn, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
  vectorize_all = FALSE
) -> asgn_unique$sim_asgn

write_csv(asgn_unique, "~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_unique_assignees_v3.csv",
          col_names = T)



# # TODO: consolidate duplicated assignee names above
# library("devtools", character.only = TRUE)
# # Dependencies
# install.packages("fansi")
# install.packages("stringi")
# install.packages("tibble")
# install_github("stasvlasov/harmonizer")
# library(harmonizer)
# ?harmonizer
# asgn_harm <- data.frame(harmonized = harmonize(asgn_unique$sim_asgn), original = asgn_unique$sim_asgn)
# 
# org.names.test <- c("žŸong-ÃÇÈÏ\n\u00b5&oacute;\u00b5<p>, LTD Co;  "
#                     , "<br> the $(Ldt &AMP; C&oacute;MP) Ïotta INt"
#                     , "Masha  &AMP;Lena Ltd. (Spb)"
#                     , "bla-bla-bla Ltd.")
# str(org.names.test)
# str(asgn_unique$sim_asgn)
# harmonize(org.names.test, procedures = list(
#   list("harmonize.toascii", detect.encoding = FALSE)
#   , "harmonize.remove.brackets"
#   , "harmonize.toupper"
#   , list("harmonize.squish.spaces", wrap.in.spaces = TRUE)
#   , "cockburn.replace.punctuation"
#   , "harmonize.squish.spaces"))

## (iii)
# Compare CPC % match for each: take only relevant
library(tidyr)
## Make it long first; then combine again
meta_full %>% 
  select("port_id","port_no","port_cpc3","port_disc_cpc",
         "sim_no","sim_cpc3","sim_disc_cpc", "date") %>% 
  # Filter old ones out
  filter(date > "2000-01-01") %>%
  # each PORT cpc class to own row: 142 -> 562 (~ x4)
  separate_rows(port_cpc3, sep = ";;") %>% 
  # each SIM cpc class to own row: 562 -> 2048 (~ x4)
  separate_rows(sim_cpc3, sep = ";;") %>% 
  # TODO: Finda way to cluster/count by patent no / for now manual
  # group_by(port_cpc4,sim_cpc4,sim_cpc4_title) %>% 
  mutate(joint_cpc = 0) -> meta_long

# Iterate over new column, counting matches
for(i in 1: nrow(meta_long)){
  if(meta_long$sim_cpc3[i] == meta_long$port_cpc3[i]){
    meta_long$joint_cpc[i] <- 1
    }
}
# Calculate matching % betwenn CPCs for SIMILAR Patents
meta_long %>% 
  group_by(port_id,  port_no, port_disc_cpc,
           sim_no, sim_disc_cpc) %>%
  summarise(port_cpc3 = paste(unique(port_cpc3), collapse = "; "),
            sim_cpc3  = paste(unique(sim_cpc3), collapse = "; "),
            joint_cpc = sum(joint_cpc)) %>% 
  transform(cpc_match_sim = joint_cpc / sim_disc_cpc) -> meta_fit

# Descrip. Analysis
summary(meta_fit)



# Make file tidy before merge
meta_full <- meta_full %>% 
  select(-c(sim_cpc3, port_cpc3)) %>% 
  mutate(port_cpc4 = str_replace_all(port_cpc4,";;","; "))

# V3: Merge back
meta_fit_final <- merge(meta_full,meta_fit, by = c("port_id","port_no","port_disc_cpc","sim_no","sim_disc_cpc"))

colnames(meta_fit_final)
# Calculate matching % betwenn CPCs for PORTFOLIO Patents & Re-Arrange
meta_fit_final <- meta_fit_final %>% 
  transform(cpc_match_port = joint_cpc / port_disc_cpc) %>% 
  relocate(c(sim_no,sim_disc_cpc), .after = port_disc_cpc) %>%
  relocate(cpc_match_sim, .after = sim_disc_cpc) %>% 
  relocate(cpc_match_port, .after = port_disc_cpc) %>% 
  relocate(sim_cpc3, .after = sim_no) %>% 
  relocate(c(port_cpc3,port_disc_cpc,port_cpc4), .after = port_no) 


## Final cleaning of string before save: remove all special characters
meta_fit_final <- meta_fit_final %>% 
  mutate(sim_asgn = str_replace_all(sim_asgn,";;","; ")) %>% 
  mutate(sim_asgn = stringi::stri_replace_all_fixed(sim_asgn, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
  vectorize_all = FALSE)) %>% 
  mutate(sim_asgn = iconv(sim_asgn, from = 'UTF-8', to = 'ASCII//TRANSLIT'))


# Save 
write_csv(meta_fit_final,"~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_sim_final.csv",
          col_names = T)
