## Goal: Add meta data (CPC, Assingee) for "most similar" patents of portfolio
## Approach: Access USPTO's patentview API via R package
#install.packages("patentsview")
library(patentsview)
library(readr)
library(dplyr)
library(tidyr)
library(unheadr) # un-wrapping columns; opposite of seperate_rows() to change 
                 # between condence and long format of CPC classes

# Unique patent no. of semantically similar patents
sim_pats <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_unique_v1.csv")

# Define our previous target
# TODO: add to the final file & also get meta-information for these
target     <- c("X.10000000.", # Test I
                "X.10215337.",
                "X.10300636.", # added
                "X.10315342.", 
                "X.10392507.", # added
                "X.10414259.",                
                "X.6769700.",
                "X.8444205.",                 
                "X.8628137.",
                "X.8768153.",
                "X.8991893.",  # added 
                "X.9073421.",
                "X.9186984.",
                "X.9399393.",  # added
                "X.9994094."
)


# a) Fields == What data is possible to get
get_endpoints() # [1] "assignees"  [2] "cpc_subsections"
View(patentsview::fieldsdf)
# Detailed information: define as "fields" for search_pv retrival function
get_fields(endpoint = "inventors") # 16 fields
get_fields(endpoint = "cpc_subsections", groups = "cpc_subsections") # 9 fields

# Information needed:
# CPC Details
#c("cpc_group_title","cpc_group_id","cpc_subgroup_id")
# Assignee Details
#c("assignee_first_name","assignee_last_name","assignee_organization")

# b) The query == How to filter the patent data
query <- qry_funs$eq(patent_number = "10300636")
fields <- c("cpc_group_title","cpc_group_id","cpc_subgroup_id",
            "assignee_first_name","assignee_last_name","assignee_organization")

system.time({
  full$"10300636" <- search_pv(
  query = query,
  fields = fields,
  all_pages = TRUE)
})

# as Function
get_meta <- function(i){
  query <- qry_funs$eq(patent_number = "i")
  fields <- c("cpc_group_title","cpc_group_id","cpc_subgroup_id",
              "assignee_first_name","assignee_last_name","assignee_organization")
  full$"i" <- search_pv(
      query = query,
      fields = fields,
      all_pages = TRUE)
}
lapply(sim_pats$similar_no, function(i) get_meta)


## Pre-define for results
# Empty list for API results for ALL
small2 <- as.list(rep(NA, length(sim_pats$similar_no)))
names(small2) <- sim_pats$similar_no

# Emptry matrix for ALL final results; first column is patent no.
small3 <- as.data.frame(matrix(ncol=6, nrow=length(sim_pats$similar_no[110:114])))
small3[1] <- sim_pats$similar_no[110:114]
colnames(small3) <- c("similar_no","assignee","assignee_key_id",
                      "cpc_group_title","cpc_group_id","cpc_subgroup_id")

## Testing Loop
#for (i in 1:5){
#  small2[i] <- print(i)
#}

# Loop (SLOW) for extracting all meta dat
for (i in 1:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = sim_pats$similar_no[i])
  fields <- c("cpc_group_title","cpc_group_id","cpc_subgroup_id",
              "assignee_first_name","assignee_last_name","assignee_organization")
  small2[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}

# Write List / Save
saveRDS(small2, 'uspto_meta_data_393_examples.Rds')
#Open
your_list <- readRDS('uspto_meta_data_393_examples.Rds')

# SINGLE ONE: 1 line per patent - FINALLY WORKS!
small2$`10470655`$patents %>% 
  # turn into DF, each field is a column
  unnest(cols = c(assignees, cpcs)) %>% 
  # Take only 1 assignee/inventor - IF/ELSE DON'T WORK FOR LAST CASE
  mutate(assignee = 
           if(!is.na(assignee_organization[1]) == TRUE | !is.null(assignee_organization[1]) == TRUE){
             assignee_organization[1]
           } 
         else if(!is.na(assignee_first_name[1]) == TRUE | !is.null(assignee_first_name[1]) == TRUE){
           str_c(c(assignee_first_name[1], assignee_last_name[1]),sep = " ")
         },
         .before = cpc_group_title) %>% 
  mutate(assignee = case_when(
    is.na(assignee_first_name[1]) == TRUE ~ "Unknown_Assignee"
  )) %>% 
  select(-c(assignee_first_name,assignee_last_name,assignee_organization)) %>% 
  unwrap_cols(groupingVar = assignee, separator = ";;") -> small3[1,1:5]

rm(small25)

# Big Loop - Clusterfuck
for (i in 110:length(sim_pats$similar_no[110:114])){
  if(length(small2[[i]]$patents$assignees[[1]]$assignee_first_name) > 1){
    # Fix problem that > assignees breaks the function
    small2[[i]]$patents$assignees[[1]] %>% 
      group_by(assignee_first_name) %>%
      summarise(assignee_organization = paste(assignee_organization, collapse ="& "))
  } 
  small2[[i]]$patents %>% 
    # turn into DF, each field is a column
    unnest(cols = c(assignees, cpcs))} -> small2[[i]]$patents

# Split the function, only run first part
%>% 
  # Take only 1 assignee/inventor - IF/ELSE DON'T WORK FOR LAST CASE
  # mutate(assignee = 
  #           if 
  #         (is.na(assignee_organization) == FALSE | is.null(assignee_organization) == FALSE){
  #           assignee_organization
  #         } else if
  #         (is.na(assignee_first_name) == FALSE | is.null(assignee_first_name) == FALSE){
  #         str_c(c(assignee_first_name, assignee_last_name),sep = " ")
  #       },
  #       .before = cpc_group_title) %>% 
  # mutate(assignee = case_when(
#  is.na(assignee_first_name) == TRUE ~ "Unknown_Assignee"
# )) %>% 
# select(-c(assignee_first_name,assignee_last_name,assignee_organization)) %>% 
if (is.na(assignee_organization) == FALSE | is.null(assignee_organization) == FALSE){
  unwrap_cols(groupingVar = assignee_organization, separator = ";;") -> small3[1:5,2:6]
} else
  unwrap_cols(groupingVar = assignee_first_name, separator = ";;") -> small3[1:5,2:6]
}




#### Get META information seperately ####

## Define empty objects for results
# Empty list for API results, only Assignee
asgn <- as.list(rep(NA, length(sim_pats$similar_no)))
names(asgn) <- sim_pats$similar_no

# Emptry matrix for ONLY ASSIGNEE results
asgn_res <- as.data.frame(matrix(ncol=2, nrow=length(sim_pats$similar_no)))
asgn_res[1] <- sim_pats$similar_no
colnames(asgn_res) <- c("similar_no","assignee")

## Assignee Only:
for (i in 1:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = sim_pats$similar_no[i])
  fields <- c("assignee_organization")
  asgn[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}

# Define function to combine double assignees
double_asgn <- function(i){
  # Paste both individuals, IF more than 1
  a <- asgn[[i]]$patents$assignees[[1]]$assignee_organization[1]
  b <- asgn[[i]]$patents$assignees[[1]]$assignee_organization[2]
  # Target matrix: [patent index, column index]
  asgn_res[[i,2]] <- paste(a,b, sep = ";;")
}

# Loop over list of patents - works fast
for (i in 1:length(sim_pats$similar_no)){
  # Test if more than 1 assignee for given patent
  if(length(asgn[[i]]$patents$assignees[[1]]$assignee_organization) > 1){
    double_asgn
  } else 
    asgn_res[[i,2]] <- asgn[[i]]$patents$assignees[[1]]$assignee_organization
}

## CPC Only
for (i in 1:length(sim_pats$similar_no)){
  query <- qry_funs$eq(patent_number = sim_pats$similar_no[i])
  fields <- c("cpc_group_title","cpc_group_id","cpc_subgroup_id")
  asgn[i] <- search_pv(
    query = query,
    fields = fields,
    all_pages = TRUE)
}

## Combine Assignee & CPC


#### END META INFORMATION SEPERATELY ####



# Add to existing Similarity DF with patents
library(plyr)
PatCols <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_v1.csv",
                     col_names = T)

comb <- merge(x = PatCols, y = small3, by = "similar_no", all.y = TRUE)

comb %>% 
  toString(.$patent_no) %>% 
  mutate(patent_no = str_replace(patent_no,".*","10000000"))
  select

comb["cpc_patent"]="G01S"
# Add with our CPC
PatCols <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_clean_cpc.csv",
                      col_names = T)
 
comb2 <- comb %>% 
  separate_rows(cpc_group_id, sep = ";;") 

comb2 %>% 
  unique(cpc_group_id)
  
  
# Old - save in DF doesn't work
pv_res$data$patents %>% 
  # turn into DF, each field is a column
  unnest(cols = c(assignees, cpcs)) %>% 
  # Take only 1 assignee
  mutate(assignee = 
           if(is.na(assignee_organization[1]) == TRUE){
             str_c(c(assignee_first_name[1], assignee_last_name[1]),sep = " ")} 
             else assignee_organization[1]) %>% 
  select(-c(assignee_first_name,assignee_last_name,assignee_organization)) %>% 
  unwrap_cols(groupingVar = assignee, separator = ";;") -> sim_pats[,2:6] 




rbind
x <- character(length(sim_pats$similar_no))
colclasses <- "character"
l <- lapply(, )


?unnest
## Put everything into parallized loop / apply
for (i in 1:length(sim_pats$similar_no[1:5])){
  "do something"
}