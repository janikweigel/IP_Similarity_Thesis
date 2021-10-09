## Goal: Validate the method 
## Approach: Expert interview, assessing a (random) sample of the data


#### Cluster into 3 categories: (i) no/low CPC match; (ii) medium; (high)
# TODO: mix between PORT/SIM matching score?
## Evaluate with Company which ones most interesting!
cpc_new <- meta_fit_300 %>% # 25 pats -> 21 assigneed
  filter(cpc_match_sim >= 0.75)


cpc_high <- meta_fit_300 %>% # 25 pats -> 21 assigneed
  filter(cpc_match_sim >= 0.75) %>% 
  # Filter out Elkamet
  filter(sim_asgn != "ELKAMET KUNSTSTOFFTECHNIK GMBH") %>% 
  group_by(sim_asgn) %>% 
  summarize(port_no = paste(port_no, collapse = "; "),
            port_cpc4 = paste(port_cpc4, collapse = ";;"),
            sim_no = paste(sim_no, collapse = "; "),
            cpc_match_sim = mean(cpc_match_sim))

cpc_mid <- meta_fit_300 %>% # 34 pats -> 27 asgn
  filter(cpc_match_sim < 0.75 & cpc_match_sim >= 0.32) %>% 
  # Filter out Elkamet
  filter(sim_asgn != "ELKAMET KUNSTSTOFFTECHNIK GMBH") %>% 
  group_by(sim_asgn) %>% 
  summarize(port_no = paste(port_no, collapse = "; "),
            port_cpc4 = paste(port_cpc4, collapse = ";;"),
            sim_no = paste(sim_no, collapse = "; "),
            cpc_match_sim = mean(cpc_match_sim))

cpc_low <- meta_fit_300 %>% # 84 pats
  filter(cpc_match_sim < 0.32) %>% 
  # Filter out Elkamet
  filter(sim_asgn != "ELKAMET KUNSTSTOFFTECHNIK GMBH")


#### SAMPLE GENERATION ####
## Experiment: Filter
set.seed(123)
s1 <- sample(cpc_high$sim_asgn,10)
s2 <- sample(cpc_mid$sim_asgn,10)
s3 <- sample(cpc_low$sim_asgn,10)

library(stringr)
sample_full <- meta_fit_300 %>%
  filter(sim_asgn %in% c(s1,s2,s3)) %>% 
  mutate(year = substr(.$date,1,4)) %>%
  select(sim_asgn, sim_no, year, sim_cpc4, sim_cpc4_title) %>% 
  group_by(sim_asgn) %>% 
  summarize(sim_no   = paste(sim_no, collapse = "; "),
            year     = paste(year, collapse = "; "),
            sim_cpc4 = paste(sim_cpc4, collapse = ";;;;"),
            sim_cpc4_title = paste(sim_cpc4_title, collapse = ";;;;"))
# UMlaute
stringi::stri_replace_all_fixed(
  sample_full$sim_asgn, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
  vectorize_all = FALSE
) -> sample_full$sim_asgn

# save-to-send // CSV Probleme 
write_csv(sample_full,"~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_patent_sample.csv",
          col_names = T)

meta_full_300 %>% 
  # ... 369 "assigned" und bestehen aus ... // V2: 134
  filter(!is.na(sim_asgn)) %>% 
  # ... 287 "unique" Assignee (strings) // V2: 108
  distinct(sim_asgn) %>% 
  arrange(sim_asgn) -> asgn_unique
