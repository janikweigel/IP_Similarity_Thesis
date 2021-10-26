## Goal: Validate the method 
## Approach: Expert interview, assessing a (random) sample of the data
## -> Evaluate with Company which patent assingees are the most interesting!

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Load from script C 
sample_asgn <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_sim_final.csv",
                            col_names = T)


## 1:  Clean ASSIGNEES
sample_asgn_clean <- sample_asgn %>% 
  mutate(asgn = str_replace_all(asgn,";;","; ")) %>% 
  mutate(asgn = stringi::stri_replace_all_fixed(asgn, 
                                                    c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
                                                    c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
                                                    vectorize_all = FALSE)) %>% 
  mutate(asgn = iconv(asgn, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  rename(sim_asgn = asgn) %>% 
  as.data.frame()



## 2: Create Assessment score from manually filled in Excel/cSV
#val_score <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/firm_survey_res.csv")
val_score <- read.csv("~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/firm_survey_res.csv", colClasses=c("sim_asgn"="character"))

val_score <- val_score %>%
  # drop 1 column / 1 empty row
  select(-sim_cpc4_title) %>% 
  filter(!sim_asgn == "") %>% 
  mutate(sim_asgn = str_replace_all(sim_asgn,";;","; ")) %>% 
  mutate(sim_asgn = stringi::stri_replace_all_fixed(sim_asgn, 
                                                c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
                                                c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
                                                vectorize_all = FALSE)) %>% 
  mutate(sim_asgn = iconv(sim_asgn, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  # Problem with long name
  mutate(sim_asgn = str_replace(sim_asgn,"COMMISSARIAT A L'ENERGIE ATOMIQUE ET AUX ENERGIES",
                                "COMMISSARIAT A L'ENERGIE ATOMIQUE ET AUX ENERGIES ALTERNATIVES")) %>% 
  rename(group = Category_A_direkte_Konkurrenz_.bestehend._B_potentiel.Konkurent.Partner_C_patent.IP_technisch_relevantX_unrelevant)

# Data Check
# TODO: KEEP EVERYTHING LOWERCASE FROM NOW ON FOR CHECKS!
str(val_score)
tolower(sample_asgn$sim_asgn) %in% tolower(val_score$sim_asgn)

# Only score
score <- val_score %>% 
  select(-c(sim_no, year, sim_cpc4)) %>% 
  mutate(sim_asgn = tolower(sim_asgn))

## 3a: Prepare main file for merge: DEFINE THE DEPENDENT METRICS
# meta_fit_lower - "Version 181": per patent match
# meta_fit_avg -  "Version 36": per assingee
meta_fit_final <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_sim_final.csv", col_names = T)

meta_fit_avg <- meta_fit_final %>% 
  # only relevant patent matches
  filter(date > "2000-01-01") %>% 
  # for correct assingee matching
  #mutate(sim_asgn = tolower(sim_asgn)) %>% 
  # New avg. CPC matching score for each patent
  transform(cpc_match_avg = ((cpc_match_sim + cpc_match_port) / 2 )) %>% 
  # Take the average of the relevant metrics per assingee
  na.omit(sim_asgn) %>% 
  group_by(sim_asgn) %>% 
  summarise(cpc_match_sim = mean(cpc_match_sim),
            cpc_match_port = mean(cpc_match_port),
            cpc_match_avg = mean(cpc_match_avg),
            joint_cpc = mean(joint_cpc), 
            cosine = mean(cosine),
            # TODO: Calculate for full basis!
            # Create new metrics: # of portfolio patents matched / # of patents per assingee
            port_no_matched = length(unique(port_no)),
            sim_no_matched = length(unique(sim_no)),
            port_no = paste(unique(port_no), collapse = "; "),
            sim_no  = paste(unique(sim_no), collapse = "; ")) 

write_csv(meta_fit_avg, "~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_sim_asgn_final.csv", col_names = T)

dependet_var_ful <- skimr::skim(meta_fit_avg)
write.table(dependet_var_ful,
            file = "dependet_var_ful.txt", sep = ",", quote = FALSE, row.names = F)

## 3b: Evaluate 
# [V_181] High CPC match scores overall
# Ranking: sim < avg < port 
# TODO: compare performance, go with val_meta$cpc_match_sim in question
summary(val_meta$cpc_match_avg)
summary(val_meta$cpc_match_sim)
summary(val_meta$cpc_match_port)
# <> compare to normal: for (>0.5) its slightly higher, for (<0.5) its signif. higher!
# <> cpc_match_port is general very high: median of 1
summary(meta_fit_final$cpc_match_sim)
summary(meta_fit_final$cpc_match_port)


## 4: EMERGE with former master sheet based on sim_asgn (xtract all patent/meta data)
# Load
val <- merge(meta_fit_avg,score,by="sim_asgn",all=F)
# Findings OLD: 36 assingees halten insg. 184 = 13.5% der Gesamtmatches // 98 patente
skimr::skim(val$Q_relevant)

# Add categorial possibility: Relevance Y/N = 1/0
val <- val  %>% 
  #rename(group = Category_A_direkte_Konkurrenz_.bestehend._B_potentiel.Konkurent.Partner_C_patent.IP_technisch_relevantX_unrelevant) %>% 
  mutate(rel = if_else(
    Q_relevant > 1,1,0))

summary(val$rel) # Y: 14 / N: 22

## 5: Outlier detection - No. of patents in each group [version_181]

# Outlier detection FULL
meta_fit_final %>% 
  # filter(Q_relevant == 1) %>% 
  # filter(date > "2000-01-01") %>% 
  select(sim_asgn, sim_no) %>% 
  group_by(sim_asgn) %>%
  summarise(sim_no = length(sim_no)) -> pats_per_asgn

# Outlier detection SAMPLE 181
val_meta %>% 
  # filter(Q_relevant == 1) %>% 
  select(sim_asgn, sim_no) %>% 
  group_by(sim_asgn) %>%
  summarise(sim_no = length(sim_no)) %>% 
  arrange(desc(sim_no)) -> pats_per_asgn_smpl

hist(pats_per_asgn_smpl$sim_no)
skimr::skim(pats_per_asgn)

# 181 patents in sample
sum(pats_per_asgn_smpl[1:3,2]) # 88 belong to the largest 3
sum(pats_per_asgn_smpl[1:4,2]) # 103 belong to the largest 4

sum_smpl <- summary(pats_per_asgn_smpl)
# Outlier defintion by Wolfram Alpha: https://mathworld.wolfram.com/Outlier.html
# "more than 1.5 times the interquartile range above the third quartile or below the first quartile"
# Interquartile Range = Q3 - Q1
IQR = 4 - 1
TH = IQR * 1.5
4+TH # Max. 8.5 -> Yeah, most of the upper ones are out

# Outlier SAMPLE 36 - not needed /7 remove yotoday
val <- val %>% 
  arrange(sim_asgn) %>% 
  .[-c(36),]

smoothScatter(val$cpc_match_avg, val$Q_relevant, pch = 16, 
              transformation = function(x) x ^ 0.4, # Scale
              colramp = colorRampPalette(c("#f7f7f7", "lightblue")),
              xlab = 'CPC Matching Metric ("Average")',
              ylab = 'Rated Relevance to Target Company')



## 6: Run regressions
# A. Linear per patent
plot(val$cpc_match_avg, val$Q_relevant, type = 'p', las = 1,
     xlab = 'Semantic Matching Rate (Portfolio Patents)',
     ylab = 'Rated Relevance to Target Company')
lm <- lm(Q_relevant ~ cpc_match_avg, data=val) #fit linear model
modsum <- summary(lm3) #view model summary
abline(reg=lm3,col="darkgreen")

# R^2
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

# p-Value
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
# Plot both
legend('bottomright', legend = rp, bty = 'n')


plot(val$cpc_match_avg, val$Q_relevant)
cor(val$cpc_match_avg, val$Q_relevant) # 0.37 port / 0.401 sim / 0.435 avg

plot(val$port_no_matched,val$Q_relevant)
cor(val$port_no_matched,val$Q_relevant) #0.609 for port / 0.562 for sim




## Structured LinApproach
lm1 = lm(Q_relevant ~ cpc_match_avg, data=val) # REL r^2 = 0.15
lm2 = lm(Q_relevant ~ port_no_matched, data=val) # REL r^2 = 0.294
lm3 = lm(Q_relevant ~ cpc_match_avg + port_no_matched, data=val) # REL only slightly higher(0.3281); cpc not signif. 
lm4 = lm(Q_relevant ~ cpc_match_sim + cpc_match_port + port_no_matched + sim_no_matched, data=val)
summary(lm2)
plot()
abline(reg=lm2,col="blue")


# B. Quad. Regression
#create a new variable for hours2
val$cpc_match_avg2 <- val$cpc_match_avg^2

#fit quadratic regression model
quadraticModel <- lm(Q_relevant ~ cpc_match_avg + cpc_match_avg2, data=val)

#view model summary
summary(quadraticModel)
plot(quadraticModel)

y <- val$Q_relevant
x <- val$cpc_match_avg
plot(x,y)

# Logistic?
logreg1 = glm(rel ~ cpc_match_avg, data=val, family="binomial")
logreg2 = glm(rel ~ port_no_matched, data=val, family="binomial")
logreg3 = glm(rel ~ cpc_match_avg + port_no_matched, data=val, family="binomial")
summary(logreg)
plot(logreg)




# C. Polynomial Reg.
lm.fit3 = lm(y ~ poly(x ,3))
lm.fit4 = lm(y ~ poly(x ,4)) # Still significant for sim
lm.fit5 = lm(y ~ poly(x ,5))
lm.fit6 = lm(y ~ poly(x ,6))
lm.fit7 = lm(y ~ poly(x ,7))


# compare them
anova()
summary(lm.fit4)



# D. As classfication (relevant/not)


# LDA
library(MASS)
lda <- lda(rel ~ cpc_match_avg, data= val) # special reg. version for LDA
lda

# KNN
knn=function(x0,x,y,K) # define a function 
{
  d=abs(x0-x) # "closeness": creates list of (non-negative) distances between 
  #  x0 (prediction point, text observation) and x (actual observation)
  o=order(d) # takes the index'es of d and sorts them: index of the smallest number, 2nd smallest...
  # The first element of o is the index corresponding to the smallest distance.
  prob=mean(y[o[1:K]]) # Find the K training-points that are closest to x0 
  # Take the average of the corresponding y-values 
  return(prob) # give out/return prob
}

y <- val$rel
x <- val$cpc_match_avg
# Y: 14 / N: 22
pred <- (14*1+22*0)/36

knn(0.42857,x,y,2) # Return: 1
knn(pred,x,y,3) # Return: 0.666
knn(pred,x,y,5) # Return: 0.8




## 7: Plot results
library(ggplot2)
fit <- lm.fit3 # LDA --> all over the place!!
prd <- data.frame(cpc_match_sim = seq(from = range(val$cpc_match_sim)[1], to = range(val$cpc_match_sim)[2], length.out = 36))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = cpc_match_sim, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = val, aes(x = cpc_match_sim, y = Q_relevant))

colnames(val)

meta_fit_final %>% 
  # filter(Q_relevant == 1) %>% 
  filter(date > "2000-01-01") %>% 
  select(sim_asgn, sim_no) %>% 
  group_by(sim_asgn) %>%
  summarise(sim_no = length(sim_no)) -> pats_per_asgn

#### Plotting away #####
library(ggplot2)
hist(pats_per_asgn$sim_no)

ggplot(pats_per_asgn_smpl, aes(x=sim_no)) + 
  geom_histogram(aes(), binwidth = 1, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(sim_no)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Patent matches per assingee",x="# of Patents", y = "# of assingees")



ggplot(pats_per_asgn, aes(x=sim_no)) + 
  geom_histogram(aes(), binwidth = 3, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(sim_no)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Patent matches per assingee",x="# of patent matches with target portfolio", y = "# of unique assingees")



#### Correlation ####
val_plot <- val %>% 
  dplyr::select(-c(sim_asgn, if_Y_which_relation, group, rel_num, cpc_match_avg2))

# Save some results for once 
write_csv(val,
          "~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/validation_asgn_final.csv",
          col_names = T)
write_csv(val_plot, # added sim_asgn for mapping
          "~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/validation_plot_final.csv",
          col_names = T)


## Linear
library(corrplot)
library(RColorBrewer)
val_cor <- cor(val_plot)

corrplot(val_cor, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

## Non-Linear
library(nlcor)

plot(val$cpc_match_avg,val$Q_relevant)

c <- nlcor(val$port_no_matched,val$Q_relevant, plt = T)
c$cor.estimate
c$adjusted.p.value
print(c$cor.plot)




#### Procedure to generate samples ####
meta_fit <- read_csv("~/Master Thesis/IP_Similarity_Thesis/data_company/portfolio_sim_final.csv",
                           col_names = T)

# Calc. Meta Avg. 
meta_final <- meta_fit %>% 
  # Filter out target company
  filter(sim_asgn != "[TARGET_COMP_NAME]") %>% 
  filter(date > "2000-01-01") %>% 
  transform(cpc_match_avg = ((cpc_match_sim + cpc_match_port) / 2 )) %>%
  mutate(sim_asgn = tolower(sim_asgn)) %>%  
  group_by(sim_asgn) %>% 
  summarize(port_no = paste(port_no, collapse = "; "),
            port_cpc4 = paste(port_cpc4, collapse = ";;"),
            sim_no = paste(sim_no, collapse = "; "),
            cpc_match_sim = mean(cpc_match_sim),
            cpc_match_port = mean(cpc_match_port),
            cpc_match_avg = mean(cpc_match_avg)) %>% 
  na.omit(sim_asgn)




## Idea: Cluster Samling into 3 categories: 
# (i) no/low CPC match; (ii) medium; (iii) - high/complete
cpc_high <- meta_final %>% # 25 pats -> 21 assignees / V4 - Sim: 594 pats -> 139 assignees
  filter(cpc_match_avg >= 0.76) # V5 (first group, then filter; AVG - > 0.66): 153 asgn
                                # V6 (> 0.75): 78 asgn

# Saved the original one
write_csv(cpc_high,"~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/cpc_group_high.csv")


cpc_mid <- meta_final %>% # 34 pats -> 27 asgn // V4 - Sim: 468 -> 201
  filter(cpc_match_avg < 0.76 &
         cpc_match_avg >= 0.25) # V5 (first group, then filter, AVG): 92 asgn
                                # V6 (0.76 > x > 0.25): 177 asgn  

write_csv(cpc_mid,"~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/cpc_group_mid.csv")



cpc_low <- meta_final %>% # 84 pats -> 66 asgn // V4 - Sim: 244 -> 196
  filter(cpc_match_avg < 0.25) # V5 (first group, then filter; AVG - < 0.34): 92 asgn 
                              # V6 (< 0.25): 82 asgn

write_csv(cpc_low,"~/Master Thesis/IP_Similarity_Thesis/data_company/Validation/cpc_group_low.csv")



#### SAMPLE GENERATION ####
## Experiment: Filter
set.seed(123)
s1 <- sample(cpc_high$sim_asgn,10)
s2 <- sample(cpc_mid$sim_asgn,10)
s3 <- sample(cpc_low$sim_asgn,10)


# Combine assignees
sample_asgn <- data.frame(asgn = c(s1,s2,s3))

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


write_csv(sample_full,"~/Master Thesis/IP_Similarity_Thesis/data_company/target_firm_patent_sample.csv",
          col_names = T)

meta_full_300 %>% 
  # ...Von X gesamt SIM patents ... // V2 ("meta_300" ?): 142
  filter(!is.na(sim_asgn)) %>% 
  # ...  sind 369 "assigned"  ... // V2: 134
  distinct(sim_asgn) %>% 
  # ... und davon 287 "unique" Assignees (strings) // V2: 108
  arrange(sim_asgn) -> asgn_unique


#### End of sample generation ####