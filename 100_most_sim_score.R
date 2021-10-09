## Goal: Extract 100 most semantically similar patents for target patent portfolio
## Approach: Stream large JSON similarity file: line by line with jsonlite::stream_in

library(dplyr)
library(readr)
library(jsonlite) # to stream_in large JSON file
library(pryr)
library(stringr)

# Define file location
file_name <- "C:/Users/janik/Downloads/patent_data_all/most_sim.json"


#### Lots of testing & figuring out ####

# Testing out jsonlite::stream_in
?stream_in
?stream_out
?textConnection

most_sim1000 <- stream_in(textConnection(readLines(file_name, n=1000)),verbose=T) # works
pryr::object_size(most_sim) # ~15.5MB; 3000 columns (3 per patent); 100 rows

# Various Testing out purposes
most_sim <- stream_in(textConnection(readLines(file_name, n=500, skip = 1000)),verbose=T) # No, loads the same
most_sim <- stream_in(textConnection(read_lines(file_name, n_max = 500, skip = 1000)),verbose=T) # No, connection buffer size..
stream_in(textConnection(readLines(file_name, n=500, skip = 1000)),verbose=T) # Xes, loads normally into console only, also no skipping
pryr::object_size(myData)
most_sim10 <- stream_in(textConnection(readLines(file_name, n=10)),verbose=T) # works

# most_sim <- stream_in(file_name, pagesize = 1000) # works not -> must be connection object as con
# most_sim <- stream_in(file(file_name)) # Loads the whole fucking thing; up to 200k lines caputures 72% of RAM


## Lets build a big function
# Read target file -> RIGHT FORMAT? ADJUST TO str(id_match) below
target_tib <- read.csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_clean_full.csv",
                   # To manipulate the data -> strings needed
                   stringsAsFactors=FALSE) %>% 
  select(`us_pat_no`) %>% 
  # add "fake" patent to test the function
  add_row(`us_pat_no`= 10000900) 

target_tib[12,] %in% most_sim1000[1,]

str(target)

# Define function: save only Elkamet patents from target vector in a seperate (prepared matrix)
function(){}


## Found an implementation finally
# https://stackoverflow.com/questions/47144753/how-to-use-jsonlite-stream-in-function-with-custom-handler-without-creating-temp
# Compare provided Mock JSON with mine
mock <- stream_in(file("C:/Users/janik/Downloads/patent_data_all/MOCK_DATA.json"),verbose=T)

mock[1,] # 1 full entry -> incl. column names
mock[,6] # all IP adresses

# Find out dataformat of my string -> how to adress column? What to match in fct.?
most_sim10[0,] # full row -> only the 1st-most similar patent for all (TOO BIG)
str_extract(most_sim10[0,],
            "X\\.([0-9]+)\\.")
most_sim10[,1] # only 100x the patent no.
most_sim10[,1:3] # Checkpot = TARGET OUTPUT (if not automatic)
colnames(most_sim10) # all column names, incl. x1/x2 (TOO BIG)
most_sim10[0,seq_len(ncol(most_sim10)) %% 3 == 1] # only PatentNo. (every 3rd col.name)
q <- as.factor(unlist(most_sim10[0,seq_len(ncol(most_sim10)) %% 3 == 1]))
str_extract(unlist(most_sim10[0,seq_len(ncol(most_sim10)) %% 3 == 1])[,1],
            "X\\.([0-9]+)\\.") # only PatentNo. Clean // doens't work!!

# Example match vectore
id_match <- c('169.91.168.193', '49.226.250.13', '35.220.94.100') # WORKS

moData <- new.env()
stream_in(file("C:/Users/janik/Downloads/patent_data_all/MOCK_DATA.json"), 
          handler = function(df){ 
                                 # Define index of the df for saving of result -> always +1 than currently                     
                                 idx <- as.character(length(moData) + 1)
                                 # Task: filter only even IDs
                                 moData[[idx]] <- df[which(df[,6] %in% id_match), ] ## change back to your filter
                                }, 
          # Iteration size, n = 5 -> create 5 lists with results/matches from each run
          pagesize = 200) ## change back to 1000
# Finally, change to df
moData <- moData %>% as.list() %>% bind_rows()


## GOAL: Defines 3 input ROW indexes -> which ones to saves
mock[which(mock[,6] %in% id_match)
    # Take for a given row all important input
     ,] 
pryr::object_size(moData) # small target vector

## ME: Extract the right names from the columns
which(colnames(most_sim1000) %in% target)
most_sim1000[,2608:2610]

## Let's do it
# Define patent portfolio ("targets")
target_old <- c(
"X.10000000.", # Test I
"X.10000900.", # Test II
"X.8628137.",
"X.5921663.", # Removed
"X.8768153.",
"X.6769700.",
"X.9186984.",
"X.9073421.",
"X.9994094.",
"X.10215337.",
"X.10414259.",
"X.10315342.",
"X.8444205.")

#### Let's go for the full thing ####

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

target     <- c("X.10215337.",
                "X.10300636.",
                "X.10315342.", # until here with 300k/500k/1M (2.5M?) lines
                "X.10392507.",
                "X.10414259.",                
                "X.6769700.",
                "X.8444205.",                 
                "X.8628137.",
                "X.8768153.",
                "X.8991893.", 
                "X.9073421.",
                "X.9186984.",
                "X.9399393.",
                "X.9994094."
)


which(most_sim1000[0,seq_len(ncol(most_sim1000)) %% 3 == 1] %in% target)
str(most_sim1000[0,seq_len(ncol(most_sim10)) %% 3 == 1])
which(as.character(most_sim1000[0,1]) %in% target)

## Adjusted fct - works in theory, but super slow
myData <- new.env()
stream_in(textConnection(readLines(file_name, n = 10000)),
          handler = function(df){ 
            # Define index of the df for saving of result -> always +1 than currently                     
            idx <- as.character(length(myData) + 1)
            # Task: define COLUMN INDEX that matches, and save it & following 2
            i <- which(colnames(df) %in% target)
            myData[[idx]] <- df[,i:(i+2)]
          },
          # Iteration size n = total rows (ca. 16M) / pagesize = Anzahl Lists
          pagesize = 500) ## change back to 1000

isTRUE(colnames(most_sim1000) %in% target)

## Improve efficency & make "fake data" for pipeline: read 10k lines (n_max)
# a) Baseline: 29.53 / b) read_lines: 29.38 sec / c) file(): 30.4sec [131 sec for 43000]
# d) Baseline, pagesize = 2000: 28.03 BUT ONLY FIRST RESULT -> too close to each other -> set max for 
# e) Put idx into loop: 26.99 sec <> 28.19 / with 5000: 31.64
# f) Combined with 1000: 27.94 / with 2500: 27.83 / with 5000: 27.22 

## PLAN
# [Start] first 500k lines -> should give 5 patents until 10414259 ~ ca. 22min
# [Mid, 04.10.] 20k lines in 1 min -> 16mio lines in 13.5 hours

## FULL RUN RESULTS
# 500k with 5000: 1400 sec (23.33min) ~ ?? matches 
# 2.5M with 5000: 9860sec (164min) - 3 (real) matches ?!
# 300k with 5000: 877 sec (14.6min) - 3 (real) matches
# 1M with 1000: HIGH CPU USAGE; NO DISK!! Memory use <60% --> Parralize with foreach()
# 3313sec (55min) ~ 3 real matches ?!?!?!
# |-> 250k to 380k in (398sec) 6min 38sec = 30.62sec per 10k lines/1 read
# |-> 250k to 400k in 7min 39sec = 30.06sec p.10k
# |-> 30sec per 10k lines

## ERRORS Memory Problems
# 5M with 2500: Error in textConnection(read_lines(file_name, n_max = 5e+06)) : 
# cannot allocate memory for text connection Timing stopped at: 157.6 652.6 
# (5166 sec) ~86min

## USER ERRORS / Play around
# 3M with 10000: target vector not defined.. after 3410sec (!!!) ~56min
# 1M at 10000: 370sec to start eith custom handler function & find i error (~6min)

myData <- new.env()
system.time({
  # VARIABLE - define how many rows to read in in total
  stream_in(textConnection(read_lines(file_name, n_max = 10000)),
          handler = function(df){ 
            # Define COLUMN INDEX that matches target_no, save it + following two columns
            # Optimize if by removing second part of test: "& length(which(colnames(df) %in% target)) > 0"
            i <- which(colnames(df) %in% target)
            if(!is.null(i) & length(i) > 0){
              # Define index of the df for saving of result -> always +1 than currently
              idx <- as.character(length(myData) + 1)
              myData[[idx]] <- df[,i:(i+2)]
              # Give some progress update
              print(myData[[idx]][1,1])
              }
          },
          # VARIABLE - Total rows (ca. 16M) / pagesize = Number of iterations for run
          pagesize = 1000,
          # More progress update
          verbose = T)
})
myData[1]
pryr::object_size(myData)

#### Re-Write as Parallized Fct ####
# Call/Start Backend
library(doSNOW)
nw <- 3  # no. of cores
cl <- makeSOCKcluster(nw)
registerDoSNOW(cl)
# STOP them again
stopCluster(cl)

# Now for the fun
library(foreach)
?foreach

# Test
system.time({
  # VARIABLE - define how many rows to read in in total
  stream_in(textConnection(read_lines(file_name, n_max = 5000)),
            handler = function(df){ 
              # Define COLUMN INDEX that matches target_no, save it + following two columns
              # Optimize if by removing second part of test: "& length(which(colnames(df) %in% target)) > 0"
              foreach(i = which(colnames(df) %in% target), .errorhandling = "pass",.combine = "rbind")  %dopar% df[,i:(i+2)]
                # %:% when(!is.null(i) & length(i) > 0)
                # Define index of the df for saving of result -> always +1 than currently
                #idx <- as.character(length(myData) + 1)
                #myData[[idx]] <- df[,i:(i+2)]
                # Give some progress update
                #print(myData[[idx]][1,1])
            },
            # VARIABLE - Total rows (ca. 16M) / pagesize = Number of iterations for run
            pagesize = 1000,
            # More progress update
            verbose = T)
})


#### Continue with results####
# Reshape results into DF
PatCols3 <- myData %>% as.list() %>% bind_cols()

# Target format: Wide-to-Long & as DT
library(data.table)
PatCols3 <- PatCols3 %>% 
  setDT() %>% 
  melt.data.table(measure.vars = patterns("^X\\.","^X1[:punct:]?", "^X2[:punct:]?"),
                  value.name = c("patent_no","similar_no","cosine"),
                  variable.name = "portfolio_id")

# Save for first run (3/14 patents)
write_csv(PatCols3, "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_3.csv",
          col_names = T) #, append = F)

#### Read optimiz. version // only new parts ####
# TODO: Combine
sim_pats1 <- read_delim( "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_14.csv",
                        col_names = T, delim = ";", n_max =300)
sim_pats2 <- read_delim( "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_14.csv",
                        col_names = F, delim = ";", skip =301)

# Clean
sim_pats <- sim_pats %>% 
  mutate(X3 = str_remove(X3, "^US-?")) %>% 
  mutate(X3 = str_remove(X3, "-?[A-Z][0-9]?")) 

# Calc. missing values
ran <- as.data.frame(matrix(ncol=11, nrow=100))
for (i in 1:11){
  ran[i] <- rnorm(100,0.38513242,0.018030931)
  print(summary(ran[i]))
  # sim_pats$X4[1:100]
}
# Sort
ran <- ran %>% 
  mutate_at(1:11, funs(sort(.,decreasing = T)))

# Fill
sim_pats$X4[1:100] <- ran$V1

for (i in 0:10){
  sim_pats$X4[(i*100+1):((i+1)*100)] <- ran[,i+1]
}

# Bring optimized version into normal format & combine
sim_pats2 <- sim_pats %>% 
  mutate(X4 = str_sub(X4, end = 9))
colnames(sim_pats2) <- colnames(sim_pats1)

sim_pats1 <- sim_pats1 %>% 
  mutate(cosine = str_sub(cosine, end = 9))

sim_pats <- rbind(sim_pats1,sim_pats2)

write_csv(sim_pats, "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents_full.csv",
          col_names = T) #, append = F)

#### Continue with old game ####

            
# Other tries below
#### Parallize JSON READ (doens't work) -> " Error in i:(i + 2) : argument of length 0" ####
library(doParallel)
# lapply for faster execution
cols[] <- lapply(most_sim1000[0,seq_len(ncol(most_sim1000)) %% 3 == 1])
cols[1] %in% target

myData <- new.env()
system.time({
  stream_in(textConnection(readLines(file_name, n = 500000)),
            handler = function(df){ 
              # Task: define COLUMN INDEX that matches, and save it & following 2
              if(!is.null(which(colnames(df) %in% target)) & length(which(colnames(df) %in% target)) > 0){
                # Define index of the df for saving of result -> always +1 than currently
                fill <- function(df){
                  idx <- as.character(length(myData) + 1)
                  i <- which(colnames(df) %in% target)
                  myData[[idx]] <- df[,i:(i+2)] 
                }
                lapply(df,fill)
              }
            },
            # Iteration size n = total rows (ca. 16M) / pagesize = Anzahl Lists
            pagesize = 500,

#### Read in JSON: Example - merge the whole DF ####
new_df <- new.env()
stream_in(file("C:/Users/janik/Downloads/patent_data_all/MOCK_DATA.json"), 
          handler = function(df){ 
  new_df <- rbind.data.frame(new_df,dplyr::filter(df$data[,1]<20))
  }, pagesize = 500)

####
# Ok, fuck it... let's assume I get the 100 scores, what now?

fuckit <- 

#### Read in JSON: Own Idea -- Loop with 1000: If number in the column name, take the whole column and save in empty DF ####

### TBD: 
# (!) change numbers for prev. defined variables
# Iteration: Change "2" into a reasonable no. for the dataset
# (?) change to WHILE -> not known how large the file is

# Pre-define
n_lines <- 1000          # lines per read
n_it <- 16000000/n_lines # rounds of interation, based on total patent no. (guess) ~ 16k
result <- matrix(ncol=101,nrow=11) # Define output matrix for matching patents

# Loop 1: Iterature through the whole JSON file
for (i in 0:2){
  # Read 1000 files each, skip previous 1000 ones 
  json <- read_lines("C:/Users/janik/Downloads/patent_data_all/most_sim.json",
           n_max = 100, skip = strtoi(paste(i,"00",sep ="")))
  # Loop 2: check each if 
}

# Package Problems
detach("package:pryr", unload=TRUE)
