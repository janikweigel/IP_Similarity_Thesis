library(dplyr)
library(readr)
library(jsonlite)
library(pryr)
library(stringr)

# Idea: Stream large JSON similarity file: line by line with jsonlite::stream_in

# Define file location
file_name <- "C:/Users/janik/Downloads/patent_data_all/most_sim.json"

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
target <- read.csv("~/Master Thesis/IP_Similarity_Thesis/data_company/elkamet_clean_full.csv",
                   # To manipulate the data -> strings needed
                   stringsAsFactors=FALSE) %>% 
  select(`us_pat_no`) %>% 
  as.character(.) %>% 
  strsplit(., split = ",")

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
## ME: ??
which(colnames(most_sim1000) %in% target_simp)
most_sim1000[,997:999]

## Let's do itt
# target for function building (early result)
target_fnct <- target %>% 
  add_row(`us_pat_no`= 10000900) %>% 
  as.data.frame(.) %>% 
  
 #  unlist(.) %>% 
  toString(.)
# mutate(`us_pat_no`= str_replace(`us_pat_no`,"(.+)","X.\\1.")) %>% 

target <- c("X.10000340.",
"X.8628137.",
"X.5921663.",
"X.8768153.",
"X.6769700.",
"X.9186984.",
"X.9073421.",
"X.9994094.",
"X.10215337.",
"X.10414259.",
"X.10315342.",
"X.8444205.")



?str_replace
# adjust fct
myData <- new.env()
stream_in(textConnection(readLines(file_name, n=1000)),
          handler = function(df){ 
            # Define index of the df for saving of result -> always +1 than currently                     
            idx <- as.character(length(myData) + 1)
            # Task: defines the column that matches, and save it + following 2
            i <- which(colnames(most_sim1000) %in% target)
            myData[[idx]] <- df[,i:(i+2)]
          },
          # Iteration size n = total rows (ca. 16M) / pagesize = Anzahl Lists
          pagesize = 500) ## change back to 1000

pryr::object_size(myData)



## Other example: merge the whole DF 
new_df <- new.env()
stream_in(file("C:/Users/janik/Downloads/patent_data_all/MOCK_DATA.json"), 
          handler = function(df){ 
  new_df <- rbind.data.frame(new_df,dplyr::filter(df$data[,1]<20))
  }, pagesize = 500)

?cat
#### Own Idea -- Loop with 1000: If number in the column name, take the whole column and save in empty DF ####

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