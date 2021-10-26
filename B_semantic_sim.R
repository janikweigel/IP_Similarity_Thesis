## Goal: Extract 100 most semantically similar patents for target patent portfolio
## Approach: Stream large JSON similarity file: line by line with jsonlite::stream_in

library(dplyr)
library(readr)
library(jsonlite) # to stream_in large JSON file
library(pryr)
library(stringr)

# Define file location of similarity file
file_name <- "most_sim.json"

# Define target vector of patents, input format JSON file adjusted
# Format as follow, number is the USPN
target     <- c("X.10000000.",
                "X.10000001.")
                # ...

## JSON Streaming Function

## FULL RUN RESULTS
# 500k with 5000: 1400 sec (23.33min) 
# 2.5M with 5000: 9860sec (164min) 
# |-> 30sec per 10k lines

# Empty environment to work within the handler function
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



## Re-Write as Parallized Fct ## Does not work ##
library(foreach)

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
PatCols <- myData %>% as.list() %>% bind_cols()

# Target format: Wide-to-Long format, clean & as DT
library(data.table)
PatCols <- PatCols %>% 
  setDT() %>% 
  melt.data.table(measure.vars = patterns("^X\\.","^X1[:punct:]?", "^X2[:punct:]?"),
                  value.name = c("patent_no","similar_no","cosine"),
                  variable.name = "portfolio_id")


# Save 
write_csv(PatCols, "~/Master Thesis/IP_Similarity_Thesis/data_company/most_sim_patents.csv",
          col_names = T) #, append = F)





# Other tries below
#### Read in JSON: First Idea -- Loop with 1000 # If number in the column name, take the whole column and save in empty DF ####

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
