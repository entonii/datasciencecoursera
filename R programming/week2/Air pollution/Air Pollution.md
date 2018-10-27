# Introduction
## For this first programming assignment you will write three functions that are meant to interact with dataset that accompanies this assignment. The dataset is contained in a zip file specdata.zip that you can download from the Coursera web site.

The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file “200.csv”. Each file contains three variables:

Date: the date of the observation in YYYY-MM-DD format (year-month-day)

sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)

nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

# Part 1
## Write a function named ‘pollutantmean’ that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function ‘pollutantmean’ takes three arguments: ‘directory’, ‘pollutant’, and ‘id’. Given a vector monitor ID numbers, ‘pollutantmean’ reads that monitors’ particulate matter data from the directory specified in the ‘directory’ argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

```
pollutantmean <- function(directory, pollutant, id = 1:332) {

        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
}
```

## My code - Part 1

```
pollutantmean<-function(directory,pollutant,id=1:332){
  #create a list of files
list_files<-list.files(directory,full.names = TRUE)
#create dataframe
#create an empty data frame
dat <- data.frame()
#loop through the list of files until id is found
for(i in id){
  #read in the file
  temp<- read.csv(list_files[i],header=TRUE, sep = ",")
  dat<-rbind(dat,temp)
}
#find the mean of the pollutant, make sure you remove NA values
result<- mean(dat[,pollutant],na.rm = TRUE)
return(round(result,3))
}
```

# Part 2
## Writing a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows

```
complete <- function(directory, id = 1:332) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
}
```
## My code - Part 2

```
complete <- function(directory,id=1:332){
  
  #create a list of files
  list_files<-list.files(directory,full.names = TRUE)
  #create an empty data frame
  dat <- data.frame()
  
  for(i in id){
    #read in the file
    temp<- read.csv(list_files[i],header=TRUE)
    #delete rows that do not have complete cases
    temp<-na.omit(temp)
    
    #count all of the rows with complete cases
    nobs<-nrow(temp)
    
    #enumerate the complete cases by index
    dat<-rbind(dat,data.frame(i,nobs))
  }
  return(dat)
}
```

# Part 3
## Writing a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

```
corr <- function(directory, threshold = 0) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
}
```

## My code - Part 3

```
corr<-function(directory,threshold=0){
  #create list of file names
  filesD<-list.files(directory,full.names = TRUE)
  
  #create empty vector
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(filesD)){
    #read in file
    temp<- read.csv(filesD[i],header=TRUE)
    #delete NAs
    temp<-temp[complete.cases(temp),]
    #count the number of observations
    csum<-nrow(temp)
    #if the number of rows is greater than the threshold
    if(csum>threshold){
      #for that file you find the correlation between nitrate and sulfate
      #combine each correlation for each file in vector format using the concatenate function 
      #since this is not a data frame we cannot use rbind or cbind
      dat<-c(dat,cor(temp$nitrate,temp$sulfate))
    }
    
  }
  
  return(dat)
}
```
