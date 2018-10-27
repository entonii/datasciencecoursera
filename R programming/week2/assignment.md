# Mean of a pollutant
## Writing a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows:

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


