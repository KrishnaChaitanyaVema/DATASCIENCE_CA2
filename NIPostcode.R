#importing NIPostcodes csv file to Rstudio
NIPostcodes <- read.csv("NIPostcodes.csv")
View(NIPostcodes)

#loading the NIPostcode data into the dataframe
NIPostcodes <- data.frame(NIPostcodes)

#From Section-1 
#The questions asked in the document are shown in the below coding
#PART-A
#to count the total no.of rows
nrow(NIPostcodes)
#the structure of the dataframe
str(NIPostcodes)
#From the NIpostcode data showing the first 10 rows
head(NIPostcodes, 10)


#PART-B
#Adding the suitable titles for each attribute of the data
column_names = c("Organisation Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfare", "Alt Thorfare", "Secondary Thorfare", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary Key")
colnames(NIPostcodes) <- column_names
head(NIPostcodes, 5)

#PART-C
#Removing the missing entries in the dataset
new_NIPostcodes <- na.omit(NIPostcodes)
new_NIPostcodes
#it is better to replace the missing values
NIPostcodes[NIPostcodes == ""] <- NA
View(NIPostcodes)

#PART-D
# we are creating the new dataframe and load the values of sum of missing values in each column in the dataset using the sapply()
na_sum <- data.frame(sapply(NIPostcodes, function(y) sum(length(which(is.na(y))))))
#Displaying the results of the sum
na_sum
#Now, we are creating the new dataframe and load the values of mean of missing values in each column in the dataset using the sapply()
na_mean <- data.frame(sapply(NIPostcodes, function(y) mean(is.na(y))))
na_mean


#PART-E
#Modifying the county attribute to be a categorising factor
NIPostcodes$County <- as.factor(NIPostcodes$County)
#The structure for the data
str(NIPostcodes)

#PART-F
#Moving the primary key identifier to the start of the dataset
#For moving the dataset we have to use the dplyr and the packages should be installed
install.packages("dplyr")
library(dplyr)

NIPostcodes <- NIPostcodes %>% select(`Primary Key`, everything())
head(NIPostcodes, 5)
View(NIPostcodes)

#PART-G
#to store the fields that are having in the town as LIMAVADY and saving the file "LIMAVADY.csv"
Limavady_data <- data.frame(NIPostcodes$Locality, NIPostcodes$Townland, NIPostcodes$Town) %>% filter(NIPostcodes$Town == "LIMAVADY")
Limavady_data
write.csv(Limavady_data, "Limavady.csv")
str(Limavady_data)

#PART-H
#storing the cleaned data into another file name called CleanNIPostcodeData
write.csv(NIPostcodes, "CleanNIPostcodeData.csv")
