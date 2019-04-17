#Section-2
#solving the tasks using the NICrimeData datasets
#Part-A
#my path in pc
path <- "C:/Users/chaitanya vema/Documents/DATASCIENCE_CA2/ALLCSV/"
#creating lst of all .csv files into one folder
files <- list.files(path = path, pattern = "*.csv")
files
#read every .csv file in files and we should create a data frame with the same name as the csv file
for (i in 1:length(files))
  {
  assign(files[i], read.csv(paste(path, files[i], sep='')))
}

#As, every csv file is having the same format we can combine all the data into single dataset
#load the data those are combined into a dataframe with a name AllCrimeData
AllNICrimeData <- rbind.data.frame(`2015-01-northern-ireland-street.csv`, `2015-02-northern-ireland-street.csv`, 
                                   `2015-03-northern-ireland-street.csv`, `2015-04-northern-ireland-street.csv`, 
                                   `2015-05-northern-ireland-street.csv`, `2015-06-northern-ireland-street.csv`, 
                                   `2015-07-northern-ireland-street.csv`, `2015-08-northern-ireland-street.csv`, 
                                   `2015-09-northern-ireland-street.csv`, `2015-10-northern-ireland-street.csv`, 
                                   `2015-11-northern-ireland-street.csv`, `2015-12-northern-ireland-street.csv`,
                                   `2016-01-northern-ireland-street.csv`, `2016-02-northern-ireland-street.csv`, 
                                   `2016-03-northern-ireland-street.csv`, `2016-04-northern-ireland-street.csv`,
                                   `2016-05-northern-ireland-street.csv`, `2016-06-northern-ireland-street.csv`,
                                   `2016-07-northern-ireland-street.csv`, `2016-08-northern-ireland-street.csv`,
                                   `2016-09-northern-ireland-street.csv`, `2016-10-northern-ireland-street.csv`,
                                   `2016-11-northern-ireland-street.csv`, `2016-12-northern-ireland-street.csv`,
                                   `2017-01-northern-ireland-street.csv`, `2017-02-northern-ireland-street.csv`,
                                   `2017-03-northern-ireland-street.csv`, `2017-04-northern-ireland-street.csv`,
                                   `2017-05-northern-ireland-street.csv`, `2017-06-northern-ireland-street.csv`,
                                   `2017-07-northern-ireland-street.csv`, `2017-08-northern-ireland-street.csv`,
                                   `2017-09-northern-ireland-street.csv`, `2017-10-northern-ireland-street.csv`,
                                   `2017-11-northern-ireland-street.csv`, `2017-12-northern-ireland-street.csv`)
#displaying all the combined datasets
AllNICrimeData
#save the combined dataset as a csv file
write.csv(AllNICrimeData, "AllNICrimeData")

#counting the no.of rows in the dataset
NROW(AllNICrimeData)

#Part-B
#modifying the structure newly created and removing the required columns
AllNICrimeData <- data.frame(AllNICrimeData$Month, AllNICrimeData$Longitude, AllNICrimeData$Latitude, AllNICrimeData$Location, AllNICrimeData$Crime.type)
head(AllNICrimeData, 5)

#showing the structure of the modified data frame
str(AllNICrimeData)

#Part-C
#Factorising the crime type attributes
AllNICrimeData$AllNICrimeData.Crime.type <- as.factor(AllNICrimeData$AllNICrimeData.Crime.type)
str(AllNICrimeData)
View(AllNICrimeData)

#Part-D
# Modifying the Location column, so that it does'nt contain "On or near" part of the string
head(AllNICrimeData, 5)
AllNICrimeData$AllNICrimeData.Location <- gsub("On or near ", "", AllNICrimeData$AllNICrimeData.Location)
head(AllNICrimeData, 5)
#Replacing the NA values
head(AllNICrimeData, 5)
AllNICrimeData$AllNICrimeData.Location[AllNICrimeData$AllNICrimeData.Location == ""] <- NA
sum(is.na(AllNICrimeData$AllNICrimeData.Location))
head(AllNICrimeData, 5)

#PART-E
#make a subset from the source dataset
new_subset <- subset(AllNICrimeData, !is.na(AllNICrimeData.Location))
new_subset
#select a random sample of size 1000 from the newly created dataset
random_crime_sample <- data.frame(new_subset[sample(nrow(new_subset), 1000), ])
View(random_crime_sample)
library(dplyr)
#converting the location attributes in randoom crime sample to upper case
random_crime_sample$AllNICrimeData.Location <- toupper(random_crime_sample$AllNICrimeData.Location)
# creating a new dataset that contains postcode and primary thorfare information from NIPostcodes dataset
new_data <- NIPostcodes[, c(6,13)]
head(new_data, 5)
# deleting the duplicate values in primary thorfare column
new_data <- new_data[!duplicated(new_data$`Primary Thorfare`),]
colnames(new_data) <- c("Primary Thorfare", "Postcode")
str(new_data)
#adding a new column to the random cricle dataset and assigning the NA values
random_crime_sample$Postcode <-NA
head(random_crime_sample, 5)
#adding the values for the postcode column by matching the location with the primary thorfare in new_data
random_crime_sample$Postcode <- new_data$Postcode[match(random_crime_sample$AllNICrimeData.Location, new_data$`Primary Thorfare`)]
#structure for the random set
str(random_crime_sample)
#number of rows
NROW(random_crime_sample)
View(random_crime_sample)

#PART-F
# we already appended the postcodes to the random sample set above
# Now we will Save the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, "random_crime_sample.csv")

str(random_crime_sample)

#PART-G
updated_random_sample <- data.frame(random_crime_sample)
colnames(updated_random_sample) <- c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Postcode")
head(updated_random_sample, 3)

chart_data <- updated_random_sample
# Sort chart_data with respect to the Postcode and crime type
chart_data <- chart_data[order(chart_data$Postcode == "BT1", chart_data$Crime.type), ]
chart_data

# creating a new chart dataset that contains postcode = "BT1"
new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type), ]
str(new_chart)

# The summary for the crime type as per postcode and location

crime_type <- data.frame(new_chart$Crime.type)
library(plyr)
crime_type <- ddply(crime_type,.(new_chart$Crime.type),nrow)
colnames(crime_type) <- c("Crime_type", "Count")
crime_type
str(crime_type)

#PART-G
Crime_Data <- table(chart_data$Crime.type)
barplot(Crime_Data, main = "Crime Type Frequency", xlab = "Crime Type", ylab = "Frequency", col = "orange", border = "green",
        density = 100)
