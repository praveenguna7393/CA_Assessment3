
#Copying crime dataset in working directory
#loading the crime dataset in dataframe

Crime_df <- read.csv("IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv", header = TRUE, check.names = FALSE)
Crime_df

#Check.names argument was turned into false in order to prevent the x in front of the column names.

head(Crime_df)

#Checking the type of the data
class(Crime_df)

#struture of the dataframe
str(Crime_df)

#Dealing with the missing data in dataframe.
no_records <- Crime_df[!complete.cases(Crime_df),]
no_records
nrow(no_records)

#As per the result from complete.cases function there is no missing values in crime datasets.

library(mice)
md.pattern(Crime_df)

#By using the mice libaray the dataset was again observed as completely observed data.

#The objective of the analytics is to find the relationship between the poverty and crime rates in ireland.
# In the poverty datasets i have records only for the years 2006, 2011 and 2016.
# So other years of crime data or not need for the further analytics, so in crime dataframe 2006, 2011 and 2016
# records are needed and other quarter years are removed.

Crime_df <- Crime_df[grep("((2003|2004|2005|2007|2008|2009|2010|2012|2013|2014|2015|2017|2018|2019)Q[0-9])"
                          ,names(Crime_df),invert = TRUE)]
Crime_df

# Crime counts are measured in quarter wise of the year summing the quarters into whole year.

CR_2006 <- Crime_df$`2006Q1` + Crime_df$`2006Q2`+Crime_df$`2006Q3`+Crime_df$`2006Q4`  
CR_2006

CR_2011 <- Crime_df$`2011Q1`+ Crime_df$`2011Q2`+ Crime_df$`2011Q3`+ Crime_df$`2011Q4`
CR_2011

CR_2016 <- Crime_df$`2016Q1`+ Crime_df$`2016Q2`+ Crime_df$`2016Q3`+ Crime_df$`2016Q4`
CR_2016

Crime_df <- Crime_df[grep("((2006|2011|2016)Q[0-9])"
                          ,names(Crime_df),invert = TRUE)]
Crime_df

Crime_df <- data.frame(Crime_df,CR_2006,CR_2011,CR_2016)
Crime_df

names(Crime_df)

#Columns like region, Offence.code, offence and type of offence are not need for my analytics
#So i am droping these columns from my crime datasets.

Crime_df <- subset(Crime_df, select = c("GARDA.DIVISION","CR_2006","CR_2011","CR_2016"))
Crime_df

# Renaming the Column name GARDA.DIVISION into COUNTY.

names(Crime_df)[names(Crime_df) == "GARDA.DIVISION"] <- "COUNTY"
Crime_df

#Use unquie function to get the unique county values in column
df <- unique(Crime_df$COUNTY)
df
# I need Countywise crime counts for these i am going to replace the county values into distinct value.
# Before we replace county value we need to change the type column from factor to character.
                                       
library(plyr)                                       
Crime_df$COUNTY <- revalue(Crime_df$COUNTY,c("CORK CITY" = "CORK","CORK NORTH" = "CORK","CORK WEST" = "CORK",
                                             "D.M.R. SOUTH CENTRAL" = "DUBLIN","D.M.R. NORTH CENTRAL" = "DUBLIN",
                                             "D.M.R. NORTHERN" = "DUBLIN","D.M.R. SOUTHERN" = "DUBLIN",
                                             "D.M.R. EASTERN" = "DUBLIN","D.M.R. WESTERN" = "DUBLIN"))


library(reshape2)
Crime_df <- melt(Crime_df, id = c("COUNTY"))
Crime_df[order(Crime_df$COUNTY),]



library(sqldf)
aggr_county_df <- sqldf('SELECT COUNTY, SUM(CR_2006) AS Count_2006, SUM(CR_2011) AS Count_2011, SUM(CR_2016) AS Count_2016
      FROM Crime_df GROUP BY COUNTY')
aggr_county_df


county_field <- as.character(Crime_df$COUNTY)
Crime_df$COUNTY <- county_field
str(Crime_df)
Crime_df

