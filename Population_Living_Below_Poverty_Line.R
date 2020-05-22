
Poverty_df <- read.csv("National_Poverty_Line.csv", header = TRUE)
Poverty_df

head(Poverty_df)

#Checking the type of the data
class(Poverty_df)

#struture of the dataframe bhbhjhj nkjh mnkjk mnjhj 
str(Poverty_df)

names(Poverty_df)

colnames(Poverty_df)

Poverty_population <- subset(Poverty_df, select = c("COUNTY","TOTPOP06","TOTPOP11","TOTPOP16","EDLOW_06",
                                                    "EDLOW_11","EDLOW_16","UNEMPM06","UNEMPM11","UNEMPM16",
                                                    "UNEMPF06","UNEMPF11","UNEMPF16"))
Poverty_population

no_records <- Poverty_population[!complete.cases(Poverty_population),]
no_records
nrow(no_records)

Poverty_population <- Poverty_population[!apply(is.na(Poverty_population) | Poverty_population == "", 1, all),]

# Changing the colname which make sense.
colnames(Poverty_population) <- c("COUNTY","Total_Population_2006","Total_Population_2011","Total_Population_2016",
                                  "Primary_Education_2006","Primary_Education_2011","Primary_Education_2016",
                                  "UnEmployement_Male_2006","UnEmployement_Male_2011","UnEmployement_Male_2016",
                                  "UnEmployement_Female_2006","UnEmployement_Female_2011","UnEmployement_Female_2016")

colnames(Poverty_population)

#Sum the umemployee rate of female and male

unEmployement_2006 <- Poverty_population$UnEmployement_Male_2006 + Poverty_population$UnEmployement_Female_2006
unEmployement_2011 <- Poverty_population$UnEmployement_Male_2011 + Poverty_population$UnEmployement_Female_2011
unEmployement_2016 <- Poverty_population$UnEmployement_Male_2016 + Poverty_population$UnEmployement_Female_2016

#Dropping the Unemployement columns of male and female
Poverty_population[, c("UnEmployement_Male_2006","UnEmployement_Male_2011","UnEmployement_Male_2016",
                       "UnEmployement_Female_2006","UnEmployement_Female_2011","UnEmployement_Female_2016")] <- list(NULL)

#Add the new unEmployement columns in dataframe

Poverty_population <- data.frame(Poverty_population,unEmployement_2006,unEmployement_2011,unEmployement_2016)
colnames(Poverty_population)

Poverty_population

#using the population column going to get the total number people under primary_eduction and unEmployement

PrimaryED_Counts_2006 <- Poverty_population$Total_Population_2006 * Poverty_population$Primary_Education_2006 / 100
PrimaryED_Counts_2011 <- Poverty_population$Total_Population_2011 * Poverty_population$Primary_Education_2011 / 100
PrimaryED_Counts_2016 <- Poverty_population$Total_Population_2016 * Poverty_population$Primary_Education_2016 / 100
unEmployee_Counts_2006 <- Poverty_population$Total_Population_2006 * Poverty_population$unEmployement_2006 / 100
unEmployee_Counts_2011 <- Poverty_population$Total_Population_2011 * Poverty_population$unEmployement_2011 / 100
unEmployee_Counts_2016 <- Poverty_population$Total_Population_2016 * Poverty_population$unEmployement_2016 / 100

#Dropping the column which are in proportion.
Poverty_population[, c("Primary_Education_2006","Primary_Education_2011","Primary_Education_2016",
                       "unEmployement_2006","unEmployement_2011","unEmployement_2016")] <- list(NULL)

Poverty_population <- data.frame(Poverty_population, PrimaryED_Counts_2006,PrimaryED_Counts_2011,PrimaryED_Counts_2016,
                                 unEmployee_Counts_2006,unEmployee_Counts_2011,unEmployee_Counts_2016)
colnames(Poverty_population)

Poverty_population

#Round the column values

library(dplyr)
Poverty_population <- Poverty_population %>% mutate_at(vars(PrimaryED_Counts_2006,PrimaryED_Counts_2011,PrimaryED_Counts_2016,
                                      unEmployee_Counts_2006,unEmployee_Counts_2011,unEmployee_Counts_2016),
                                 funs(round(., 0)))

Final_Proverty_PrimaryEducation_df <- subset(Poverty_population, select = c("COUNTY","PrimaryED_Counts_2006","PrimaryED_Counts_2011",
                                                   "PrimaryED_Counts_2016"))

Final_Proverty_UnEmployement_df <- subset(Poverty_population, select = c("COUNTY","unEmployee_Counts_2006","unEmployee_Counts_2011",
                                                                         "unEmployee_Counts_2016"))

#Add year column in the poverty dataset inorder to merge the crime datasets.
library(reshape2)
check_df <- melt(Final_Proverty_PrimaryEducation_df, id = c("COUNTY"))
check_df[order(check_df$COUNTY),]
class(check_df)

colnames(check_df)[2] <- "Year"
colnames(check_df)[3] <- "Primary_Education_Counts"

#Replacing the values
check_df$Year <- revalue(check_df$Year,c("PrimaryED_Counts_2006" = "2006","PrimaryED_Counts_2011" = "2011","PrimaryED_Counts_2016" = "2016")) 
check_df

Final_Poverty_primaryEd_df <- check_df
Final_Poverty_primaryEd_df

na.omit(Final_Poverty_primaryEd_df)

library(sqldf)
Final_Poverty_primaryEd_df <- sqldf('SELECT COUNTY, Year, SUM(Primary_Education_Counts) AS Primary_ED_Counts 
                        FROM Final_Poverty_primaryEd_df GROUP BY COUNTY, Year')
Final_Poverty_primaryEd_df

library(reshape2)
check_df1 <- melt(Final_Proverty_UnEmployement_df, id = c("COUNTY"))
check_df1[order(check_df$COUNTY),]
class(check_df1)

colnames(check_df1)[2] <- "Year"
colnames(check_df1)[3] <- "UnEmployeement_Counts"

#Replacing the values
check_df1$Year <- revalue(check_df1$Year,c("unEmployee_Counts_2006" = "2006","unEmployee_Counts_2011" = "2011","unEmployee_Counts_2016" = "2016")) 
check_df1

Final_Poverty_unEmployee_df <- check_df1
Final_Poverty_unEmployee_df

na.omit(Final_Poverty_unEmployee_df)

library(sqldf)
Final_Poverty_unEmployee_df <- sqldf('SELECT COUNTY, Year, SUM(UnEmployeement_Counts) AS UnEmployee_Counts 
                        FROM Final_Poverty_unEmployee_df GROUP BY COUNTY, Year')
Final_Poverty_unEmployee_df

