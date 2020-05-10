library(plyr)
test1 <- ddply(merge(Final_Crime_df,Final_Poverty_primaryEd_df, all.x = TRUE),
      .(COUNTY, Year))

library(plyr)
test2 <- ddply(merge(Final_Crime_df,Final_Poverty_unEmployee_df, all.x = TRUE),
      .(COUNTY, Year))

Final_df <- ddply(merge(test1,test2, all.x = TRUE),
                  .(COUNTY, Year))
Final_df

head(Final_df)
temp_df <- Final_df
is.numeric(temp_df$COUNTY)
is.numeric(temp_df$Year)
is.numeric(temp_df$Crime_Counts)
is.numeric(temp_df$PrimaryED_Counts)
is.numeric(temp_df$UnEmployee_Counts)


temp_df$COUNTY <- as.numeric(Final_df$COUNTY)
temp_df$Year <- as.numeric(Final_df$Year)
temp_df$Crime_Counts <- as.numeric(Final_df$Crime_Counts)
str(temp_df)

install.packages("corrplot")
opar <- par(no.readonly = TRUE)
library(corrplot)
corrplot(corr = cor(temp_df),tl.col = "Black",tl.cex = 0.90)

pairs(temp_df)
head(temp_df)

#My County variable is categorical variable.
str(Final_df)

#Implementing statistical method into the datasets.






