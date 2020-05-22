# Final dataframe was created by merging the National_population_line and crime_datasets
Crime_Analytics_df <- Final_df
Final_df

library(lattice)


#histogram was used in order to check the whether the varibles in dataframe
#are normally distributed or not.
histogram(~Primary_ED_Counts | Year, data = Final_df)


#QQ-Plot was used in order to find the normality distribution by using the reference line year 2006.
with(Crime_Analytics_df,
     qqplot(Crime_Analytics_df$Crime_Counts, 
            Crime_Analytics_df$Primary_ED_Counts,
            main = "Primary_ED_Counts vs Crime_Counts",
            xlab = "Crime_Counts",
            ylab = "Primary_ED_Counts"))

#The qq refernece line was drawn with the year '2006' for crime_counts.
with(Crime_Analytics_df, {
  qqnorm(Crime_Analytics_df$Crime_Counts[Crime_Analytics_df$Year == "2006"],
         main = "2006")
  qqline(Crime_Analytics_df$Crime_Counts[Crime_Analytics_df$Year == "2006"])
})

#In order to find the p-value to find the variables are normally distributed or not the function called shapiro was used.
normality_test <- shapiro.test(Crime_Analytics_df$Crime_Counts)
normality_test$p.value


#In order to find the p-value to find the variables are normally distributed or not the function called shapiro was used.
normality_test <- shapiro.test(Crime_Analytics_df$Primary_ED_Counts)
normality_test$p.value

# In order to find the relationship between the crime_counts and primary_eduction_counts the correlation test was 
# by using the spearmen method.

correlation_test = cor.test(Final_df$Crime_Analytics_df, Final_df$Primary_ED_Counts, method = 'spearman', exact = F)
correlation_test

install.packages('pwr')
library(pwr)
effective_size <- cohen.ES(test="r",size="large")
effective_size

# Power calculation analysis.
size_sample <- pwr.r.test(r=effective_size$effect.size, sig.level = 0.05, power = 0.9, alternative = "two.sided")
size_sample

# Hypothesis statstical method has been implemented in order to find the correlation between the two variables using spearman 
# function'
hypo <- cor.test(x=Crime_Analytics_df$Crime_Counts, y=Crime_Analytics_df$Primary_ED_Counts, method = 'spearman', exact = F)
hypo


