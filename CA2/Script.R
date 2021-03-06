
#CA 2 - DATA ANALYSIS  - MONICA NUGENT - L00131284

#THERE ARE TWO DATASETS, EACH DATASET IS IMPORTED AND MANIPULATED TO MATCH EACH OTHER SO THAT THE TWO DATASETS CAN THEN BE MERGED


.libPaths() #I was having problems with dplyr so I had to set the library paths, otherwise it would not install, I had to delete the dplyr library folder several times.
.libPaths(c("C:/Users/Mon/Documents/R/win-library/3.4", "C:/Program Files/Microsoft/R Client/R_SERVER/library"))
install.packages("pwr")
install.packages("dplyr")
install.packages("data.table")
library(pwr)
library(dplyr)
library(data.table)


#DATASET 1 - IMPORT
HRA31 <- read.csv("Data/HRA31.csv")
head(HRA31)
str(HRA31)

#DATASET 1 - ATTRIBUTE NAMING
colnames <- c("All_Admissions", "Disorder_Type", "State", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016") #create a vector with the column names
colnames(HRA31) <- colnames #apply the column names to my_data dataframe
head(HRA31, 5) #display first 5 rows, so that we can check that the column names have changed.
str(HRA31)

##DATASET 1 - ATTRIBUTE MANIPULATION TO MATCH THE OTHER DATASETS
HRA31 <- cbind("Sexes" = NA, "Household_Income" = NA, HRA31) #Add columns to the dataset and fill the new columns with NA.
HRA31 <- HRA31[c(3:4, 1, 5, 2, 6:16)]
head(HRA31)
str(HRA31)

#DATASET 2 - IMPORT
VSD30 <- read.csv("Data/VSD30.csv")
head(VSD30)
str(VSD30)

#DATASET 2 - ATTRIBUTE NAMING
colnames <- c("Sexes", "State", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016") #create a vector with the column names
colnames(VSD30) <- colnames #apply the column names to my_data dataframe
head(VSD30, 5) #display first 5 rows, so that we can check that the column names have changed.
str(VSD30)

##DATASET 2 - ATTRIBUTE MANIPULATION TO MATCH THE OTHER DATASETS
VSD30 <- cbind("All_Admissions" = NA, "Disorder_Type" = NA, "2006" = NA, "2007" = NA, "Household_Income" = NA, VSD30) #Add columns to the dataset and fill the new columns with NA.
VSD30 <- VSD30[c(1:2, 6, 7, 5, 3:4, 8:16)]
head(VSD30)
str(VSD30)

#CHECK THAT THE TWO DATASETS HAVE MATCHING ATTRIBUTES
head(HRA31, 2)
head(VSD30, 2)


#BIND ALL TWO DATASETS VERTICALLY CREATING ONE MERGED DATASET.
merged_dataset <- rbind(HRA31, VSD30) #bind all three dataframes vertically.
head(merged_dataset)
str(merged_dataset)



#HYPOTHESIS TEST 1
#HRA31 subset - Did admissions go up during the recession period? -> H1
H1 <- subset(merged_dataset, merged_dataset$All_Admissions != "NA" | merged_dataset$Disorder_Type != "NA") # Create a subset from the merged dataset with All_Admissions and Disorder Type of attributes that don't have NA in them.
H1a <- H1[c(-2, -3, -4, -5)] # remove attributes that are not needed
H1a
H1a$Before <- rowSums(H1a[, c("2006", "2007")], na.rm = T) #sum the years attributes into before, during and after Ireland's recession period.
H1a$During <- rowSums(H1a[, c("2008", "2013")], na.rm = T) #same as above
H1a$After <- rowSums(H1a[, c("2014", "2016")], na.rm = T) #same as above
H1a
H1a <- H1a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)] #remove attributes that are not needed
H1a
str(H1a)
pwr_test_1 <- pwr.chisq.test(w = ES.w1(0.1, 0.3), df = 3, sig.level = 0.5, power = 0.9) #power chi square passed onto a variable so the N value can be used when creating a random sample, used small effect size with 3 degrees of freedom (4-1)
pwr_test_1 #show results from power test
plot(pwr_test_1) #plot power test
H1_table_sample <- sample_n(H1a, pwr_test_1$N + 1) #create a random sample using the N value from the power test 
str(H1_table_sample) # check structure and number of records and if they match the N value from the power test
H1_Totals <- colSums(H1_table_sample[, c("Before", "During", "After")], na.rm = T) #sum up the columns from the random sample
H1_Totals
H1_table <- as.table(as.matrix(H1_Totals)) # convert dataframe into a table so that power analysis be carried out
H1_table
str(H1_table)
chisq.test(H1_table) #chi square test performed on the random sample data as per power analysis resuts.


#HYPOTHESIS TEST 2
#VSD30 subset Test wether suicide rates affected both, women or men more -> H3
H3 <- subset(merged_dataset, merged_dataset$Sexes != "NA") # Create a subset from the merged dataset of All_Admissions and Disorder Type attributes that don't have NA in them.
H3a <- H3[c(-1, -2, -4, -5)] 
H3a$Before <- rowSums(H3a[, c("2006", "2007")], na.rm = T) #sum the years attributes into before, during and after Ireland's recession period.
H3a$During <- rowSums(H3a[, c("2008", "2013")], na.rm = T) #same as above
H3a$After <- rowSums(H3a[, c("2014", "2016")], na.rm = T) #same as above
H3a
H3a <- H3a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)] #remove attributes that are not needed
H3a
pwr_test_3 <- pwr.chisq.test(w = 0.3, df = 3, sig.level = 0.5, power = 0.9) #power chi square passed onto a variable so the N value can be used when creating a random sample, used small effect size with 2 degrees of freedom (4-1)
pwr_test_3 #show results from power test
plot(pwr_test_3) #plot power test
H3a <- sample_n(H3a, pwr_test_3$N + 1) #create a random sample using the N value from the power test 
H3a
str(H3a)
H3_Tables <- H3a %>% group_by(H3a$Sexes) %>% summarise(sum(During), sum(After)) #using pipes take H3a dataset, then group by state and summarize the before, during and after attributes
H3_Tables
H3_Totals <- as.table(as.matrix(H3_Tables)) # convert dataframe into a table so that power analysis be carried out
H3_Totals
str(H3_Totals)
chisq.test(table(H3_Totals)) #chi square test performed on the random sample data as per power analysis results.


##END - PLEASE READ PARAGRAPH BELOW##

# There are two hypothesis below that I was trying to get to work. 1) I didn't realize that my dataset 2 was only data based on income per county and that it didn't include depression diagnosis categories. So I had to start again with Hypothesis 2
# I created another subset of dataset 1, and this time manipulated it so that I ended up with State, Before, During and After. Somehow the chi-square test will not run on the table created from the dataframe. This is the same code I used on dataset 1
# to create the table. I spent countless hours trying to get chi square test to run on the table.

#Data set no longer needed
#DATASET 1 - IMPORT
CIA01 <- read.csv("Data/CIA01.csv")
head(CIA01)
str(CIA01)

#DATASET 1 - ATTRIBUTE NAMING
colnames <- c("State", "Household_Income", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015") #create a vector with the column names
colnames(CIA01) <- colnames #apply the column names to my_data dataframe
head(CIA01, 5) #display first 5 rows, so that we can check that the column names have changed.
str(CIA01)

#DATASET 1 - ATTRIBUTE MANIPULATION TO MATCH THE OTHER DATASETS
CIA01 <- cbind("All_Admissions" = NA, "Disorder_Type" = NA, "Sexes" = NA, "2016" = NA, CIA01) #Add columns to the dataset and fill the new columns with NA.
CIA01 <- CIA01[c(1:3, 5:16, 4)]
head(CIA01)
str(CIA01)



#HYPOTHESIS TEST 2
#HRA31 subset - Test wether some counties were affected more than others -> H2
H2 <- subset(merged_dataset, merged_dataset$All_Admissions != "NA" | merged_dataset$Disorder_Type != "NA") # Create a subset from the merged dataset with All_Admissions and Disorder Type of attributes that don't have NA in them.
H2
H2a <- H2[c(-3, -5)] # remove attributes that are not needed
H2a
H2a$Before <- rowSums(H2a[, c("2006", "2007")], na.rm = T) #sum the years attributes into before, during and after Ireland's recession period.
H2a$During <- rowSums(H2a[, c("2008", "2013")], na.rm = T) #same as above
H2a$After <- rowSums(H2a[, c("2014", "2016")], na.rm = T) #same as above
H2a <- H2a[c(-4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14)] #remove attributes that are not needed
H2a
str(H2a)
H2a <- subset(H2a, H2a$All_Admissions == "All_Admissions" | H2a$Disorder_Type == "All disorders")
H2a
str(H2a)
pwr_test_2 <- pwr.chisq.test(w = ES.w1(0.1, 0.3), df = 25, sig.level = 0.5, power = 0.9) #power chi square passed onto a variable so the N value can be used when creating a random sample, degrees of freedom (26-1) = 25
pwr_test_2 #show results from power test
plot(pwr_test_2) #plot power test
H2_table_sample <- sample_n(H2a, pwr_test_1$N + 1) #create a random sample using the N value from the power test 
str(H2_table_sample) # check structure and number of records and if they match the N value from the power test
H2_Totals <- H2_table_sample[c(-1, -2)]
H2_Totals
str(H2_Totals)
H2_table <- as.table(as.matrix(H2_Totals)) # convert dataframe into a table so that power analysis be carried out
#H2_table <- as.numeric(levels(H2_t))
#H2_table_1 <- data.table(lapply(H2_table, as.numeric), stringsAsFactors = FALSE)
#dt <- data.table(lapply(dt, as.character), stringsAsFactors = FALSE)

#H2_table <- H2_table[, Before := as.numeric(Before)] #convert attributes, Before, During and After to Numeric
#H2_table <- H2_table[, During := as.numeric(During)]
#H2_table <- H2_table[, After := as.numeric(After)]
str(H2_table)
H2_table
chisq.test(H2_table) #chi square test performed on the random sample data as per power analysis resuts.


#HYPOTHESIS TEST 2
#CIA01 subset Test wether some counties were affected more than others -> H2
H2a <- merged_dataset[c(-1, -2, -3, -5)] # Take the merged dataset and remove the columns that will not be required for this hypothesis test.
H2a
H2a$Before <- rowSums(H2a[, c("2006", "2007")], na.rm = T) #sum the years attributes into before, during and after Ireland's recession period.
H2a$During <- rowSums(H2a[, c("2008", "2013")], na.rm = T) #same as above
H2a$After <- rowSums(H2a[, c("2014", "2016")], na.rm = T) #same as above
H2a
H2a <- H2a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)] #remove attributes that are not needed
H2a
pwr_test_2 <- pwr.chisq.test(w = ES.w1(0.1, 0.3), df = 3, sig.level = 0.5, power = 0.9) #power chi square passed onto a variable so the N value can be used when creating a random sample, used small effect size with 3 degrees of freedom (4-1)
pwr_test_2 #show results from power test
plot(pwr_test_2) #plot power test
H2_table <- sample_n(H2a, pwr_test_2$N + 1) #create a random sample using the N value from the power test 
str(H2_table) # check structure and number of records and if they match the N value from the power test
H2_Tables <- H2a %>% group_by(H2a$State) %>% summarise(sum(Before), sum(During), sum(After)) #using pipes take H2a dataset, then group by state and summarize the before, during and after attributes
H2_Tables
str(H2_Tables)
H2_Totals <- as.table(as.matrix(H2_Tables)) # convert dataframe into a table so that power analysis be carried out
H2_Totals
str(H2_Totals)
chisq.test(table(H2_Totals)) #chi square test performed on the random sample data as per power analysis results.


########################################### CODE that wasn't used
#H2_Tables <- H2_Tables[-row(H2_Tables)[H2_Tables == 0],] removes rows with zero values
#H2_Tables$`sum(Before)` <- round(H2_Tables$`sum(Before)`) rounds column values
#H2_Tables$`sum(During)` <- round(H2_Tables$`sum(During)`) rounds column values
#H2_Tables$`sum(After)` <- round(H2_Tables$`sum(After)`) rounds column values
#H2_Tables
#df %>% group_by(grp) %>% summarise_all(funs(mean))
#H2a[, list(2006 = sum(2006)), by = State]
#data[, list(Frequency = sum(Frequency)), by = Category]
#H2a <- as.data.frame(xtabs(State ~ 2016, H2a))
#H2a <- H2a[, lapply(.SD, sum), by = State]
#H2a <- colSums(H2a[, c("Border")], na.rm = T)

#created subset but left dataset too small so I removed it
#H2a <- subset(H2, H2$State == "Kerry" | H2$State == "Meath" | H2$State == "Wicklow" | H2$State == "Dublin City" | H2$State == "South" | H2$State == "Ulster (part of)")
#H2a <- subset(H2, H2$State == "Border" | H2$State == "Mid-West" | H2$State == "Southern and Eastern" | H2$State == "Border, Midland and Western" | H2$State == "Midland" | H2$State == "West" | H2$State == "South-West" | H2$State == "South-East"  | H2$State == "Ulster (part of)")



attach(my_data)
new_data <- subset(my_data, Age >= 35 | Age < 24, select = c(Q1, Q2, Q3, Q4))
new_data


H1 <- merged_dataset[merged_dataset$All_Admissions != NA]

H1 <- as.table(merged_dataset)
H1[All_Admissions != NA]

merged_dataset[merged_dataset$All_Admissions != 'NA',]

my_data[my_data$Sepal.Length > 7 & my_data$Sepal.Width <= 3,]
head(H1)
##########

H1 <- merged_dataset[merged_dataset$All_Admissions %in% ,]

lapply(merged_dataset, All_Admissions)

H1 <- merged_dataset$All_Admissions, merged_dataset$`2006`


#merged_dataset[is.na(merged_dataset)] <- 0
#head(merged_dataset)

#First the dataframe needs to be converted into a table
#data_table <- matrix(unlist(merged_dataset))
data_table <- as.table(as.matrix(merged_dataset))

#NA's need to be removed otherwise the chisq function will not work on the data
merged <- replace(data_table, is.na(data_table), 0)
head(merged)
is.atomic(merged)
str(merged)


#Hypothesis 1

H1 <- merged[, 'All_Admissions' >= 1]
head(H1)




getElement[[merged, "All_Admissions" != 0]]
merged[["All_Admissions"]]
A[, 'X']

merged[merged$All_Admissions > 3.0,]

H1 <- merged[All_Admissions) > '0',]

H1 <- merged$.S3Class$All_Admissions

#merged <- as.matrix(merged)

merged <- as.table(as.matrix(merged_dataset))
chisq.test(table(merged.S3Class))

chisq.test(Comp1$homeownerstatus, Comp1$maritalstatus)


library("gplots")
merged <- as.table(as.matrix(merged_dataset))
balloonplot(t,(merged), main ="merged_dataset", xlab = "", ylab = "", label = FALSE)

#chisq_data <- chisq.test(table(merged))
#chisq_data