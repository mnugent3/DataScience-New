.libPaths()
.libPaths(c("C:/Users/Mon/Documents/R/win-library/3.4", "C:/Program Files/Microsoft/R Client/R_SERVER/library"))
install.packages("pwr")
install.packages("dplyr")
library(pwr)
library(dplyr)

#Import table
CIA01 <- read.csv("Data/CIA01.csv")
head(CIA01)

#Add column names
colnames <- c("State", "Household_Income", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015") #create a vector with the column names
colnames(CIA01) <- colnames #apply the column names to my_data dataframe
head(CIA01, 5) #display first 5 rows, so that we can check that the column names have changed.
str(CIA01)

#adjust columns to match HRA 31
CIA01 <- cbind("All_Admissions" = NA, "Disorder_Type" = NA, "Sexes" = NA, "2016" = NA, CIA01)
CIA01 <- CIA01[c(1:3, 5:16, 4)]
head(CIA01)

#The same steps are performed on the second dataset

#Import table
HRA31 <- read.csv("Data/HRA31.csv")
head(HRA31)

#Add column names
colnames <- c("All_Admissions", "Disorder_Type", "State", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016") #create a vector with the column names
colnames(HRA31) <- colnames #apply the column names to my_data dataframe
head(HRA31, 5) #display first 5 rows, so that we can check that the column names have changed.
str(HRA31)

#adjust columns to match CIA01 and VSD030
HRA31 <- cbind("Sexes" = NA, "Household_Income" = NA, HRA31)
HRA31 <- HRA31[c(3:4, 1, 5, 2, 6:16)]
head(HRA31)

#The same steps are performed on the third dataset

#Import table
VSD30 <- read.csv("Data/VSD30.csv")
head(VSD30)

#Add column names
colnames <- c("Sexes", "State", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016") #create a vector with the column names
colnames(VSD30) <- colnames #apply the column names to my_data dataframe
head(VSD30, 5) #display first 5 rows, so that we can check that the column names have changed.
str(VSD30)

#adjust columns to match HRA31 and CIA01
VSD30 <- cbind("All_Admissions" = NA, "Disorder_Type" = NA, "2006" = NA, "2007" = NA, "Household_Income" = NA, VSD30)
VSD30 <- VSD30[c(1:2, 6, 7, 5, 3:4, 8:16)]
head(VSD30)

#Check that all columns match
head(CIA01, 2)
head(HRA31, 2)
head(VSD30, 2)

#Now that all three datasets have the same columns in the same order, they can be binded vertically into one dataset

merged_dataset <- rbind(CIA01, HRA31, VSD30) #bind all three dataframes vertically.
head(merged_dataset)
str(merged_dataset)


#HYPOTHESIS TEST 1
#HRA31 subset - Did admissions go up during the recession period? -> H1
#attach(merged_dataset)
H1 <- subset(merged_dataset, merged_dataset$All_Admissions != "NA" | merged_dataset$Disorder_Type != "NA")
H1a <- H1[c(-2, -3, -4, -5)]
H1a
H1a$Before <- rowSums(H1a[, c("2006", "2007")], na.rm = T)
H1a$During <- rowSums(H1a[, c("2008", "2013")], na.rm = T)
H1a$After <- rowSums(H1a[, c("2014", "2016")], na.rm = T)
H1a
H1a <- H1a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)]
H1a
str(H1a)
pwr.chisq.test(w = 0.3, df = 3, sig.level = 0.5, power = 0.9) #tried sig.level of 0.2 because of my p value being so low, but I got the same results, also tried 0.5 effect size and 0.1 I didn't have enough data to use it.
H1_table_sample <- sample_n(H1a, 56) 
nrow(H1_table_sample)
H1_Totals <- colSums(H1_table_sample[, c("Before", "During", "After")], na.rm = T)
H1_Totals
H1_table <- as.table(as.matrix(H1_Totals)) # convert dataframe into a table so that power analysis be carried out
H1_table
chisq.test(H1_table)


#HYPOTHESIS TEST 2
#CIA01 subset Test wether some counties were affected more than others -> H2
#attach(merged_dataset)
H2 <- merged_dataset[c(-1, -2, -3, -5)]
H2
levels(H2$State)
H2a <- subset(H2, H2$State == "Southern and Eastern" | H2$State == "Border, Midland and Western")
#H2a <- subset(H2, H2$State == "Border" | H2$State == "Mid-West" | H2$State == "Southern and Eastern" | H2$State == "Border, Midland and Western" | H2$State == "Midland" | H2$State == "West" | H2$State == "South-West" | H2$State == "South-East"  | H2$State == "Ulster (part of)")
H2a$Before <- rowSums(H2a[, c("2006", "2007")], na.rm = T)
H2a$During <- rowSums(H2a[, c("2008", "2013")], na.rm = T)
H2a$After <- rowSums(H2a[, c("2014", "2016")], na.rm = T)
H2a
H2a <- H2a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)]
H2a
H2_Tables <- H2a %>% group_by(H2a$State) %>% summarise(sum(Before), sum(During), sum(After))
H2_Tables
str(H2_Tables)
pwr.chisq.test(w = 0.5, df = 2, sig.level = 0.5, power = 0.9) #tried sig.level of 0.2 because of my p value being so low, but I got the same results, also tried 0.5 effect size and 0.1 I didn't have enough data to use it.
H2_Totals <- as.table(as.matrix(H2_Tables)) # convert dataframe into a table so that power analysis be carried out
H2_Totals
str(H2_Totals)
chisq.test(table(H2_Totals))


#HYPOTHESIS TEST 3
#VSD30 subset Test wether suicide rates affected both, women or men more -> H3
H3 <- subset(merged_dataset, merged_dataset$Sexes != "NA")
H3a <- H3[c(-1, -2, -4, -5)]
H3a$Before <- rowSums(H3a[, c("2006", "2007")], na.rm = T)
H3a$During <- rowSums(H3a[, c("2008", "2013")], na.rm = T)
H3a$After <- rowSums(H3a[, c("2014", "2016")], na.rm = T)
H3a
H3a <- H3a[c(-2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)]
H3a
#There is no before recession data, so this column is removed
H3a <- H3a[c(-2)]
H3a
H3_Tables <- H3a %>% group_by(H3a$Sexes) %>% summarise(sum(During), sum(After))
H3_Tables
pwr.chisq.test(w = 0.5, df = 2, sig.level = 0.5, power = 0.9)
H3_Totals <- as.table(as.matrix(H3_Tables)) # convert dataframe into a table so that power analysis be carried out
H3_Totals
str(H3_Totals)
chisq.test(table(H3_Totals))


##END##

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





attach(my_data)
new_data <- subset(my_data, Age >= 35 | Age < 24, select = c(Q1, Q2, Q3, Q4))
new_data


Limavady_data <- subset(my_data_formatted, grepl('LIMAVADY', my_data_formatted$Locality) & grepl('LIMAVADY', my_data_formatted$Townland) & grepl('LIMAVADY', my_data_formatted$Town))

H1 <- subset(my_data_formatted, grepl('LIMAVADY', my_data_formatted$Locality) & grepl('LIMAVADY', my_data_formatted$Townland) & grepl('LIMAVADY', my_data_formatted$Town))

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


###Now that the three datasets have been binded/merged they can be analyzed
#There are three hypothesis to be tested, therefore the chosen test is the chi-quare test


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