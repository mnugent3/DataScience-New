#install rvest for web scrapping website
install.packages("rvest")
#install stringr to manipulate strings
install.packages("stringr")
library(rvest)
library(stringr)


####Final Code - after a lot, a lot of tweaking....######
#Specify url of website to be scrapped
url <- sprintf("http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=%d", 1:6)
#used sprint f instead of a loop to replace the last digit of the url with numbers 1 to 6
str(url)
url

#this functions reads the urls and collects the .locationname data field, it then strips whitespace, and the \n gets replaced with a comma
location_data <- lapply(url, function(x) {
    location_data_html <- read_html(x) # reads urls one by one
    location_name <- html_nodes(location_data_html, ".locationname") #reads .locationname field
    location_name <- html_text(location_name, trim = TRUE) #converts html to text, and trim strips whitespace
    location_data_list <- str_replace_all(location_name, "[\n]", ", ") #replaces \n with ,
    location_data_list
})

str(location_data)

#this functions does the same as the previous one but collect .address data field, it then strips whitespace, and the \n gets replaced with a comma
address_data <- lapply(url, function(x) {
    address_data_html <- read_html(x) # reads urls one by one
    address_name <- html_nodes(address_data_html, ".address") #reads .address data field from the urls
    address_name <- html_text(address_name, trim = TRUE) #converts html to text and trim will strip whitespace
    address_data_list <- str_replace_all(address_name, "[\n]", ", ") #replace \n with ,
    address_data_list
})

str(address_data)

#this section unlists the result from the previous two functions and places the data into a dataframe
location_data <- unlist(location_data) #unlist the results from each function
address_data <- unlist(address_data) #unlist the results from each function
Hospitals_30m <- as.data.frame(cbind(location_data, address_data)) #create a data frame for Hospitals within 30 mile radius
str(Hospitals_30m)
head(Hospitals_30m, 60)

##20 miles dataset
url <- sprintf("http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=20&pageNo=%d", 1:6) #used sprintf to replace the last digit of the url with numbers 1 to 6
str(url)
url

#this functions reads the urls and collects the .locationname data field, it then strips whitespace, and the \n gets replaced with a comma
location_data <- lapply(url, function(x) {
    location_data_html <- read_html(x) # reads urls one by one
    location_name <- html_nodes(location_data_html, ".locationname") #reads .locationname field
    location_name <- html_text(location_name, trim = TRUE) #converts html to text, and trim strips whitespace
    location_data_list <- str_replace_all(location_name, "[\n]", ", ") #replaces \n with ,
    location_data_list
})

str(location_data)

#this functions does the same as the previous one but collect .address data field, it then strips whitespace, and the \n gets replaced with a comma
address_data <- lapply(url, function(x) {
    address_data_html <- read_html(x) # reads urls one by one
    address_name <- html_nodes(address_data_html, ".address") #reads .address data field from the urls
    address_name <- html_text(address_name, trim = TRUE) #converts html to text and trim will strip whitespace
    address_data_list <- str_replace_all(address_name, "[\n]", ", ") #replace \n with ,
    address_data_list
})

str(address_data)

#this section unlists the result from the previous two functions and places the data into a dataframe
location_data <- unlist(location_data) #unlist the results from each function
address_data <- unlist(address_data) #unlist the results from each function
Hospitals_20m <- as.data.frame(cbind(location_data, address_data)) #create a data frame for Hospitals within 30 mile radius
str(Hospitals_20m)
head(Hospitals_20m, 60)

#Miles attribute are appended to new dataframes, one for 30 miles dataset and one for 20 miles dataset, attributes are filled with the according miles number
H30 <- data.frame(append(Hospitals_30m, c(Miles = '30'))) #add a column named miles and insert 30 into the data field in each row - this means 30 miles
H20 <- data.frame(append(Hospitals_20m, c(Miles = '20'))) # add a column named miles and insert 20 into the data field in each row - this means 20 miles
str(H20)
str(H30)

#The previously created dataframes are binded
hospitals_merged <- rbind(H20, H30) #bind the two dataframes, H20 first, then H30 beneath it
str(hospitals_merged)
head(hospitals_merged, 300) # show the first 300 records, it shows records that are 30 miles and records with 20 miles

#Dataframe is checked for duplicates, and placed in a new vector
duplicated(hospitals_merged$location_data, hospitals_merged$address_data) # check if any rows are duplicated
nrow(hospitals_merged) # count how many rows prior to removing duplicates, result is 300
final_hospital <- hospitals_merged[!duplicated(hospitals_merged$location_data, hospitals_merged$address_data),] #move none duplicated records to final_hospital
str(final_hospital)
nrow(final_hospital) # count how many rows after removing duplicates, result is 292, 8 duplicates found
duplicated(final_hospital$location_data, final_hospital$address_data) #check if the duplicates are gone

#final vectotr is placed into a dataframe
head(final_hospital, 100)
hospital <- as.data.frame(final_hospital)
length(final_hospital)
str(final_hospital) #structure of final_hospital
summary(final_hospital)
head(hospital, 100)

##END#############################################################################################################################################



#Attempt1 - at combining several urls, some code might be missing due to copy/cut and paste -> didn't work
url1 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=1'
url2 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=2'
url3 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=3'
url4 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=4'
url5 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=5'
url6 <- 'http://www.catholichealthinitiatives.org/body.cfm?xyzpdqabc=0&id=39758&action=list&sortby=state&ZIP=&DISTANCE=30&pageNo=6'
web_page <- c(url_1, url_2, url_3, url_4, url_5, url_6)
location_data <- html_nodes(web_page, ".locationname")
location_data <- html_text(location_data, trim = TRUE) #trim will strip whitespace
head(location_data)
length(location_data)
head(location_data)

web_page <- read_html(url)
address_data <- html_nodes(web_page, ".address")
address_data <- html_text(address_data, trim = TRUE)
head(address_data)
length(address_data)
head(address_data)

Hospitals_30m <- data.frame(hospital_name = location_data, address = address_data)

####END of Attempt 1######


###################################################################################################################################

#####WORKING####### But it only scrapped one page at a time when I had 6....so I tried to find another way of including the 6 pages
#Specify url of website to be scrapped
url <- 'http://www.catholichealthinitiatives.org/body.cfm?id=39758&action=list'

#read the hml code from the website
web_page <- read_html(url)
location_data <- html_nodes(web_page, ".locationname")
location_data <- html_text(location_data, trim = TRUE) #trim will strip whitespace
head(location_data)
length(location_data)
head(location_data)

web_page <- read_html(url)
address_data <- html_nodes(web_page, ".address")
address_data <- html_text(address_data, trim = TRUE)
head(address_data)
length(address_data)
head(address_data)

Hospitals_30m <- data.frame(hospital_name = location_data, address = address_data)

######WORKING########


#I did other attempts at reading tables but were all unsuccesful, as I really wanted to use the CSO data I intended to use for the CA.
#I deleted all these because they morfed into a mess