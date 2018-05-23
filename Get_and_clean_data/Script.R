swirl::install_course()
library(swirl)
swirl()

data(diamonds, package = "ggplot2")
head(diamonds, 4)
dim(head(diamonds, 4)) #to see dimensions of data.

install.packages("magrittr")
library("magrittr")

#with pipes you do what you did last you do it first
diamonds %>% head(4) %>% dim

x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)
round(exp(diff(log(x))), 1)

x %>% log() %>% diff() %>% exp() %>% round(1)

install.packages("dplyr")
library("dplyr")

head(diamonds, 5)
class(diamonds)
diamonds

#Select
select(diamonds, carat, price) #select from diamonds, carat, price attributes

#pipes
diamonds %>% select(carat, price)

#vector
diamonds %>% select(c(carat, price))

#Newer method
my_attributes <- c('carat', 'price')
select(diamond)

install.packages("hflights")
library(hflights)
head(hflights)
str(hflights)

?par
