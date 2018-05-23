library(swirl)
install_course_zip("C:/swirl_courses-master.zip", multi = TRUE, which_course = "Statistical Inference")
swirl()

##H0:mu=0.5  Null Hypothesis
##H1:mu not equal 0.5  Alternative hypothesis

##Test protocol:
##Step by step so that it can be produced again.
##Flip the coin a ceratin amount of times - samples
##Examine proportion of heads
##Record results
##Statistical test(do)
##We will judge significance by our p-value. if our p-value falls below a certain threshold, say 0.05, we will conclude our coin's behaviour is inconsistent with that of a nomral coin.
##P value to determine if null hypothesis is true or false
##If < 0.5 we fail to reject the null hypothesis.

##test = 1 sample t-test

##pwr() function this is pwr.p.test()##

##significance level is same as confidence level 0.05 is the default, 95% confident my hypothesis is correct.



install.packages("pwr")
library(pwr)
power_changes <- pwr.p.test(h = ES.h(p1 = 0.65, p2 = 0.50), #P1 alternative hypothesis and p2 is the null hypothesis
                            sig.level = 0.05,
                            power = 0.80,
                            alternative = "two.sided"
                            )

plot(power_changes)


effect_size <- cohen.ES(test = c("r"), #gives us the conventional effect size, instead of using table (page 31), use small to get more accuracy, but it means we need more records.
                        size = c("medium")) # r = effect size as per table in page 31.

power_test <- pwr.r.test(r = effect_size$effect.size,
                        power = 0.80, sig.level = 0.05)

plot(power_test)


effect_size <- pwr.p.test(h = c(0.2, 0.5, 0.8),
           n = 20,
           sig.level = 0.05)

effect_size
plot(effect_size)

#p value gives you the answer to whether you have hit type I or type II error, ensure to mention and calculate and if it's too close you go back and change significance level and calculate p value again.
#choose test
#choose pwr function for test

#CA3

#B0 and BJ is the outcome of R
#

students <- pwr.p.test(h = ES.h(p1 = 0.55, p2 = 0.50), #P1 alternative hypothesis and p2 is the null hypothesis
                            sig.level = 0.05,
                            power = 0.80,
                            alternative = "two.sided"
                            )

students <- cohen.ES(test = c("r"), #gives us the conventional effect size, instead of using table (page 31), use small to get more accuracy, but it means we need more records.
                        size = c("small")) # r = effect size as per table in page 31.

power_test <- pwr.r.2ptest(r = students$effect.size,
                        power = 0.80, sig.level = 0.05)

plot(power_test)
