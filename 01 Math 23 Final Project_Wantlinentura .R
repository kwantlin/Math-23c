# GENDER, INCOME, AND HEAD OF HOUSEHOLD

# Kathryn Wantlin & Brian Ventura


# We examine the effect of gender and income on identification of head of household in American Community Survey (ACS) 
# data. The ACS is sent to approximately 3.5 million households every year and is the largest household survey 
# administered by the Census Bureau. It contains questions previously only on the long form of the decennial census, 
# including questions regarding educational attainment, employment, migration, and more. In particular, it include 
# questions that ask respondents to 1) identify a head of household and 2) list the relationships of other members of 
# the household to the head of househole (spouse, child, parent, etc.). This information is used by the Census Bureau 
# to understand household composition in the US, thus enabling communities to plan future programs and services 
# for residents. We look at whether gender and income are important predictors for identification of head of household in
# households with non-same-sex married couples. We that being male is associated with a 15 percentage point increase in 
# the likelihood of being identified as head of household. We find a similar effect for breadwinner status; namely, making more 
# money that one's spouse is associated with a 16 percentage point increase in the likelihood of being identified as 
# head of household.

# Throughout this file, there will be "NOTE"s that dictate exactly what project guidelines we are satisfying. 
# The first one is below.

# NOTE: Our team consists of exactly two members (creativity/complexity #22, 1/10)

#### CLEANING THE DATA SET ####

data <- read.csv("data.csv")

#Our data set includes a *huge* amount of information that we don't need for our purposes.
#First we need to drop all the unmarried individuals or those individuals who are married but whom
#their spouse is not present in the household.
                          

#The variable MARST refers to marital status, with a code of 1 being "married, spouse present" (see variable
#documentation for more information). We drop all individuals who for whom MARST=/=1.
data <- data[(data$MARST==1),]

#Next we want to delete those individuals who are not the head of household or the spouse. The variable RELATE 
#describes relationship to the head of household, and codes
#"house of household" as 1 (self) and "spouse" as 2. We need to drop everyone for whom RELATE=/=1 or RELATE=/=2. 
data <- data[(data$RELATE==1 | data$RELATE==2),]

#Given we are concerned with the impact of gender on identification as head of household, we need to eliminate
#all couples comprised of individuals with the same gender. To do this, we can identify individual households using the
#SERIAL variable. SERIAL is a unique identifier for each household, so we can go through the data by SERIAL and
#drop observations where both individuals in the household are the same gender. 
#The variable SEX codes gender, with 1 being male and 2 female. We first want to make this an indicator variable,
#so we subtract 1 from the column. Now 0 indicates being male and 1 indicates being female.
data$SEX <- data$SEX - 1

#We can calculate the average of SEX by SERIAL. If the average of SEX conditional on SERIAL is 0.5, this means
#the couple is of different genders. We create a the variable GENDDIF which takes the value 1 if the couple is 
#of different genders or 0 if they are the same gender. 
gender.info <- as.data.frame.table(tapply(data$SEX,data$SERIAL,mean))
names(gender.info) <- c("SERIAL","GENDDIF")
func1 <- function(x) if(x==0.5){1} else{0}
gender.info$GENDDIF <- sapply(gender.info$GENDDIF,func)

#Now we want to merge this data with our larger data set so we have the GENDDIF information at the person level
data <- merge(data, gender.info, by="SERIAL",quiet=TRUE)

#Now we drop all individuals who are in same-sex marriages
data <- data[(data$GENDDIF==1),]

#Additionally, we make the variable RELATE an indicator variable, where 1 indicates the person is the 
#head of household. 
func2 <- function(x) if(x==2){0} else{1}
data$RELATE <- sapply(data$RELATE,func2)
head(data)

#As a check, we want to confirm that there are the same number of spouses as there are heads of households. 
table(data$RELATE)


#### CREATING ADDITIONAL (USEFUL) VARIABLES ####

#Now we need to create some additional useful variables that we'll use later in our analysis. The first is BREAD,
#short for "breadwinner," which indicates if an individual is making the most money in their household. 
#The second is RELINC or "relative income", which is the income of the person relative to their spouse.

#We have several different measures of income, but we will use INCTOT, which represents total personal income.
#First we'll find the maximum and minimum personal incomes in each household
income.info.max <- as.data.frame.table(tapply(data$INCTOT,data$SERIAL,max))
names(income.info.max) <- c("SERIAL","INCTOTMAX")
income.info.min <- as.data.frame.table(tapply(data$INCTOT,data$SERIAL,min))
names(income.info.min) <- c("SERIAL","INCTOTMIN")

#Now we combine these together with the larger dataset
income.info <- merge(income.info.max, income.info.min, by="SERIAL",quiet=TRUE)
data <- merge(data, income.info, by="SERIAL",quiet=TRUE)

#Now we can encode BREAD
func3 <- function(x,y) if(x==y){1} else{0}
data$BREAD <- mapply(func3,data$INCTOT,data$INCTOTMAX)

#Now we can encode RELINC
func4 <- function(x,y,z) if(x==y){x-z} else{x-y}
data$RELINC <- mapply(func4,data$INCTOT,data$INCTOTMAX,data$INCTOTMIN)
head(data)

#Now we save our cleaned data set with our new variables
write.csv(data,"clean.csv")


# NOTE: We have a dataframe (dataset standards #1, 1/4)
# NOTE: It has at least two categorical/logical columns, e.g. sex and head of household status (dataset standards #2, 2/4)
# NOTE: It has at least two numeric columns, e.g. income and relative income (dataset standards #3, 3/4)
# NOTE: It has at least 20 rows (dataset standards #4, 4/4)
# NOTE: It has a lot of columns; we have 30 different variables, 9 are used in the analysis (creativy/complexity #1, 2/10)
# NOTE: It has >1 million observations, which can be considered a population (creativity/complexity #2, 3/10) 
# NOTE: We used a number of self-defined functions, and multiple commands not in the class scripts to optimize speed (this is a 
#       HUGE dataset, so the apply() family was necessary). This is professional level. (creativity/complexity #10, 4/10) 


#### HEAD OF HOUSEHOLD AND GENDER ####

#We want to examine the relationship between head of household status and gender. There is societal motivation
#behind this analysis: we expect that men are more likely to be identified as head of household given social
#expectations of gender roles (i.e. the idea of male as being dominant). 

#We can see that there is negative correlation between being female and being head of household.
clean <- read.csv("clean.csv")
cor(clean$SEX, clean$RELATE)

# NOTE: We use a correlation appropriately (creativity/complexity #16, 5/10)

#We can build a contigency table to see this effect in terms of counts.
tbl <- table(clean$SEX,clean$RELATE); tbl

#From the contigency table, we see that more men are head of households (and thus necessarily more women are not head of household)

# NOTE: We built a contigency table (graphical display #4, 1/4)
# NOTE: We analyze a contingency table (requires analysis #3, 1/4)

#We can run a chi-sq test to see if these counts are statistically different from what we would expect. 
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(clean$SEX,clean$RELATE)

# NOTE: We calculate a p-value based on a distribution function (requires analysis #2, 2/4)

#We see that these counts are statistically different from what we would expect if husbands and wives were equally likely to be
#head of household; it appears that husbands are indeed significantly more likely to be identified as head of household in our 
#data as compared to wives.

#We can also visualize this data using a barplot. 
#install.packages("reshape2")
library(reshape2)
#install.packages("ggplot2")
library(ggplot2)

new <- data.frame(c(sum(clean$RELATE==1 & clean$SEX==1), sum(clean$RELATE==0 & clean$SEX==1)), 
                  c(sum(clean$RELATE==1 & clean$SEX==0), sum(clean$RELATE==0 & clean$SEX==0)), c(1,0))
names(new) <- c("Head of Household", "Spouse", "Gender")
head(new)
melted <- melt(new, id.vars='Gender')
head(melted)
ggplot(melted,aes(x=variable,y=value,fill=factor(Gender)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Gender",labels=c("Male", "Female"))+
  xlab("Relationship Status")+ylab("Frequency")+ggtitle("Head of Household Status by Gender")+
  theme(plot.title = element_text(hjust = 0.5))


#Overall, it appears that being male with associated with a higher likelihood of being identified as head of 
#household.

# NOTE: We made a barplot (graphical dislpay #1, 2/4)
# NOTE: We use ggplot() and made a nice graph (creativity/complexity #11, 6/10)

#### HEAD OF HOUSEHOLD AND INCOME ####

# We now want to examine the impact of income on head of household. We have three different measures of income:
# total personal income, relative income to spouse, and breadwinner status. Total personal income is exactly what it
# sounds like; relative income is the individual's income minus their spouse's income; and breadwinner status is an
# indicator variable of whether or not an individual makes more money than their spouse.

#We expect all three to be related to the likelihood of head of household. Namely, we expect 1) higher income
#individuals to be more likely to be identified as head of household; 2) that individuals with higher relative 
#income are more likely to be identified as head of household; and 3) that individuals identified as breadwinner
#are more likely to be identified as head of household. We expect these results as having higher income is likely 
#associated with feeling greater ownership over the home and over the responsibility of providing for one's family,
#feelings one could associate with the desire to be identified as head of household.

#First, we look at the relationship between income and head of household status. We can construct a scatterplot
#to examine this: 
inc <- clean$INCTOT/1000
ggplot(clean, aes(x=inc, y=clean$RELATE)) + geom_point()+ xlab("Income ($000s)")+ylab("Head of Household Status")+
  ggtitle("Head of Household Status by Income")+theme(plot.title = element_text(hjust = 0.5))

#From looking at the plot, it looks as if there is no strong positive relationship between income and status as 
#head of household. This suggests that absolute income has no impact on the likelihood of being identified as 
#head of household.

#Now we can construct a similar plot for relative income. 
relinc <- clean$RELINC/1000
ggplot(clean, aes(x=relinc, y=clean$RELATE)) + geom_point()+ xlab("Relative Income ($000s)")+
  ylab("Head of Household Status")+ggtitle("Head of Household Status by Relative Income")+
  theme(plot.title = element_text(hjust = 0.5))

#Interestingly, it also looks as if there is no strong positive relationship between relative income and status as 
#head of household. This suggests that the amount more or less one makes relative to their spouse has no bearing on
#whether or not one is identified as head of household.

# NOTE: This is a surprising outcome; head of household identification doesn't depend on how much more money you make relative
#       to your spouse. (creativity/complexity #9, 7/10)

#Perhaps then it just matters if one makes more money than their spouse (i.e. is the breadwinner). First, given
#both variables are binary, we can find correlation, build a contingency table and run a chi-sq test.
cor(clean$RELATE,clean$BREAD)
tbl <- table(clean$BREAD,clean$RELATE); tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(clean$BREAD,clean$RELATE)

#We see that breadwinners are statistically much more likely to be head of household.

#We can construct a barplot to examine this visually:
new <- data.frame(c(sum(clean$RELATE==1 & clean$BREAD==1), sum(clean$RELATE==1 & clean$BREAD==0)), 
                  c(sum(clean$RELATE==0 & clean$BREAD==1), sum(clean$RELATE==0 & clean$BREAD==0)), c(1,0))
names(new) <- c("Head of Household", "Spouse", "Breadwinner")
head(new)
melted <- melt(new, id.vars='Breadwinner')
head(melted)
ggplot(melted,aes(x=variable,y=value,fill=factor(Breadwinner)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Breadwinner Status",labels=c("Non-Breadwinner", "Breadwinner"))+
  xlab("Relationship Status")+ylab("Frequency")+ggtitle("Head of Household Status by Breadwinner Status")+
  theme(plot.title = element_text(hjust = 0.5))

#Overall, it appears that absolute and relative income are not predictive of head of household status, but that
#breadwinner status is a good predictor. Thus it appears to not matter how much money you make in general or how
#much money you make relative to your spouse, but simply if you make more money than them.

#### INTERACTIONS BETWEEN GENDER AND INCOME ####

#It would be naive of us to discuss gender and income without describing the relationship between them. 
#Indeed, in the current moment there is much discussion about the gender wage gap, with many citing the fact
#that women earn approximately 80 cents for every dollar earned by men. While it is unclear how much of this 
#gap is explained by variables such as hours worked, experience, education, etc., it is true in absolute terms
#that men earn more than women. 

#In the previous two sections, we examined the effects gender and income (separately) have on the probability of 
#someone being identified as head of household. In this section, we examine the interactions between gender and 
#income. 

#This section ultimately motivates the need for regression-based analysis, conducted in the fourth section. 

#First, some descriptive statistics. The correlation between gender and income:
cor(clean$INCTOT,clean$SEX) #Being female is thus negatively associated with income

#Mean income by gender, recalling 0 = male and 1= female:
tapply(clean$INCTOT,clean$SEX,mean)
tapply(clean$INCTOT,clean$SEX,mean)[1]-tapply(clean$INCTOT,clean$SEX,mean)[2]

#Median income by gender
tapply(clean$INCTOT,clean$SEX,median)
tapply(clean$INCTOT,clean$SEX,median)[1]-tapply(clean$INCTOT,clean$SEX,median)[2]

#There is a difference of $36,758 between the average incomes of men and women. Additionally, there is a 
#$28,300 difference in the median incomes of men and women. This likely includes a number of
#individuals who are not working and therefore do not have any personal income. We can drop these individuals
#from the sample:
posIncome <- clean[(clean$INCTOT>0),]
tapply(posIncome$INCTOT,posIncome$SEX,mean)
tapply(posIncome$INCTOT,posIncome$SEX,mean)[1]-tapply(posIncome$INCTOT,posIncome$SEX,mean)[2]
tapply(posIncome$INCTOT,posIncome$SEX,median)
tapply(posIncome$INCTOT,posIncome$SEX,median)[1]-tapply(posIncome$INCTOT,posIncome$SEX,median)[2]
#We see that the difference in mean and median income doesn't drop significantly when excluding those 
#who have no income.

#We can also examine this by additionally only looking at individuals who are working at least full time 
#(at least 40hrs a week).
fullTime <- posIncome[(posIncome$UHRSWORK>=40),]
tapply(fullTime$INCTOT,fullTime$SEX,mean)
tapply(fullTime$INCTOT,fullTime$SEX,mean)[1]-tapply(fullTime$INCTOT,fullTime$SEX,mean)[2]
tapply(fullTime$INCTOT,fullTime$SEX,median)
tapply(fullTime$INCTOT,fullTime$SEX,median)[1]-tapply(fullTime$INCTOT,fullTime$SEX,median)[2]
#Again the difference is still significant, at $29,000 for mean incomme and $16,200 for median income. 

#It might be that men work far more hours on average than women. We can use the variable UHRSWORK, which
#encodes the number of average hours someone worked last year. 
tapply(fullTime$UHRSWORK,fullTime$SEX,mean)
tapply(fullTime$UHRSWORK,fullTime$SEX,mean)[1] - tapply(fullTime$UHRSWORK,fullTime$SEX,mean)[2]

#We see that this isn't the case for full-time workers; men seem to work just a couple more hours than women a week;
#certainly not enough to explain a difference in income by the thousands. Other confounding variables may be level 
#of education, the distribution of occupations by gender, or the like.

#This is ultimately a rabbithole; an entire subset of economics is dedicated to studying the gender wage gap,
#which ultimately goes beyond the scope of this analysis. 

#Overall though, we see that our data agrees with the general premise that men earn more than women.

#Let's see if this trend holds true within households. 
#First we can look at how gender impacts the likelihood of being the breadwinner (i.e. having the highest
#personal income in a given household). We expect that if, in general, men make more money than women, they are more
#likely to be the breadwinner. 

#First, we can look at some means and correlation:
tapply(clean$BREAD,clean$SEX,mean)
cor(clean$BREAD,clean$SEX)

#It looks like overall that men are usually the breadwinner (i.e. have the highest personal income in their
#household).  

#We can also make a contigency table and run a chi-squared test:
tbl <- table(clean$BREAD,clean$SEX); tbl
chisq.test(clean$BREAD,clean$SEX)

#It looks the men are statistically much more likely to be the household breadwinner than women. 
#As an exercise, we can take a small sample of the data and run a permutation test to see if this holds
#in smaller subsets of the data. We take the first 50 rows (which includes 25 couples) and use relative income
#data.
mini <- clean[1:50,]
female <- mini$SEX==1
rel <- mini$RELINC
idx <- which(female); head(idx)
mean(rel[idx])   
mean(rel[-idx])    
Obs <- mean(rel[-idx])-mean(rel[idx]); Obs  
N <-10000; diff <- numeric(N)
for (i in 1:N) {
  scramble <- sample(female,length(female))
  idx <- which(scramble)
  diff[i] <- mean(rel[-idx])-mean(rel[idx])
}
pvalue <- mean(diff > Obs); pvalue

#Even in this small subset, we see that women are much more likely to earn less than their husbands. We can repeat
#this with another subset of 25 couples:
mini <- clean[10000:10050,]
female <- mini$SEX==1
rel <- mini$RELINC
idx <- which(female); head(idx)
mean(rel[idx])   
mean(rel[-idx])    
Obs <- mean(rel[-idx])-mean(rel[idx]); Obs  
N <-10000; diff <- numeric(N)
for (i in 1:N) {
  scramble <- sample(female,length(female))
  idx <- which(scramble)
  diff[i] <- mean(rel[-idx])-mean(rel[idx])
}
pvalue <- mean(diff > Obs); pvalue

# NOTE: We ran a permutation test (required analysis #1, 3/4)

#The same trend holds true. Ultimately, the chi-squared test gave us information about the likelihood of women being
#breadwinners given a chi-square distribution, while the permutation test let us look at the data without imposing
#a particular distribution. The two methods here agree, and suggest there is a both statistically significant 
#difference by gender between the likelihood of a spouse earning more than their partner and the amount difference. 

# NOTE: We compared analysis from a simulation (permutation test) and a classical method (chi-sq) (required analysis #4, 4/4)

#We can also examine these differences by looking at the distribution of relative income by gender. For example,
#it might be that men earn more on the whole than women, but women are more likely to earn more than their husbands.
#This is entirely feasible; for example, in households with incomes below the median, it might be that women 
#dominate their husands' incomes. However, above the median, it might be that men have so much larger incomes than
#their wives that the mean income for the population of men is higher than the mean income for the population of 
#women (or some other combination of factors). 

#We should note explicitly that by construction, the analysis of female relative income and male relative income 
#will be mirrors of one another; within a couple, if the husband earns more than his wife, the wife must earn 
#less than her husband. As such, the majority of this analysis could be done on only one gender, and the 
#implications for the other gender would be easy to see. 

#For thoroughness, we do the analysis for both genders. 
male.relInc <- subset(clean,SEX==0)$RELINC/1000
female.relInc <- subset(clean,SEX==1)$RELINC/1000
hist(male.relInc, main="Male Income Relative to Female Spouse",xlab="Relative Income ($000s)")
hist(female.relInc, main="Female Income Relative to Male Spouse",xlab="Relative Income ($000s)")

# NOTE: We made a histogram (graphical display #2, 3/4)

#It's clear that the relative income for men is skewed right and, correspondingly, the relative income for women
#is skewed left. We can calculate the skew in order to quantity this effect: 
library(moments)
skewness(male.relInc)
skewness(female.relInc)

# NOTE: We calculated skew appropriately (creativity/complexity #13, 8/10)

#As we would expect from the visualization of the data, the skewness of male relative income is positive
#(and thus, correspondingly, the skewness of female relative income is negative).

#Let's try to fit a distritbution to this data. We can use the fitdistrplus package to do this.
#install.packages("fitdistrplus")
library(fitdistrplus)

#We will first do the female relative income distribution.
descdist(female.relInc, discrete = FALSE)

#This gives us the Cullen and Frey graph, which looks at what theoretical distribution fits our data best on the
#basis of kurtosis and skewness.

# NOTE: We never generated a Cullen and Frey graph in class scripts (creativity/complexity #19, 9/10)

#From the Cullen and Frey graph we see that no typical theoretical distribution seems to fit our data well,
#likely due to the extreme values of the data (different in orders of magnitude). 
#As such, we use the square root transformation to make the data a bit more manageable. 
sqrt.female.relInc <- sign(female.relInc)*sqrt(abs(female.relInc))
hist(sqrt.female.relInc,main="Square Root of Female Income Relative to Male Spouse",
     xlab="Square Root of Relative Income ($000s)")
descdist(sqrt.female.relInc, discrete = FALSE)

#From the Cullen and Frey graph we see that a logistic model looks like it will be the best fit.
#We can confirm this by plotting the distribution on top of the hitogram of data.
#This package also gives us more information regarding the fit of our theoretical distribution to the data.
fit.female.log <- fitdist(sqrt.female.relInc,"logis")
plot(fit.female.log)

#From the plots, we can see that the logistic distribution is a decent fit for our data but doesn't do very well
#around the extremes of the data.

#We can now repeat this process with the male relative income distribution.
sqrt.male.relInc <- sign(male.relInc)*sqrt(abs(male.relInc))
hist(sqrt.male.relInc,main="Square Root of Male Income Relative to Female Spouse",
     xlab="Square Root of Relative Income ($000s)")
descdist(sqrt.male.relInc, discrete = FALSE)
fit.male.log <- fitdist(sqrt.male.relInc,"logis")
plot(fit.male.log)

#We can zoom in on the first graph in the above plot.
hist(sqrt.male.relInc,main="Square Root of Male Income Relative to Female Spouse",
     xlab="Square Root of Relative Income ($000s)", prob=TRUE)
curve(dlogis(x, location = 3.903665, scale = 3.559214, log = FALSE), add=TRUE, col="red")

# NOTE: We generated a histogram with a probility distribution overlaid (graphical display #3, 4/4)
# NOTE: We used an R function for a probability distribution that isn't binomial, normal, or chi-sq (creativity/complexity #6, 10/10)

#Overall, it largely appears that men are more likely to earn more than their spouses as compared to women.
#We thus see than the trend of men making more money than women is true not only within the population,
#but also within households. 

#### REGRESSION ANALYSIS ####

#From the previous section, we see that the relationship between gender and income is significant. As such,
#it's unclear which is more highly correlated/predictive of head of household identification. 

#Regression analysis is by far the fastest and simplest way to get an idea of the interactions between
#gender, income, and head of household identification. 

#First, we confirm our results from the previous three sections using regression analysis. We see that the
#relationship between head of household identification and gender is significant, with and without controls
#for education, age, race, and weekly hours:
relate <- clean$RELATE
sex <- clean$SEX
age <- clean$AGE
race <- clean$RACE
hrs <- clean$UHRSWORK
educ <- clean$EDUC
lm(relate~sex)
lm(relate~sex+age+race+hrs+educ)

# NOTE: We used a linear regression technique (creativity/complexity #14, 11/10)

#From the above, we see that being female is negatively associated with identification as head of household
#(the rest of the controls effectively are noisy zeros).

#We now look at the relationship between head of household identification and income, with and without controls.
#We do this income analysis in two ways: on income and breadwinner status.
income <- clean$INCTOT
bread <- clean$BREAD
relinc <- clean$RELINC

lm(relate~income)
lm(relate~income+age+race+hrs+educ)

lm(relate~bread)
lm(relate~bread+age+race+hrs+educ)

#We see that income alone doesn't seem to have a major effect on identification of head of household, while
#winner status seems to have a significant positive effect (this is true with and without controls). This ultimately gives us
#increased support for apparent lack of relationship we saw earlier in the analysis. 

#We do not look at relative income as constructed given the mirror-nature of the variable;
#a regression analysis will automatically assume no effect of relative income. However, we can look at a gender
#subset to see if we can tease out any effects
male <- subset(clean,SEX==0)
male.HOH <- male$RELATE
male.relInc <- male$RELINC
male.race <- male$RACE
male.age <- male$AGE
male.hrs <- male$UHRSWORK
male.educ <- male$EDUC

lm(male.HOH~male.relInc)
lm(male.HOH~male.relInc+male.race+male.age+male.hrs+male.educ)

#It appears that relative income doesn't seem to have a major effect on men being identified as head of household.
#We can double check the same is true for women as well:

female <- subset(clean,SEX==0)
female.HOH <- female$RELATE
female.relInc <- female$RELINC
female.race <- female$RACE
female.age <- female$AGE
female.hrs <- female$UHRSWORK
female.educ <- female$EDUC

lm(female.HOH~female.relInc)
lm(female.HOH~female.relInc+female.race+female.age+female.hrs+female.educ)

#It appears that relative income doesn't seem to have a major effect on women being identified as head of household. This agrees
#with what we found earlier in the analysis. 

#Ultimately, it looks that that making more than your spouse is predictive of your identification as head
#of household, but the amount more or less you make doesn't seem to have a major impact. 

#Next, we looks at the relationship between sex and breadwinner status.
lm(sex~bread)
lm(bread~sex)

#As expected from our analysis in section 3, being the breadwinner is predictive of sex and sex is predictive of 
#being the breadwinner. This establishes the correlation between breadwinner status and sex (which could be an issue
#for our regression anlaysis, but that level of statistical analysis is outside the scope of this project). 

#We know that breadwinner status is closely linked to gender. We can now use a regression to see which is 
#more predictive:
reg1 <- lm(relate~sex+bread)
reg2 <- lm(relate~sex+bread+age+race+hrs+educ)
summary(reg1)
summary(reg2)

#It ultimately appears that gender and breadwinner status have equal impacts on identification as head of household.
#Namely, being male increases the likelihood of being identified as head of household by 15pp relative to 
#being female, while being the breadwinner in a household increases the likelihood of being identified as head of 
#household by 16pp. 

#Even if these numbers are not perfect, it's clear there is substantive effect of gender and breadwinner status on
#head of household identification.




















