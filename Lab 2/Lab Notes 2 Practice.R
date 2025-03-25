#5.3 Practice from Lab Notes 2

data <- ("./Framingham.Rdata")
load(data)
fram <- stata_data

#(i) Create two tables of CHD at follow-up (one with frequency and one with relative frequency)

xtabs(~ chdfate, data= fram, addNA = TRUE)
prop.table(table(fram$sex, fram$chdfate, useNA='ifany'))

#(ii) Create a bar chart of CHD outcomes using base R

freq_table <- xtabs(~ chdfate, data= fram, addNA = TRUE)
barplot(freq_table, main='CHD Outcomes', xlab='', las=2, cex.names = 0.6)

#(iii) Create a bar chart of CHD outcomes using ggplot2

ggplot(data = fram, aes(x = chdfate)) +
  geom_bar()

#(iv) Add the following option to yoru ggplot2 code: + facet_wrap(~factor(sex)) What does this do?
ggplot(data = fram, aes(x = chdfate)) +
  geom_bar() + facet_wrap(~factor(sex))

## Divides the results into different plots by sex

#6.6 Practice from Lab Notes 2

#(i) Create a numerical summary of body mass index 'bmi'
summary(fram$bmi)

#(ii) Create a histogram of bmi using Base R or ggplot2
ggplot(fram, aes(x=bmi)) + geom_histogram(binwidth = 1)

#(iii) Createa a density blot of bmi using base R or ggplot2
ggplot(fram, aes(x = bmi)) + geom_density()

#(iv) Create a boxplot of bmi using base R or ggplot2
ggplot(fram, aes(y=bmi)) + geom_boxplot()

#(v)Create a histogram overlaid with a density curve for bmi with ggplot2.
#see if you can use the same ggplot 'logic' from above to stratify this plot by sex
ggplot(fram, aes(x=bmi)) +
  geom_histogram(aes(y = ..density..) ,
                 binwidth = 2, colour = 'black', fill='white') +
  geom_density(alpha = .2, fill='#FF6666')

#7.3 Practice from Lab Notes 2

#(i) For each variable with any missing values, create a table displaying how many missing values there are.
fram_missing <- fram[!complete.cases(fram),]
table(fram_missing$scl, fram_missing$bmi)

#(ii)How many complete observations are there in the Framingham data set?
fram_complete <- fram[complete.cases(fram),]
nrow(fram_complete)

#(iii) Create a categorical variable called high.chol that is 1 when the serum cholesterol levels are greater than or equal to 200
# and is 0 otherwise. Create a bar chart of this new variable.
fram$high.col <- if_else(fram$scl >=200, 1, 0)
ggplot(fram, aes(x=high.col)) + geom_bar()

#(iv)Create a new variable for log-transformed serum cholesterol called log.scl.
# Create a boxplot for this new variable.
fram$log.scl <- log(fram$scl)
ggplot(fram, aes(y=log.scl)) + geom_boxplot()

#8.2 Practice from Lab Notes 2

#(i)Create a scatterplot of BMI and age. Describe the relationship you see
ggplot(fram, aes(x=age, y=bmi)) + geom_point(pch=8, color='green')

#(ii)Adapt the plot you made above by coloring the points of the scatterplot based on sex.
# Describe the relationship you see.
ggplot(fram, aes(x=age, y=bmi)) + geom_point(aes(color=factor(sex)))
