

#Question 1
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
data1 <- read.csv(url(fileLocation))
head(data1)
dim(data1)
?cut

#Question 2

data1$ageGroup  <- cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
head(data1)
class(data1$ageGroup)
levels(data1$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(data1)

# Question : 3  

ImpSub <- subset(data1, Impressions>0)
dim(ImpSub)
summary(ImpSub)

# Question 4 

ImpSub$CTR <- ImpSub$Clicks/ImpSub$Impressions

# Question 5 

head(ImpSub)
library(ggplot2) # used for visualizations

ggplot(ImpSub, aes(x=Impressions, fill=ageGroup))+   labs(title="Impressions by age group")+geom_histogram(binwidth=1)

ggplot(ImpSub, aes(x=CTR, fill=ageGroup))+   labs(title="CTR by age group")+geom_histogram(binwidth=0.1)

summary(ImpSub)
# Question 6 

summary(ImpSub)
ImpSub$CTR_Group <- cut(ImpSub$CTR, c(-Inf,0.2, 0.4,0.6, 0.8, Inf),right = FALSE)
head(ImpSub)
?cut
summary(ImpSub)
head(ImpSub)
class(ImpSub$CTR_Group)
head(ImpSub[ImpSub$CTR == 0.4,])

levels(ImpSub$CTR_Group) <- c("<0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8+")

head(ImpSub)

# Question 7
imp <- aggregate(ImpSub$Impressions, by=list(Category=ImpSub$Gender), FUN=sum)
imp[imp$Category==0,]
clicks <- aggregate(ImpSub$Clicks, by=list(Category=ImpSub$Gender), FUN=sum)
clicks[clicks$Category == 0,]
signin <- aggregate(ImpSub$Signed_In, by=list(Category=ImpSub$Gender), FUN=sum)
signin[signin$Category == 0,]

# Question 8 
mean(ImpSub$Age)

# Question 9 
aggregate(ImpSub$Impressions, by=list(Category=ImpSub$ageGroup), FUN=mean)
aggregate(ImpSub$Clicks, by=list(Category=ImpSub$ageGroup), FUN=mean)
aggregate(ImpSub$CTR, by=list(Category=ImpSub$ageGroup), FUN=mean)

# Question 10

CountAgeGrp.tab <- table(ImpSub$ageGroup,ImpSub$CTR_Group)
CountAgeGrp.tab

# Question 11

# Question 12 

