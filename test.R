#import package and data
#Leo test
install.packages("dplyr")
library(readr)
library(ggplot2)
library(dplyr)
movie <- read_csv("movie.csv")

#convert factor into numeric
movie$Adjusted_Gross2 <- as.numeric(gsub(",", "", as.character(movie$Adjusted_Gross)))
class(movie$Adjusted_Gross2)

#Q1 Does higher budget movie generate higher revenue (use adjusted gross revenue)? 
#What’s the relationship between budget and adjusted gross revenues? 
#Add a trend line to your visualization to tell a better story.
ggplot(data=movie, aes(x=Budget, y=Adjusted_Gross2)) + geom_point() + geom_smooth() + ylim(c(200,1000))+labs(title="Budget vs Gross Revenue", x="Budget", y="Adjust Gross Revenue") 


#Q2 What genre makes highest revenue? Recommend top 5 best-selling genre by adjusted gross revenue.
genre_rev<-aggregate(movie$Adjusted_Gross2, by=list(Genre=movie$Genre), FUN=mean)
genre_rev <- transform(genre_rev, 
                       Genre = reorder(Genre, x))
ggplot(data=genre_rev, aes(x=Genre, y=x)) +
  geom_bar(stat="identity") +
  guides(fill=FALSE) + labs(title="Average Adjusted Revenue by Genre",y="Average Revenue in Millons")+
  theme(axis.text.x=element_text(angle = -30, hjust = 0))

#Q3 
filter1<-movie$Genre %in% c("action","adventure","animation","comedy","drama")
movie2<-movie[filter1,]
ggplot(data=movie2, aes(Adjusted_Gross2))+ geom_histogram(bins=41,aes(fill=Genre), colour="black") + facet_grid(Genre~.,)+xlim(c(200,1000))+labs(title="Movies' Gross Revenue by Genre",x="Adjusted Gross Revenue in millons")

names(movie)
str(movie$Profit)
movie$Profit_num<-as.numeric(as.character(movie$Profit))
is.na(movie$Profit_num)
movie[!complete.cases(movie),]

?qplot
qplot(data=movie,x=sqrt(Profit_num),fill=I("blue"),color=I("black"),binwidth=1)
qplot(data=movie,x=log10(Profit_num),geom="freqpoly")
qplot(data=subset(movie,movie$Day_of_Week %in% c("Wednesday","Thursday")),x=log10(Profit_num),geom="freqpoly",color=Day_of_week)

qplot(data=movie,x=Runtime_min,y=Profit_num)
ggplot(data = movie,aes(x=Runtime_min,y=Profit_num))+geom_point(alpha=1/3)+geom_smooth(method="lm")
cor(movie$Runtime_min,movie$Profit_num,method="pearson")

ggplot(data = movie, aes(Day_of_Week, Genre)) + geom_tile((fill = Profit_num), 
                                                          color = "white", scale_fill_gradient(low = "white", high = "blue"))
