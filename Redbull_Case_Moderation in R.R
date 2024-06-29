#Question
#Q: The ezine group is responsible for finding high impact ezines and placing strategic ads there for redbull. Consider the effect of ezines campaign on redbulls sales. suppose a manager told you that the west coast ezine group was surfing at the beach everyday and was putting up junk ads. Run an interaction with ezine and west. Is the interaction significant. Is the west coast spending impacting ezine. 

#Q. your CEO believes that your higher level of ads spent on TV causes the ezine ads to not perform. is he correct? Support with evidence.
#based on past marketing mix decide the marketing mix
head(Redbull)
sum(is.na(Redbull))

#Multiple plots
library(psych)
pairs.panels(Redbull[1:8])

#cREATE dichotmous variables fror regions
Redbull$West <- (Redbull$region=="W")
Redbull$North <- (Redbull$region=="N")
Redbull$South<- (Redbull$region=="S")
Redbull$East <- (Redbull$region=="E")
#MODERATION
#Step1
#It is for non West
West_Result <- lm(sales~ezine+West +(ezine*West),Redbull)
summary(West_Result)

#STEP 2 FOLLOW UP ANALYSIS
#it is for West
#Spotlight analysis
Redbull$Non_West <-(Redbull$West==FALSE)
Non_West_Result <- lm(sales~ezine+Non_West +(ezine*Non_West),Redbull)
summary(Non_West_Result)

#The manager was wrong, as in the West Coast, the ezine ads are performing well, and sales are 
#higher than in non-West Coast regions (North, South, and East), indicating that they are not putting 
#up junk ads.
#The interaction between Non-West Coast and ezine is significant at p value 0.1. Within Non-West 
#Coast (North, South, and East), for every dollar spent on ezine, the sale is increasing by 82826 
#dollars. And the interaction between ezine and the West Coast is significant at a p value of 0.1. 
#Within the West Coast, for every dollar spent on ezine, the sales of Red Bull increase by 290,100 
#dollars.

#Q2

Redbull$centertv <- (Redbull$tv-mean(Redbull$tv))

Center_TV_Result <- lm(sales~ezine+centertv +(ezine*centertv),Redbull)
summary(Center_TV_Result)


Redbull$high_spend_on_tv <-(Redbull$centertv-sd(Redbull$centertv))

ResultHigh_spend_tv <- lm(sales~ezine+high_spend_on_tv +(ezine*high_spend_on_tv),Redbull)
summary(ResultHigh_spend_tv)


Redbull$Low_spend_tv <-(Redbull$centertv+sd(Redbull$centertv))
Result_Low_spend_on_TV <- lm(sales~ezine+Low_spend_tv +(ezine*Low_spend_tv),Redbull)
summary(Result_Low_spend_on_TV)
#



head(Redbull)

Redbull$west <- (Redbull$region=="W")
Redbull$north <- (Redbull$region=="N")
Redbull$south<- (Redbull$region=="S")
Redbull$east <- (Redbull$region=="E")
#MODERATION
#Step1
Result_West <- lm(sales~ezine+west +(ezine*west),Redbull)
summary(Result_West)

#STEP 2 FOLLOW UP ANALYSIS

Redbull$nonwest <-(Redbull$west==FALSE)
Result_NonWest <- lm(sales~ezine+nonwest +(ezine*nonwest),Redbull)
summary(result_nonWest)


#Q2

Redbull$ctv <- (Redbull$tv-mean(Redbull$tv))

resultCtv <- lm(sales~ezine+ctv +(ezine*ctv),Redbull)
summary(resultCtv)


Redbull$htv <-(Redbull$ctv-sd(Redbull$ctv))

resultHtv <- lm(sales~ezine+htv +(ezine*htv),Redbull)
summary(resultHtv)


Redbull$Ltv <-(Redbull$ctv+sd(Redbull$ctv))
resultLtv <- lm(sales~ezine+Ltv +(ezine*Ltv),Redbull)
summary(resultLtv)
#
#There is multicollinearity between ezine and TV, as the correlation between ezine and TV is 0.97, 
#which is greater than 0.8. So, due to multicollinearity, the significant variable becomes 
#insignificant. So we did moderation to check how much one variable is affecting the other variable 
#at different levels in our case center: high and low spending.
#The boss was correct about the impact of TV ads on ezine and high spending on TV ads causing 
#ezine ads to perform differently.
#As when we check spotlighting on three levels (center, high, and low), it shows that:
#1. On average spending on TV ads, for every one dollar spent on ezine, Red Bull sales 
#increase by 95120 dollars
#2. When TV ads expenditure is High, then for every one dollar spent on ezine, Red Bull sales 
#increase by $4780 dollars.
#3. When TV ads expenditure is Low, then for every one dollar spent on ezine, Red Bull sales 
#increase by $142400 dollars.
#So, it is evident that due to high spending on TV ads, the effectiveness of ezine ads decreases, as 
#the increase in Red Bull sales per dollar spent on ezine is lower ($4,780) compared to when TV 
#ads expenditure is low ($142,400)

