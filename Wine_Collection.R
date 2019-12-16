#Wine Collection Data
#Final Project -- Olga Vinogradova --
library(readxl)
MyBottles <- read_excel("UCLAx App Programming/R. Data Science/final project 
                        R/MyBottles.xlsx")
View(MyBottles)
summary(MyBottles)
head(MyBottles)

#Data Munging/ Transformation
MyWine <- MyBottles
names(MyWine)
# > names(MyWine)
# [1] "iInventory"   "Value"        "NativePrice"  "Size"         "iWine"       
# [6] "Type"         "Color"        "Category"     "Vintage"      "Wine"        
# [11] "Locale"       "Producer"     "Varietal"     "Country"      "Region"      
# [16] "StoreName"    "PurchaseDate" "Location"     "Bin"          "BeginConsume"
# [21] "EndConsume"   "CScore" 

#install.packages("dplyr")
library(dplyr)
wine_df <- select(MyWine, "Value", "NativePrice", "Size", "Color", "Vintage", "Country",
                  "CScore", "Wine")
names(wine_df) <- tolower(names(wine_df))
names(wine_df) <- c("value","price","size", "color", "vintage", "country", "score", "wine")
profit_net <- wine_df$value - wine_df$price
wine_df <- mutate(wine_df, profit_net)
wine_df$has_profit <- ifelse(wine_df$profit_net > 0, TRUE, FALSE)
wine_df <- mutate(wine_df, has_profit)
names(wine_df)
#[1] "value"      "price"      "size"       "color"      "vintage"    "country"   
#[7] "score"      "wine"       "profit_net" "has_profit"


#to remove NA's
summary(wine_df)
#only "score" column has NA's: NA's   :26 
df_no_NA <- wine_df
nrow(df_no_NA)
#[1] 865
df_no_NA_trimmed <- df_no_NA[complete.cases(df_no_NA$score),]
nrow(df_no_NA_trimmed)
#[1] 839

#to remove items with the purchase price 0 (inherited wine collection) and vintage 
#year of 1001 (it's not a year but it means it is selected from the finest «lots» 
#of each vintage and aged for 5 years) Thus this observation can be removed since it 
#might impact further calculations
df_clean <- filter(df_no_NA_trimmed, (df_no_NA_trimmed$price != 0) & (df_no_NA_trimmed$vintage != "1001"))
nrow(df_clean)
#[1] 777

#to normalize "size" column (char to numeric)
# size 
# <chr>  
# 750ml
# 750ml
# ...
#remove "ml":
size_num_ml <- strsplit(df_clean$size, "ml")

firstelement <- function(x){x[1]}
size_nums <- sapply(size_num_ml, firstelement)
#remove "L":
size_num_L <- strsplit(size_nums, "L")
size_num <- sapply(size_num_L, firstelement)
size_num <- as.numeric(size_num)

filter_function <- function(x){if (x[1] < 2 ){
  (x[1])*1000
}else {x[1]}
}

size_norm <- sapply(size_num, filter_function)
df_clean$size <- size_norm
head(df_clean$size)
#[1] 750 750 750 750 750 750


# to scale "size"
library(stats)
head(df_clean)
scale_size <- scale(df_clean$size)
#attr(,"scaled:center")
#[1] 718.4685
df_clean <- mutate(df_clean, scale_size)
summary(df_clean)
# scale_size.V1   
# Min.   :-2.119219  
# 1st Qu.: 0.194551  
# Median : 0.194551  
# Mean   : 0.000000  
# 3rd Qu.: 0.194551  
# Max.   : 4.822093

#to create score ranges
rating <- cut(df_clean$score,breaks=c(80, 88, 90, 92, 94, 100),
              labels = c("Bad", "OK", "Good", "Great", "Amazing"),
              include.lowest = TRUE, right = FALSE )
#Levels: [80,88) [88,90) [90,92) [92,94) [94,100]
#Levels:   Bad     OK     Good    Great  Amazing

df_clean <- mutate(df_clean, rating)

names(df_clean)
# > names(df_clean)
# [1] "value"      "price"      "size"       "color"      "vintage"    "country"   
# [7] "score"      "wine"       "profit_net" "has_profit" "scale_size" "rating"    

#filtering from dublicated values

dublicate_bottle <- duplicated(df_clean, fromLast = TRUE)

df_clean <- mutate(df_clean, dublicate_bottle)
df_clean <- filter(df_clean, df_clean$dublicate_bottle == FALSE )
nrow(df_clean)
#[1] 334

names(df_clean)
# [1] "value"            "price"            "size"             "color"           
# [5] "vintage"          "country"          "score"            "wine"            
# [9] "profit_net"       "has_profit"       "scale_size"       "rating"          
# [13] "dublicate_bottle"

wine_set <- select(df_clean, "rating","score", "price", "has_profit", "vintage",
                   "scale_size", "color", "country","wine")
wine_set <- arrange(wine_set, price, desc(score))
names(wine_set)
#[1] "rating"     "score"      "price"      "has_profit" "vintage"    "scale_size"
#[7] "color"      "country"    "wine"    

summary(wine_set)
# > summary(wine_set)
# rating        score           price         has_profit         vintage    
# Bad    :  2   Min.   :84.00   Min.   :  17.00   Mode :logical   Min.   :1850  
# OK     : 11   1st Qu.:91.56   1st Qu.:  45.00   FALSE:160       1st Qu.:2009  
# Good   :112   Median :92.38   Median :  60.00   TRUE :174       Median :2010  
# Great  :149   Mean   :92.48   Mean   :  82.67                   Mean   :2007  
# Amazing: 60   3rd Qu.:93.34   3rd Qu.:  87.25                   3rd Qu.:2012  
#               Max.   :97.00   Max.   :1000.00                   Max.   :2016

# scale_size.V1       color             country              wine          
# Min.   :-2.119219   Length:334         Length:334         Length:334        
# 1st Qu.: 0.194551   Class :character   Class :character   Class :character  
# Median : 0.194551   Mode  :character   Mode  :character   Mode  :character  
# Mean   : 0.213025                                                           
# 3rd Qu.: 0.194551                                                           
# Max.   : 4.822093   

#price vs rating
#the average bottle price by rating:
avg_price_rating <- sapply(split(wine_set$price, wine_set$rating), mean)
# > avg_price_rating
# Bad        OK        Good      Great    Amazing 
# 117.50000  60.90909  48.10813  80.47664 155.46717 

#"Bad" wine is very expensive; "Good" wine much cheaper than "ok" wine.
par(mfrow  =c(1,2))
plot(wine_set$price, y = wine_set$rating, pch = 10, col = "red", 
     main = "price vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating")
#there is one bottle for $200 that makes "bad" average so high. Same for the "good" range. 

plot(wine_set$price, y = wine_set$score, pch = 7, col = "blue", 
     main = "price vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating, points")

#some anomalies: expensive wine with low rating
#$200 - 84

#need to remove only one $1000 bottle from the set
subset(wine_set, wine_set$price == 1000)
# rating score price has_profit vintage scale_size[,1] color country    wine           
# Great   93   1000    TRUE      1924      0.195       White France  Château d'Yquem

wine_set <- subset(wine_set, price < 1000)

plot(wine_set$price, y = wine_set$score, pch = 7, col = "red", 
     main = "price vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating, points")

#you can see at least three observations are out of target ( low rating and expensive)

boxplot(wine_set$price,
        main = "Price",
        xlab = "Price, USD",
        
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
# Show price broken down by different variables:
boxplot(wine_set$price ~ as.factor(wine_set$vintage), col="blue")
boxplot(wine_set$price ~ as.factor(wine_set$rating), col="green")
boxplot(wine_set$price ~ as.factor(wine_set$color), col="red")
boxplot(wine_set$price ~ as.factor(wine_set$country), col="yellow")
boxplot(wine_set$price ~ as.factor(wine_set$has_profit), col="purple")

boxplot(wine_set$score ~ as.factor(wine_set$vintage), col="blue")
boxplot(wine_set$score ~ as.factor(wine_set$color), col="red")
boxplot(wine_set$score ~ as.factor(wine_set$country), col="yellow")
boxplot(wine_set$score ~ as.factor(wine_set$has_profit), col="purple")



par(mfrow  =c(1,1))

cor(wine_set$score, wine_set$price)
#1] 0.5372097
#not a strong correlation
cor(wine_set$score, wine_set$vintage)
#[1] 0.1661194
#low correlation

library(scatterplot3d)

scatterplot3d(wine_set$vintage, 
              wine_set$score,
              wine_set$price, 
              
              highlight.3d=TRUE, 
              col.axis="blue", 
              col.grid="lightblue",
              main="Wine Set", 
              pch=20, 
              xlab="Vintage, Year", 
              ylab="Score", 
              zlab="Price")

lm1 <- lm(wine_set$price ~ wine_set$score) #linear model fit
# lm(formula = wine_set$price ~ wine_set$score)
# 
# Coefficients:
#   (Intercept)  wine_set$score  
# -1798.51           20.31  


summary(lm1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -1798.515    162.130  -11.09   <2e-16 ***
#   wine_set$score    20.312      1.753   11.59   <2e-16 ***

#Std Error is very high  - confirms that there is no good strong correlation 
#between price and quality

plot(lm1)
abline(c(0,0),col="red",lwd=3)

#Residuals vs Fitted:
#it has linear pattern - not good 
#scale-location:

#Normal Q-Q
#residuals don't follow a straight line well the more expensive wine is.


#Histogram: counts the number of observations in each interval
hist <- hist(wine_set$price, freq=TRUE, col="blue")  

pairs(set_60[,c(2,3,5)])   
cor(set_60[,c(2,3,5)], method="pearson")  

lm1_60 <- lm(set_60$price ~ set_60$score) #linear model fit
summary(lm1_60)
plot(lm1_60)

coef(lm1_60)[1]+coef(lm1_60)[2]*94
# > coef(lm1_60)[1]+coef(lm1_60)[2]*94
# (Intercept) 
# 100.6614 
#predicted price associated with a score of 94 is 100.66 USD

#not an accurate prediction

---------------------------------------------------------------
#dividing data frame into 3 price categories
#(under $25, between $25 and 60, between $60 and 200):
set_25 <- subset(wine_set, wine_set$price<25 & wine_set$score >= 90)
set_25_60 <- subset(wine_set, wine_set$price>=25 & wine_set$price < 60 & wine_set$score >= 92)
set_60 <- subset(wine_set, wine_set$price >= 60 & wine_set$price <= 200 & wine_set$score >= 94)


# purchase_list_25 <- set_25[1:10,]
# purchase_list_25_60 <- set_25_60[1:10,]
# purchase_list_60 <- set_60[1:10,]

#Need to create a list of wines from these 3 sets that are worth to purchase

plot(set_25$price, y = set_25$score, pch = 7, col = "blue", 
     main = "price<25 vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating, points")

plot(set_25_60$price, y = set_25_60$score, pch = 7, col = "red", 
     main = "25<price>60 vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating, points")

plot(set_60$price, y = set_60$score, pch = 7, col = "red", 
     main = "price>60 vs rating",
     xlab = "price per bottle, $",
     ylab = "Rating, points")

library(Hmisc) 
# Cut a numeric variable into intervals for each price category:
#<$25
priceGroups1 <- cut2(set_25$price,g=3)    # 3 cut points
levels(priceGroups1)
#> levels(priceGroups1)
#[1] "[17,20)" "20"      "24"  
scoreGroups1 <- cut2(set_25$score,g=3)    # 3 cut points
levels(scoreGroups1)
#[1] "[90.3,91.2)" "[91.2,92.5)" "[92.5,93.0]"

#>$25-$60
priceGroups2 <- cut2(set_25_60$price,g=3)    # 3 cut points
levels(priceGroups2)
#[1] "[25,48)" "[48,52)" "[52,58]"
scoreGroups2 <- cut2(set_25_60$score,g=3)    # 3 cut points
levels(scoreGroups2)
#> levels(scoreGroups2)
#[1] "[92.0,92.3)" "[92.3,92.6)" "[92.6,95.0]"

#>$60
priceGroups3 <- cut2(set_60$price,g=3)    # 3 cut points
levels(priceGroups3)
#[1] "[ 60,107)" "[107,154)" "[154,200]"
scoreGroups3 <- cut2(set_60$score,g=3)    # 3 cut points
levels(scoreGroups3)
#> levels(scoreGroups3)
#[1] "[94.0,94.5)" "[94.5,95.0)" "[95.0,96.3]"

# Use different color for each of the 3 price groups
plot(set_60$price,set_60$score,pch=19,col=priceGroups3,cex=0.5)





