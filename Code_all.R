
library(xml2) 
library(rvest) 

news <- data.frame(abstract = NA,
                   label = NA,
                   date = NA)  

#scrape WSJ news in 2015-2017 
for (year in 2015:2017){
  for (month in 1:12){
    if (month == 1|month == 3|month == 5| month == 7|month == 8|month == 10|month == 12){
      for (date in 1:31){  
        url <- paste0("http://www.wsj.com/public/page/archive-",year,"-",month,"-",date,".html")
        tal <- read_html(url) #store in an efficient format  
        
        #Get abstract
        abstract <- tal %>% 
          html_nodes("p") %>% 
          html_text() %>%  
          strsplit(split = "\n") %>%  
          unlist() 
        
        abstract <- gsub("[[:punct:]]","",abstract)
        abstract <- gsub("[[:digit:]]","",abstract)
        abstract <- abstract[155:length(abstract)]
        abstract <- tolower(abstract)
        abstract_df <- as.data.frame(abstract) 
        abstract_df$label <- NA
        
        for (i in 1:nrow(abstract_df)){
          if (grepl("[:alnum:]", abstract_df[i,1]) == FALSE){
            abstract_df[i,2] <- 0
          } else{
            abstract_df[i,2] <- 1
          }
        }
        
        abstract_df <- abstract_df[abstract_df$label == 1 ,]  
        abstract_df$date <- paste0(year,"-",month,"-",date) 
        news <- rbind(news, abstract_df) 
      }
    } else if (month == 4|month == 6|month == 9|month == 11){
      for (date in 1:30){ 
        url <- paste0("http://www.wsj.com/public/page/archive-",year,"-",month,"-",date,".html")
        tal <- read_html(url) #store in an efficient format 
        
        #Get abstract
        abstract <- tal %>%
          html_nodes("p") %>%
          html_text() %>% 
          strsplit(split = "\n") %>% 
          unlist() 
        
        abstract <- gsub("[[:punct:]]","",abstract)
        abstract <- gsub("[[:digit:]]","",abstract)
        abstract <- abstract[155:length(abstract)]
        abstract <- tolower(abstract)
        abstract_df <- as.data.frame(abstract) 
        abstract_df$label <- NA
        
        for (i in 1:nrow(abstract_df)){
          if (grepl("[:alnum:]", abstract_df[i,1]) == FALSE){
            abstract_df[i,2] <- 0
          } else{
            abstract_df[i,2] <- 1
          }
        }
        
        abstract_df <- abstract_df[abstract_df$label == 1 ,]  
        abstract_df$date <- paste0(year,"-",month,"-",date) 
        
        news <- rbind(news, abstract_df) 
      }
    }else{
      for (date in 1:28){
        url <- paste0("http://www.wsj.com/public/page/archive-",year,"-",month,"-",date,".html")
        tal <- read_html(url) #store in an efficient format 
        
        #Get abstract
        abstract <- tal %>%
          html_nodes("p") %>%
          html_text() %>% 
          strsplit(split = "\n") %>% 
          unlist() 
        
        abstract <- gsub("[[:punct:]]","",abstract)
        abstract <- gsub("[[:digit:]]","",abstract)
        abstract <- abstract[155:length(abstract)]
        abstract <- tolower(abstract)
        abstract_df <- as.data.frame(abstract) 
        abstract_df$label <- NA
        
        for (i in 1:nrow(abstract_df)){
          if (grepl("[:alnum:]", abstract_df[i,1]) == FALSE){
            abstract_df[i,2] <- 0
          } else{
            abstract_df[i,2] <- 1
          }
        }
        abstract_df <- abstract_df[abstract_df$label == 1 ,]  
        abstract_df$date <- paste0(year,"-",month,"-",date) 
        news <- rbind(news, abstract_df) 
      }
    }
  }
}

news<- news[-1,] 
news <- news[,-2] 



#####################################
# News Headline Sentiment Analysis  #
#####################################

library(tidytext) #for easy text manipulation
library(SnowballC) #for stemming words
library(NLP)  
library(tm) 
library(dplyr) 

# Get a sentiment dictionary 
dict <- get_sentiments("bing") 

# Parse your text
#create date formate 
toks <- news %>% 
  unnest_tokens(word, abstract)  %>% 
  mutate(word = wordStem(word)) 

toks$date <- as.Date(toks$date,"%Y-%m-%d") 

#Remove stopwords and blank tokens
data(stop_words)  
toks <- toks %>% 
  anti_join(stop_words) 

#Get Average Sentiment
toks <- toks %>% inner_join(dict) 

senti_character <- as.character(toks$sentiment)
senti_character <- gsub("negative","0", senti_character)  
senti_character <- gsub("positive","1", senti_character)  
toks$senti <- as.numeric(senti_character) 

senti_bydate <- aggregate(toks$senti, by = list(date = toks$date), FUN = mean) 
colnames(senti_bydate)[which(names(senti_bydate) == "x")] <- "avg.senti"


####################
# Load stock data  #
####################

#Set Working Directory to Locate the 30 Companies' Data
setwd("/Users/sihan/GoogleDrive/DATA_SCI/Final Project/Dow Jones") 

#Get the File List
files <- list.files(path = "/Users/sihan/GoogleDrive/DATA_SCI/Final Project/Dow Jones", recursive = TRUE,
                    pattern = "\\.csv$", full.names = FALSE) 

# Loop the data frame consisting all 30 companies' data
data2=lapply(files, read.csv, header=TRUE, sep=",")
data_rbind <- do.call("rbind", data2) 

#Change the data type of Date as Date
data_rbind$date <- as.Date(data_rbind$date, format = "%Y-%m-%d")

#Get a new column of Year & Month so that we can subset the data for Year 2017
data_rbind$Year <- as.numeric(format(data_rbind$date, "%Y"))
data_rbind$Month <- as.numeric(format(data_rbind$date, "%m"))

#Generate a new data frame of year 2015-2017
data_3yrs <- subset(data_rbind, data_rbind$Year == 2017 | data_rbind$Year == 2016 | data_rbind$Year == 2015 ) 


#Create a new column of the status of price change, using open price
data_3yrs$ChangeOpening <- NA
data_3yrs$ChangeClose <- NA
data_3yrs$pctopen <- NA
data_3yrs$pctclose <- NA


firm_total <- data.frame(date = as.Date(NA),
                         open = NA,
                         close = NA,
                         name = NA,
                         ChangeOpening = NA,
                         ChangeClose = NA,
                         pctopen = NA,
                         pctclose = NA)  


#calculate opening price change 
firmlist <- unique(data_3yrs$Name) 

for (i in firmlist){ 
  firm <- data_3yrs[data_3yrs$Name == i, c("date","open","close","ChangeOpening","ChangeClose","pctopen","pctclose")] 
  
  #open price change 
  for (j in 2:nrow(firm)){ 
    if (firm$open[j] > firm$open[j-1]){
      firm$ChangeOpening[j-1] <- "increase" 
    } else if (firm$open[j] <= firm$open[j-1]){
      firm$ChangeOpening[j-1] <- "decrease"
    }
  }
  
  #close price change 
  for (j in 2:nrow(firm)){ 
    if (firm$close[j] > firm$close[(j-1)]){
      firm$ChangeClose[j] <- "increase"
    } else if (firm$close[j] <= firm$close[j-1]){
      firm$ChangeClose[j] <- "decrease"
    }
  } 
  
  #percentage change 
  for (j in 2:nrow(firm)){ 
    firm$pctopen[j-1] <- (firm$open[j] - firm$open[j-1])/firm$open[j-1] *100
    firm$pctclose[j] <- (firm$close[j] - firm$close[j-1])/firm$close[j-1] *100
  }
  
  firm$name <- i 
  firm_total <- rbind(firm_total, firm) 
} 

firm_total <- firm_total[-1,] 




#################################################
# Merge news sentiment with stock price by date #
#################################################

sentiment_stock <- merge(senti_bydate, firm_total, by = "date", all = TRUE) 
sentiment_stock <- na.omit(sentiment_stock)
sentiment_stock$month <- as.factor(format(sentiment_stock$date, "%m"))

sentiment_stock$ChangeOpening <- as.factor(sentiment_stock$ChangeOpening)
sentiment_stock$ChangeClose <- as.factor(sentiment_stock$ChangeClose)

sentiment_stock$quarter <- ceiling(as.numeric(sentiment_stock$month) / 3) 
sentiment_stock$year <- as.numeric(format(sentiment_stock$date, "%Y"))


#merge with GDP
setwd("/Users/sihan/GoogleDrive/DATA_SCI/Final Project") 

library(readxl)
gdp <- read_excel("gdp.xlsx") 
gdp$quarter <- gsub("Q","", gdp$quarter)
gdp <- gdp[,-3] 
gdp$quarter <- as.numeric(gdp$quarter) 
sentiment_stock <- merge(sentiment_stock, gdp, by = c("year","quarter")) 

#Interest rate
interest <- read_excel("interest.xlsx") 
colnames(interest)[which(names(interest) == "rate")] <- "interest.rate"
interest$year <- format(interest$date,"%Y") 
interest$year <- as.numeric(interest$year)
interest <- interest[,-1]
sentiment_stock <- merge(sentiment_stock, interest, by = "year") 

#NASDQ
NASDQ <- read_excel("NASDQ.xlsx") 
colnames(NASDQ)[which(names(NASDQ) == "Date")] <- "date"
colnames(NASDQ)[which(names(NASDQ) == "Open")] <- "open_nasdq"
colnames(NASDQ)[which(names(NASDQ) == "Close")] <- "close_nasdq"
NASDQ <- NASDQ[,c("date", "open_nasdq","close_nasdq")]
NASDQ$date <- as.Date(NASDQ$date)
sentiment_stock <- merge(sentiment_stock, NASDQ, by = "date") 


#unemp
unemp <- read_excel("unemployment.xlsx") 
unemp$date <- as.Date(unemp$date)
sentiment_stock <- merge(sentiment_stock, unemp, by = "date") 


#assets
asset <- read_excel("asset.xlsx") 
asset$name <- toupper(asset$name)
sentiment_stock <- merge(sentiment_stock, asset, by = c("year","quarter","name")) 


###################
##SUPPORT VECTORS##
###################  
#Load rpart libraries
library(e1071) 

# only keep sentiment > 0.65 or < 0.35
sentiment_stock_short <- sentiment_stock[(sentiment_stock$avg.senti > 0.65 | sentiment_stock$avg.senti < 0.35), ]

train <- sentiment_stock[sentiment_stock$year == 2015 | sentiment_stock$year == 2016, ]
test <- sentiment_stock[sentiment_stock$year == 2017,] 


# Function that creates k folds 
kfolds <- function(k){ 
  #
  #Desc: Assign each record to one of the k-folds 
  # 
  #Args:
  #  k = number of folds   
  # 
  #Returns:
  #  a vector of the partition assignments 
  #
  train$vec <- runif(nrow(train)) # ramdonly assign each observation with a random number 
  train$fold <-  cut(train$vec, breaks = k, label = FALSE) # randomly assign observations into k groups according to the random numbers
  return(train$fold) 
} 
train <- cbind(train, kfolds(6)) 
colnames(train)[grep("kfolds", names(train))] <- "kfold" 


#Load F1 Score
meanf1 <- function(actual, predicted){
  
  #Mean F1 score function
  #actual = a vector of actual labels
  #predicted = predicted labels
  
  classes <- unique(actual)
  results <- data.frame()
  for(k in classes){
    results <- rbind(results, 
                     data.frame(class.name = k,
                                weight = sum(actual == k)/length(actual),
                                precision = sum(predicted == k & actual == k)/sum(predicted == k), 
                                recall = sum(predicted == k & actual == k)/sum(actual == k)))
  }
  results$score <- results$weight * 2 * (results$precision * results$recall) / (results$precision + results$recall) 
  return(sum(results$score)) 
}


scores <- data.frame() 
for (i in unique(train$kfold)){
  #Fit SVM  under default assumptions -- cost = 1, gamma = 0.055
  #svm.rbf.fit <- svm(ChangeOpening ~ avg.senti + name + growth + interest.rate + unemployment + open_nasdq, 
  #                   data = train[train$folds != i, ], kernel = "radial", 
  #                   cost = 1, gamma = 0.05555) 
  
  #Tools to review output
  #print(svm.rbf.fit) 
  
  #Calibrate SVMs
  pred.test <- svm(ChangeOpening ~ avg.senti + name + growth + interest.rate + unemployment + open_nasdq + assets, 
                   data = train[train$kfold != i, ], kernel = "radial", cost = 1, gamma = 8) 
  print(pred.test)  
  
  #Predict
  pred.rbf <- predict(pred.test, train[train$kfold == i, ]) 
  scores <- rbind(scores, data.frame(model = "SVM", 
                                     actual = train[train$kfold == i, ]$ChangeOpening, 
                                     predicted = pred.rbf)) 
  #examine result
  table(pred.rbf) 
}

meanf1(scores$actual, scores$predicted) 



#Close price 
scores.close <- data.frame() 
for (i in unique(train$kfold)){
  #Fit SVM  under default assumptions -- cost = 1, gamma = 0.055
  #svm.rbf.fit <- svm(ChangeOpening ~ avg.senti + name + growth + interest.rate + unemployment + open_nasdq, 
  #                   data = train[train$folds != i, ], kernel = "radial", 
  #                   cost = 1, gamma = 0.05555) 
  
  #Tools to review output
  #print(svm.rbf.fit) 
  
  #Calibrate SVMs
  pred.test.close <- svm(ChangeClose ~ avg.senti + name + growth + interest.rate + unemployment + close_nasdq + assets, 
                         data = train[train$kfold != i, ], kernel = "radial", cost = 1, gamma = 8) 
  print(pred.test.close)  
  
  #Predict
  pred.rbf.close <- predict(pred.test.close, train[train$kfold == i, ]) 
  scores.close <- rbind(scores.close, data.frame(model = "SVM", 
                                                 actual = train[train$kfold == i, ]$ChangeClose, 
                                                 predicted = pred.rbf.close)) 
  #examine result
  table(pred.rbf.close) 
}

meanf1(scores.close$actual, scores.close$predicted) 



#test 2017 data 
pred.test.close.2017 <- svm(ChangeClose ~ avg.senti + name + growth + interest.rate + unemployment + close_nasdq + assets, 
                            data = train, kernel = "radial", cost = 1, gamma = 8) 

print(pred.test.close.2017) 

pred.rbf.close.2017 <- predict(pred.test.close.2017, test, type = "class") 
test$predicted <- pred.rbf.close.2017

meanf1(test$ChangeClose, test$predicted) 





