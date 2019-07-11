library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(mice)
library(tidyr)
library(mice)
library(tidytext)

#Datacleaning OK Cupid----First Import and replace blanks with "NA" and "-1" in Income category with "NA"
Data<-read.csv('profiles.csv', na.strings =c("","-1"))

#remove all essays but essay 5 "The six things I could never do without"
Data$essay0<-NULL
Data$essay1<-NULL
Data$essay2<-NULL
Data$essay3<-NULL
Data$essay4<-NULL
Data$essay6<-NULL
Data$essay7<-NULL
Data$essay8<-NULL
Data$essay9<-NULL
#remove variable "last online" information is NA to analysis
Data$last_online<-NULL

#remove rows with more than 7 (50% of optional responses) missing values using manual function 'delete.na'. 
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

#59K before 
Data<-delete.na(Data, 7)
#56K Observations remaining 

#determine amount of missing values for each category 

sort(sapply(Data, function(x) { 
  sum(is.na(x)) }), decreasing=TRUE)

#variables with missing values greater than 30% of observations are removed from analysis. 
#This includes "income", "offspring", and "Diet

Data$income<-NULL
Data$offspring<-NULL
Data$diet<-NULL

#18 variables remain

#cleaning data / formatting 
#replace "&rsquo;" in sign" category with "'"

Data$sign<-str_replace_all(Data$sign,"&rsquo;","'")

#replace "<<br/>" in essay 5 with " " 
Data$essay5<-str_replace_all(Data$essay5,"<br />"," ")


#Make a Backup of Data before moving on with analysis 
Data_Backup<-Data

#review data structure
str(Data)

#update height to numeric category..Factor Variable Trap, must convert to character first 
Data$height<-as.numeric(as.character(Data$height))

#view height and age summarys
summary(Data$height)
summary(Data$age)
#identify and remove outliers for height and age 
sum(Data$height<50)
sum(Data$age>65)

#remove rows with outliers for height and age. 


#remove rows with missing education data for analysis. 
Data<-Data[!is.na(Data$education),]
#left with 51K observations 

#imputate missing values for Drinks, Drugs, and Smoking
imp_data<-mice(Data[,4:5])

#run MICE for missing data for Drinks, Drugs, and Smoking


#which words do men and women tend to use?
#for this section, I learned that %>% keeps things neater 
library(tidytext)
men.only <-
  Data %>%
  filter(sex == 'm')
women.only <-
  Data %>%
  filter(sex == 'f')

#tolower() changes all the letters to lowercase so ThiS and this are counted as one word
#strsplit() splits the string into individual strings be
#table() counts the frequency of each word
men.word.count <-
  data.frame(table(unlist(strsplit(tolower(men.only$essay5), " "))))
women.word.count <-
  data.frame(table(unlist(strsplit(tolower(women.only$essay5), " "))))

men.word.count %>%
  arrange(-Freq)
top_n(30)

women.word.count %>%
  arrange(-Freq) 
top_n(30)

men.word.count <-data.frame(table(unlist(strsplit(tolower(men.only$essay5), " "))))
#Renamed Var1 to Word because it was easier to understand structure using "Word"
colnames(men.word.count)[1] <- 'Word'
#gsub is a part of the stringr package. In this case, it's used to replace special characters in the set of strings. 
#[\n] is a line break, while [-], [[:punct:]], [0-9] are other special characters like punctuation and numerical digits. 
men.word.count$Word <- gsub("[\n]", "", men.word.count$Word)
men.word.count$Word <- gsub("-", "", men.word.count$Word)
men.word.count$Word <- gsub("[[:punct:]]", "", men.word.count$Word)
men.word.count$Word <- gsub("[0-9]", "", men.word.count$Word)
men.word.count <- men.word.count[men.word.count$Word != '', ]

men.word.table<-men.word.count %>%
  #The stop_words dataset in the tidytext package contains stop words (words to be filtered out, i.e prepositions and articles) from three lexicons. 
  #filter() to use only one set of stop words
  filter(!(Word %in% stop_words$word)) %>%
  arrange(-Freq) %>%
  top_n(5000)

ggplot(men.word.table, aes(x = Word, y = Freq))+geom_bar(stat = "identity")



  
  