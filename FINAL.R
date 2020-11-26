
library(dplyr)


library(ggplot2)
library(stringr)
library(RColorBrewer)

#Preparing and cleaning data
airbnb <- read.csv('airbnb_listings.csv', sep = ';', comment.char = '#', stringsAsFactors = FALSE)
columns = c('ID', 'Host.Name','Host.Since','Host.Acceptance.Rate','Host.Listings.Count','City','State','Market','Country.Code','Country','Latitude','Longitude','Property.Type',
            'Room.Type','Accommodates','Bathrooms','Bedrooms','Amenities','Price','Weekly.Price','Monthly.Price','Security.Deposit',
            'Cleaning.Fee','Number.of.Reviews','Extra.People','Guests.Included','Host.Verifications','Property.Type','Cancellation.Policy','Review.Scores.Rating',
            'Review.Scores.Accuracy','Review.Scores.Checkin','Review.Scores.Cleanliness','Review.Scores.Communication','Review.Scores.Location','Review.Scores.Value',
            'Reviews.per.Month','Features')

airbnb <- airbnb %>% select(columns)

#create a function to remove the special charachters from a string
remove_special_charachters <- function(x) {
  return(gsub("[^-0-9/.]+", "", x))
}

# first we will get rid of records with false value (alphabetical value)
airbnb <- airbnb[!grepl('[[:alpha:] ]', airbnb$Host.Acceptance.Rate),, drop = FALSE]
# Then we remove the "%" from each record 
airbnb$Host.Acceptance.Rate <- sapply(airbnb$Host.Acceptance.Rate, remove_special_charachters)
# Convert the column to a numerical column
airbnb$Host.Acceptance.Rate<-as.numeric(airbnb$Host.Acceptance.Rate)
# We convert the column to numerical columns
airbnb$Host.Listings.Count<-as.numeric(airbnb$Host.Listings.Count)
#Convert column to numerical column
airbnb$Accommodates <-as.numeric(airbnb$Accommodates)
#Convert column to numeric column
airbnb$Bathrooms <- as.numeric(airbnb$Bathrooms)
#Convert column to numeric
airbnb$Bedrooms <- as.numeric(airbnb$Bedrooms)

airbnb$Price <- as.numeric(airbnb$Price)
airbnb$Weekly.Price <- as.numeric(airbnb$Weekly.Price)
airbnb$Monthly.Price <- as.numeric(airbnb$Monthly.Price)
airbnb$Security.Deposit <- as.numeric(airbnb$Security.Deposit)
airbnb$Cleaning.Fee <- as.numeric(airbnb$Cleaning.Fee)
airbnb$Number.of.Reviews <- as.numeric(airbnb$Number.of.Reviews)
airbnb$Extra.People <- as.numeric(airbnb$Extra.People)
airbnb$Guests.Included <- as.numeric(airbnb$Guests.Included)
airbnb$Review.Scores.Rating <- as.numeric(airbnb$Review.Scores.Rating)
airbnb$Review.Scores.Accuracy <- as.numeric(airbnb$Review.Scores.Accuracy)
airbnb$Review.Scores.Checkin <- as.numeric(airbnb$Review.Scores.Checkin)
airbnb$Review.Scores.Cleanliness <- as.numeric(airbnb$Review.Scores.Cleanliness)
airbnb$Review.Scores.Communication <- as.numeric(airbnb$Review.Scores.Communication)
airbnb$Review.Scores.Location <- as.numeric(airbnb$Review.Scores.Location)
airbnb$Review.Scores.Value <- as.numeric(airbnb$Review.Scores.Value)
airbnb$Reviews.per.Month <- as.numeric(airbnb$Reviews.per.Month)

# remove "_new" term from canclation policy in order to reduce the combinations
pattern_removal <- function(x){
  return(str_remove(x, "_new"))
}
airbnb$Cancellation.Policy <- sapply(airbnb$Cancellation.Policy, pattern_removal)

# creating new features
pb <- txtProgressBar(min = 0, max = nrow(airbnb), style = 3)

temp = strsplit(as.character(airbnb$Host.Verifications),",")

for(j in 1:length(temp)){
  lst = temp[[j]]
  if('email' %in% lst){
    airbnb[j, 'email'] <- 1
  }
  if('reviews' %in% lst){
    airbnb[j, 'reviews'] <- 1
  }
  if('kba' %in% lst){
    airbnb[j, 'kba'] <- 1
  }
  if( 'facebook' %in% lst){
    airbnb[j, 'facebook'] <- 1
  }
  if( 'phone' %in% lst){
    airbnb[j, 'phone'] <- 1
  }
  if( 'government_id' %in% lst){
    airbnb[j, 'government_id'] <- 1
  }
  if( 'jumio' %in% lst){
    airbnb[j, 'jumio'] <- 1
  }
  if( 'work_email' %in% lst){
    airbnb[j, 'work_email'] <- 1
  }
  setTxtProgressBar(pb, j)
}

close(pb)

pb <- txtProgressBar(min = 0, max = nrow(airbnb), style = 3)
temp = strsplit(as.character(airbnb$Features),",")
for(j in 1:length(temp)){
  lst = temp[[j]]
  if('Host Has Profile Pic' %in% lst){
    airbnb[j, 'Host Has Profile Pic'] <- 1
  }
  if('Host Identity Verified' %in% lst){
    airbnb[j, 'Host Identity Verified'] <- 1
  }
  if('Is Location Exact' %in% lst){
    airbnb[j, 'Is Location Exact'] <- 1
  }
  if( 'Instant Bookable' %in% lst){
    airbnb[j, 'Instant Bookable'] <- 1
  }
  if( 'Host Is Superhost' %in% lst){
    airbnb[j, 'Host Is Superhost'] <- 1
  }
  if( 'government_id' %in% lst){
    airbnb[j, 'government_id'] <- 1
  }
  if( 'Require Guest Phone Verification' %in% lst){
    airbnb[j, 'Require Guest Phone Verification'] <- 1
  }
  if( 'Require Guest Profile Picture' %in% lst){
    airbnb[j, 'Require Guest Profile Picture'] <- 1
  }
  if( 'Requires License' %in% lst){
    airbnb[j, 'Requires License'] <- 1
  }
  setTxtProgressBar(pb, j)
}

close(pb)

#Replace missing/empty values on the generated columns to 0
airbnb[, 38:53][is.na(airbnb[, 38:53])] <- 0
write.csv(airbnb, 'airbnb_master_table.csv')

# Replace missing values with median of the column
for(i in 1:ncol(airbnb)){
  if(is.numeric(airbnb[,i])){
    airbnb[is.na(airbnb[,i]), i] <- median(airbnb[,i], na.rm = TRUE)
  }
}
nums <- unlist(lapply(airbnb, is.numeric))
cor(airbnb[, nums])

corheat <- round(cor(airbnb[, nums]),2)
library(reshape2)
melted_corheat <- melt(corheat)
head(melted_corheat)

#heatmap of correlation
library(ggplot2)
ggplot(data = melted_corheat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Numerical Variables') + ylab('Numerical Varibles')

#Price vs country
by_country <- airbnb %>% group_by(Country) %>% summarise(mean_price = mean(Price, na.rm = TRUE)) 
by_country <- by_country[-c(1), ]
by_country <- by_country[order(by_country$mean_price),]

library(ggplot2)
ggplot(data=by_country, aes(x=Country, y= mean_price)) + geom_bar(stat="identity", fill = 'black') + 
  xlab('Country') + ylab('Mean Price') + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Import Master Data Set
airbnb<- read.csv("airbnb_master_table.csv", stringsAsFactors=FALSE)

#Grouping by Cancellation Policy
by_cancelation <- airbnb %>% group_by(Country, Cancellation.Policy , na.rm = TRUE) %>% summarise(mean_price = mean(Price, na.rm = TRUE)) 
#Eliminate rows with incorrect inpited info
by_cancelation <- by_cancelation[-c(1:3), ]
by_cancelation <- by_cancelation[-c(59), ]
#Plot
ggplot(data = by_cancelation, aes(y = mean_price, x = Country, fill = Cancellation.Policy)) + geom_bar(stat="identity")  + 
    xlab('Country') + ylab('Mean Price') + 
  ggtitle("Mean Price per Country and Cancellation Policy Plot") + theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Checking for super host
by_host <- airbnb %>% group_by(Property.Type, Country, Host.Is.Superhost)%>% summarise(mean_price = mean(Price, na.rm = TRUE)) 
by_host$IsSuperHost <- as.logical(by_host$Host.Is.Superhost)
by_host <- by_host %>% group_by(Country)
ggplot(by_host, aes(y = Property.Type, x = mean_price, color = IsSuperHost)) + geom_jitter() + theme_classic() +
  xlab('Mean Price') + ylab('Property Type') + scale_shape_manual( values = 5) + ggtitle('Are Super Hosts more Expensive per Country?')
str(by_host)

#Checking for super host diffrent plot    
by_host2 <- airbnb %>% group_by(Property.Type, Country, Host.Is.Superhost)%>% summarise(mean_price = mean(Price, na.rm = TRUE)) 
by_host2$IsSuperHost <- as.logical(by_host2$Host.Is.Superhost)
by_host2 <- by_host2 %>% group_by(Country)
ggplot(by_host2, aes(y = Country, x = mean_price, color = IsSuperHost)) + geom_point() + theme_classic() +
  xlab('Mean Price') + ylab('Country') + scale_shape_manual( values = 5) + ggtitle('Are Super Hosts more Expensive per Country?')
str(by_host)

#Checking property type vs rating 
property_vs_rating <- airbnb %>% group_by(Property.Type, Country.Code, Price) %>% summarise(mean_price = mean(Price, na.rm = TRUE)) 
ggplot(property_vs_rating, aes(x = Property.Type,y = mean_price)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Amenities vs ratings
library(magrittr)
#Selecting only the required columns
amenities_df <- airbnb %>%
  dplyr::select(ID,Amenities,`Review.Scores.Rating`,`Host.Is.Superhost`)
amenities_df <- na.omit(amenities_df)

#changing the amenities column into different columns
library(data.table)
setDT(amenities_df)
amenities_df[, rn := seq_len(.N)]
long_amenities <- amenities_df[, strsplit(Amenities, ",", fixed = TRUE), by = rn]

long_amenities[, V1 := stringr::str_trim(stringr::str_replace_all(V1, '["]', ""))]
amenities_sep <- dcast(long_amenities[!V1 %like% "^translation missing"], rn ~ V1, length, value.var = "rn", fill = 0)

amenities_df <- amenities_df[,c(1,3,4,5)]
final_amenities <- merge(amenities_sep, amenities_df, by = "rn")

#Seperating highly rated listings
a_high <- final_amenities %>%
  filter(final_amenities$`Review.Scores.Rating` >=95) 

a_high <- a_high[, !names(a_high) %in% c("rn", "Review.Scores.Rating","ID")] 
a_high_sum <- colSums(a_high)
a_high_df <-data.frame("Amenities"= colnames(a_high))
a_high_df$Sum <- a_high_sum
a_high_df <- a_high_df[order(a_high_df$Sum),]
a_high_df <- a_high_df %>%
  filter(a_high_df$Sum >= 2500)

#50 amenities of highly rated listings
ggplot(a_high_df,aes(x=reorder(Amenities,Sum),y = Sum,fill = a))+geom_bar(stat = "identity", fill="steelblue")+coord_flip()+labs(x="Amenities",y="Number of listings",title="Highly rated listings")+theme_classic()

#Cleanliness rating vs cleaning fee
ggplot(airbnb, aes(x=Cleaning.Fee, y=Review.Scores.Cleanliness)) + geom_point() + labs(title = "Cleaning fee vs cleanliness score")+theme_classic()

#Cancellation policy of superhosts
plot <- summarise(group_by(airbnb,Cancellation.Policy,`Host.Is.Superhost`), count.plot = n())
plot$`Host.Is.Superhost`<-ifelse(plot$`Host.Is.Superhost`=="1",TRUE,FALSE)
plot <- plot[-1,]
ggplot(plot, aes(x=Cancellation.Policy,y = count.plot ,fill=`Host.Is.Superhost`)) + geom_bar(stat = "identity")

#What it takes to be a Superhost
airbnb$Superhost<- ifelse(airbnb$`Host.Is.Superhost`=="1",TRUE,FALSE)
ggplot(airbnb,aes(x= Host.Acceptance.Rate,y=Review.Scores.Rating))+geom_point(aes(colour=Superhost))+labs(title="What it takes to be a superhost")

