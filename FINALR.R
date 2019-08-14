#R FINAL PROJECT USING AIRBNB HOUSING DATA
#WRITTEN BY: Alexander Qaddourah, Junji Weiner, Aldo Peter,
# Alyson Chen, and Wenbin Yang.


# Importing all Denver AIRBNB Data in R.

calendar <- read.csv("C:\\Users\\Owner\\Desktop\\R Project AirBNB\\denver_calendar.csv",stringsAsFactors=FALSE)
listings <- read.csv("C:\\Users\\Owner\\Desktop\\R Project AirBNB\\denver_listings.csv",stringsAsFactors=FALSE)
reviews <- read.csv("C:\\Users\\Owner\\Desktop\\R Project AirBNB\\denver_reviews.csv",stringsAsFactors=FALSE)

# URL contains a link to all Neighbourhood Airbnb web pages. We will use this
#later in the code.

URL <- read.csv("C:\\Users\\Owner\\Desktop\\R Project AirBNB\\URLs.csv",stringsAsFactors=FALSE)


# Installing package leaflet which is the core for the map we want to build.

install.packages('leaflet')
require('leaflet')

#Copy for testing:

listings2 <- listings

#Convert all price column in listing2 to numeric, while also taking out
#the dollar sign:
#We want to do this so we can see the normal price/night column but also fill
#in the missing values for properties in the price/month column. 

listings2$price <- sub(pattern="$",replacement="",listings2$price,fixed=TRUE)
listings2$price <- as.numeric(listings2$price)
head(listings2$price)

listings2$weekly_price <- sub(pattern="$",replacement="",listings2$weekly_price,fixed=TRUE)
listings2$weekly_price <- as.numeric(listings2$weekly_price)
head(listings2$weekly_price)


#Filling in "NA's" with 0's for assignment.

listings2[is.na(listings2)] <- 0


#Fill empty weekly prices with price/night * 7:

listings2$price_WEEK <- rep(0,times=nrow(listings2))

listings2[listings2$weekly_price == 0,'price_WEEK'] <- listings2[listings2$weekly_price == 0,'price']*7

listings2[listings2$weekly_price != 0,'price_WEEK'] <- listings2[listings2$weekly_price != 0,'weekly_price']


#FINAL COLUMNS INFORMATION FOR NIGHT/WEEK/MONTH
head((listings2$price),25)
head((listings2$price_WEEK),25)

######################
#Now that we have built our first 2 price columns that we will incorporate into
#the map, we can start subsetting/aggregating other important data that we want
#to appear into our map.
#NOTE: NB = Neighbourhood
######################
#Using Aggregate to find Mean Prices per NB:
#Gives mean prices per night and mean prices per month per NB.

zNIGHT <- aggregate(price~neighbourhood, FUN=mean, data=listings2)
zWEEK <- aggregate(price_WEEK~neighbourhood, FUN=mean, data=listings2)

zNIGHT$price <- round(zNIGHT$price)
head(zNIGHT)
zWEEK$price_WEEK <- round(zWEEK$price_WEEK)
head(zWEEK)

zNIGHT
zWEEK

########################
#Using aggregate to find Average Rating per NB
#Gives average rating per NB.

zRATING <- aggregate(review_scores_rating~neighbourhood, FUN=mean, data=listings2)

zRATING$review_scores_rating <- round(zRATING$review_scores_rating)

zRATING

######################
#Using aggregate to find Count of Property Type per NB
#This code gives us the total amount of properties available in a NB.

zTYPE <- aggregate(property_type~neighbourhood, FUN=length, data=listings2)
zTYPE

#####################
#Here, we need latitude and longitude to enter into the leaflet package so
#that it knows where to plot the markers. We pulled the lat,long,and NB columns
#out of the original data and entered it into a new dataframe called newdata.

#We then found the mean latitude and mean longitude for each NB, after using 
#a few aggregate lines of code. This gave us our variable NBmeans which we will
#use to plot our map markers.

lats <- c("latitude", "longitude", "neighbourhood")
newdata <- listings2[lats]
head(newdata)

aggregate(latitude ~ longitude, FUN=mean, data=newdata)
latitudeAverage <- aggregate(latitude ~ neighbourhood, FUN=mean, data=newdata)
longitudeAverage <- aggregate(longitude~ neighbourhood, FUN=mean, data=newdata)

NBmeans <- cbind(latitudeAverage, longitudeAverage) 

head(NBmeans)
#At first, NBmeans produced 2 columns of neighbourhood. The code below removes
#the second instance of the column.
NBmeans <- subset(NBmeans,select=-c(3))
head(NBmeans)

#####################
#Below is our text analysis. We decided to run a small text analysis to
#determine if said property allowed pets or did not allow pets. We ran our
#analysis inside the original data in the "house rules" column.

txtPET <- listings2[grepl("no pet|no dog|no dogs|no cats|no cat",listings2$house_rules),]

#We aggregated to find the number of properties per NB that did NOT allow pets.
notpetfriendly <- aggregate(property_type~neighbourhood,data=txtPET,FUN=length)

zPet <- merge(zRATING, notpetfriendly, by='neighbourhood',all=TRUE)
zPet[is.na(zPet)] <- 0
#For zPet, properties that return a 0 are one's that have 0 mentions of "no
#dogs,cats,etc" and thus are petfriendly. The NBs that returned anything over 1
#are not.
#####################
#Here, we created a "master data set" that contained all useful columns and 
#information that we wanted to build our map with. By only using a select # of
#columns, we can avoid using the normal listings data that contains 100+ columns.


#Into this new dataframe named zDATA, we have incorporated both price averages,
#average ratings, number of properties available per NB, the URL for each NB
#(explained at the top of the page), our text analysis for Pets vs no Pets, and
#finally our NBmeans data, which is our averaged lat and long for each NB.
zDATA <- cbind(zRATING,zNIGHT,zWEEK,NBmeans,zTYPE,URL)
zDATA <- subset(zDATA,select=-c(3,5,7,10,12))
head(zDATA)

########################
library(leaflet)

#########MAPPING##########

#The first block of our map code helps us color the markers in our map based
#on certain conditions. In this case, we want the color to be based on zNIGHT
#which is price per night. In the code, we want the color to be green if it is
#below $100 a night, $101-$200 makes it orange, and over $200 makes it a red
#marker. 

getColor <- function(zNIGHT) {
  sapply(zNIGHT$price, function(price) {
  if(price <= 100) {
    "green"
  } else if(price <= 200) {
    "orange"
  } else {
    "red"
  } })
}

#The icons variable can be tied to "awesomeIcons" so that icons knows that it
#needs to change the colors.

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(zNIGHT)
)

#For loop that tests in review scores and tests if the rating is 
#100-90,99-80,79-70,69-60, or lower, it will be assigned a series of stars.
#The "u2b50" line is an emoji character for a star. Thus, we have 5 of them
#for the first group, 4 for the second and so on. 

for (i in 1:length(zDATA$review_scores_rating)){
	if(zDATA$review_scores_rating[i] >= 90){
		zDATA$review_scores_rating[i] <- "\u2b50?\u2b50?\u2b50?\u2b50?\u2b50?"
	}else if(zDATA$review_scores_rating[i] >= 80){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?\u2b50?\u2b50?'
	}else if(zDATA$review_scores_rating[i] >= 70){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?\u2b50?'
	}else if(zDATA$review_scores_rating[i] >= 60){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?'
	}else{
		zDATA$review_scores_rating[i] <-'\u2b50?'
	}
}

#Here, we did a similar emoji system but in the zPet data that was run above.
#If the value was 0, as in, the property had 0 mentions of no pets/dogs, it
#will be assigned the code for a dog emoji.
#If the value was not 0, it will have the code for a "people" emoji.

for (i in 1:length(zPet $property_type)){
	if(zPet $property_type[i] == 0){
		zPet $property_type[i] <- "\U0001f436"
	}else{
		zPet $property_type[i] <- '\U0001f46b'
		}
}

#Lastly, we are now ready to run our map. Using leaflet, we can use the lat
#and long from our zDATA to create our markers per NB. 
#After this, each label in quotes is a label that will display in the popup 
#section of the map when a marker is clicked upon. After the label, we can 
#pull in the data column that we want to appear here. As you can see, we brought 
#neighbourhood, the average ratings, the average prices, and the # of properties
#, and our emoji entry for pet vs no pet NBs.
# The last line brings in the URL data set created at the top of the page, and 
#turns it into an html so the user can click on whichever NB they would like to
#see the properties on the airbnb website. 

leaflet(zNIGHT) %>% addTiles() %>%  
  addAwesomeMarkers(lng = zDATA$longitude, lat = zDATA$latitude,icon=icons,
             popup = paste("Neighbourhood:", zDATA$neighbourhood, "<br>",
                           "Avg Score:", zDATA$review_scores_rating, "<br>",
                           "Avg Price/Night:", zDATA$price, "<br>",
                           "Avg Price/Week:", zDATA$price_WEEK, "<br>",
				   "Number of Properties:",zDATA$property_type, "<br>",
				   "Suitable for:",zPet$property_type, "<br>",
                           "<a href='", URL$URL , "' target='_blank'>", "AirBnb Properties in this Neighbourhood:</a>"))

#######################
######TXT ANALYSIS#####
#######################


#All of the code below relates to building a word cloud of most common words
#for the ENTIRE Denver Housing Market:

#text analysis and word mining code:
#packages required:

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

#load in library's:

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#read in csv file
text <- readLines(file.choose())
head( text )

#load docs in as corpus which is a collection of documents:
docs <- Corpus(VectorSource(text))
inspect(docs)

#tm_map used to change and replace special characters from the text:

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "!")
docs <- tm_map(docs, toSpace, "we’re")
docs <- tm_map(docs, toSpace, "We’re ")

#below functions cleans up the file:

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))

#removed repetative words that we though were not important to wordcloud:

docs <- tm_map(docs, removeWords, c("one", "denver", "Denver", "'s", "neighborhood",
"away", "within", "rino", "neighborhoods", "south", "field", "can", "denvers",
"many", "also", "highlands", "hill", "will", "north", "minute", "two", "colfax",
"five", "lodo", "find", "highright", "house", "well", "bar", "get", "zooget",
"just", "min", "colorado", "points", "homes", "colfax", "two", "around",
"min", "take", "high", "less", "points", "across", "around", "walk",
"blocks", "right", "like", "store", "mile", "street", "city", "distance",
"minutes", "located", "west"))

#creates a term document matrix of all the words that repeat
#and their frequency:

denTerms <- TermDocumentMatrix(docs)
m <- as.matrix(denTerms)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

#creates the wordcloud with the parameters set, wich are:
#minimum frequency is 200, and max number of words 100.

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 200,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#adds a title to the wordcloud:

mtext("Denver Air BnB Word Overview", side = 3, padj = -1)

#plots in a bar chart, the most frequent words used after we removed
#some in the docs <- tm_map(docs, removeWords code line:

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="blue", main ="Denver Air BnB Word Frequency",
        ylab = "Word Count")



########################
#######REGRESSION#######
########################
#Below are three regression models that we put together.


#The one below uses the home provided in the project description in Question 3
#to help predict the price that should be set for rental.

#Our thought was that we only wanted to test it against homes in Sloane Lake,
#because that is where the home is located, and using other neighbourhoods to 
#Predict it would be innaccurate. We decided to use Sloane Lake homes for bed
#and bath because those are two things that most people look at when selecting a
#home.

#Sloane Lake Home Prices vs Bedrooms/Bath in Sloane Lake.

lakeDATA <- subset(listings2, neighbourhood='Sloane Lake')


lakeDATA

summary(lm(price~bedrooms+bathrooms,data=lakeDATA))

#Price Estimation off beds and baths:
19.974 +  51.106(beds) + 24.784(baths)

#PRICE = 
19.974 +  51.106*(4) + 24.784*(2)
#$273.97/night


#Graph of above:
ggplot (data=lakeDATA,aes(y=price,x=bedrooms+bathrooms))+
	geom_point()+
	geom_smooth(method='lm')+
	theme_bw()

################
#The code below explains the other two regression plots we ran.

listings <- read.csv("C:\\Users\\Owner\\Downloads\\denver_listings(1).csv", stringsAsFactors=FALSE)
listings


#Install emoGG library to graphs

devtools::install_github("dill/emoGG")
library(emoGG)
emoji_search('house')

#sub data set for zip codes, smart location, lat and long, that will be used
#to map the neighbourhoods by price

#master subset of data including zipcode, price per night, smart_location, neighbourhood,
#property_type, accomodates, number of bathrooms, number of bedrooms and number of beds
myVar2 <- c("zipcode", "price", "smart_location", "neighbourhood", "property_type", "accommodates", "bathrooms", "bedrooms", "beds","review_scores_rating")
masterSub <- listings[myVar2]
as.dataframe <- masterSub
head( masterSub )
masterSub$price <- sub( pattern="$", replacement="", masterSub$price, fixed=TRUE )
masterSub$price <- sub( pattern=",", replacement="", masterSub$price, fixed=TRUE )
masterSub$price <- as.numeric( masterSub$price )
head( masterSub )

avgPrice <- as.data.frame(aggregate(price~property_type, FUN=mean, data=masterSub))

avgPrice$apr<- round(avgPrice$price)

head(avgPrice)

unique(masterSub)

#Bar Chart model Property Type to Price:#

library(ggplot2)

aPrice <- ggplot(avgPrice, aes(x=property_type, y=apr))
aPrice + geom_histogram(size=5)+(aes(colour=factor(apr)))+
xlab('Property Types')+
ylab('Average Price')

aPrice2 <- ggplot(avgPrice, aes(x=property_type, y=apr))
aPrice2 + theme(axis.text.x=element_text(angle=90,hjust=1))+
geom_bar(stat='identity', color="red", fill="lightblue")+
xlab('Property Types')+
ylab('Average Price')+
ggtitle("Average Price per Property Type")

#Regression Average Price vs. Average Review:#

ratingPrice$review_scores_rating <- as.factor(ratingPrice$review_scores_rating)
ratingPrice <- as.data.frame(aggregate(price~review_scores_rating, FUN=mean, data=masterSub))
head(ratingPrice)

ratingPrice2 <- ggplot(ratingPrice, aes(x=review_scores_rating, y=price))+
geom_emoji(emoji="1f3e0", size=.05)+
xlab('Average Review Score')+
ylab('Average Price')+
ggtitle("Average Price vs. Average Review Score")+
geom_smooth(method='lm', color='darkblue')
ratingPrice2

#Regression Equation to find Price based off Review:#

summary(lm(price~review_scores_rating,data=ratingPrice))

#Price check:
Price = 189.61 + .20(rating)
189.61+(90*.20)

##end
