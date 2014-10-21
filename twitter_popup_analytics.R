# Twitter Export Analytics
# Myles Harrison
# October 2014
# http://www.everydayanalytics.ca

# SUMMARY GRAPHS
# Read in the twitter data
data <- read.csv(file='tweets.csv',header=T, stringsAsFactors=FALSE)

# Convert times to POSIXct
data$timestamp <- as.POSIXct(data$timestamp)

# Time trending
monthly <- table(cut(data$timestamp, breaks="month"))
names(monthly) <- substr(names(monthly), 1, 7)
plot(monthly, ylab='# tweets', main='Tweets per month', type='o',
     pch=16, col='darkblue')
box()
summary(as.vector(monthly))

# Barplot of total tweets by month of year
month <- table(format(data$timestamp, '%m'))
barplot(month, col='darkblue', main='Total Tweets by Month of Year',
        ylim=c(0, max(month)*1.1), ylab="Total Tweets", xlab="Month of Year")
box()

# Barplot of total tweets by weekday
weekday <- table(format(data$timestamp, '%A'))
dow <- factor(names(weekday), levels=c('Sunday', 'Monday', 'Tuesday', 
                                 'Wednesday', 'Thursday', 'Friday', 'Saturday'), ordered=T)
hrs <- seq(1:24)

weekday <- weekday[order(dow)]
barplot(weekday, col='darkblue', main='Total Tweets by Day of Week',
        ylim=c(0, max(weekday)*1.1), ylab="Total Tweets", xlab="Day of Week")
box()

# Barplot of total tweets by hour
hours <- aggregate(tweet_id ~ as.numeric(format(data$timestamp, "%H")), data=data, FUN=length)
# Create dummy vector and populate (to account for hours w zero tweets)
plot(hours, col='darkblue', main='Total Tweets by Hour of Day', lwd=15, type='h',
        ylim=c(0, max(hours)*1.1), ylab='Total Tweets', xlab='Hour of Day')
box()

# Tweets by source
data$source <- sub("</a>", "", sub("<.*?>", "", data$source))
bysource <- table(data$source)
barplot(bysource, col='darkblue', main="Total Tweets by Source", ylim=c(0, max(bysource)*1.1),
        xlab='Tweet Source', ylab='Total Tweets')
box()

# Distribution of tweets by length
h <- hist(nchar(data$text), breaks=seq(1:40)*4,plot=FALSE)

plot(h$mid, h$counts, type='o', pch=16, col='darkblue', 
     main='Tweet Distribution by Length', xlab='Tweet Length', ylab='# tweets')

# TEXT MINING
library(tm)

corpus <- Corpus(VectorSource(data$text))

# Remove stopwords, punctuation, and tolower
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, PlainTextDocument)

# Create Document-Term matrix and find frequent terms
dtm <- DocumentTermMatrix(corpus)
v <- sort(colSums(as.matrix(dtm)), decreasing=T)
# Exclude hashtags and '@'s
i <- grep("^#|^@", names(v), invert=T)
terms <- v[i]
oldmar = par("mar")
par(mar=par("mar")+c(0,4,0,0))
barplot(rev(terms[1:10]), col='darkblue', horiz=T, las=1, xlab='# of tweets',
        main='Terms by appearance', xlim=c(0, max(terms)*1.1))
box()

# Find frequently occuring hashtags
i <- grep("^#", names(v))
hashtags <- v[i]
barplot(rev(hashtags[1:10]), col='darkblue', horiz=T, las=1, xlab='# of tweets',
        main='Hashtags by appearance', xlim=c(0, max(hashtags)*1.1))
box()
