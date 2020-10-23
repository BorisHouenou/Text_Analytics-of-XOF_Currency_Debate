
#import packages

install.packages("tm")
install.packages("RWeka")
install.package("slam")

library("tm")
library("RWeka")
library("slam")
setwd("filepath")
#import csv
mydata = read.csv("cfa_scrapping_facebook.csv", sep = ";")  # read csv file 

#prepare text
corpus <- Corpus(VectorSource(mydata$message)) # create corpus object

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("les", "pour", "que", "qui",
                                     "kemi", "seba", "est", "avec", "et", 
                                     "ou", "ni", "car", "donc", "facebook", 
                                     "urgences", "dans", "une", "un", 
                                     "des", "nos", "par", "vous", 
                                     "sont", "notre", "quand", "ils", 
                                     "la", "le", "cest", "plus", "nous",
                                     "http", "web", "com", "sur", "aux","pas","son", 
                                     "leur", "alors","ton", "ces", "ça", "lol", "rfi", "foka", "datafetched"
                                     ,"facebookpostlikes", "facebookobject", "data", ""))
# convert all text to lower case
corpus <- tm_map(corpus, tolower)

#make the term document matrix
tdm <- TermDocumentMatrix(corpus)

#find the frequent terms
findFreqTerms(tdm, lowfreq = 500)
# Turn term document matrix into true matrix
dat_m <- as.matrix(tdm)
head(dat_m)

# Find the row sums (i.e. total number of times a word is used); this will be a vector
dat_vec <- rowSums(dat_m)
head(dat_vec)

# Sort this vector so most abundant terms are first
dat_vec_sort <- sort(dat_vec, decreasing=TRUE)
head(dat_vec_sort)

# Turn this into a data frame, with names of vector as "word" and "freq" as value
str(dat_vec_sort)
dat_ready <- data.frame(word = names(dat_vec_sort),freq=dat_vec_sort)
head(dat_ready, 10)


library(dplyr)
library(tidytext)

# First, make the term document matrix "tidy"
dat_tidy <- tidy(tdm)
head(dat_tidy)

# Next, we group by the term and find the sum of count for that term
# We then arrange so that the most abundant terms are first
dat_tidy_sums <- dat_tidy %>%
  group_by(term) %>%
  summarize(word_sums = sum(count, na.rm = TRUE)) %>%
  arrange(desc(word_sums)) %>%
  as.data.frame()

head(dat_tidy_sums)

# First, use set.seed so each time we make the word cloud it is identical
set.seed(1234)

# Make the word cloud, adjusting the defaults for greater control
wordcloud(words = dat_ready$word, freq =dat_ready$freq, 
          min.freq = 20,  
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Find frequency and correlations between terms
findFreqTerms(tdm, lowfreq = 4)

a<-findAssocs(tdm, terms = "sortir", corlimit = 0.3)
a<-unlist(a)
a<-sort(a, decreasing=TRUE)
write.csv(a, file = "sortir.csv")
str(a)

a<-data.frame(word = names(a),freq=a)
colnames(a)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(a$Correlation[1:10],  names.arg =a$Mots[1:10],
        col ="lightblue", main ="Mots les plus Correlés avec 'Sortir' du CFA",
        ylab = "Correlation")
b<-findAssocs(tdm, terms = "cfa", corlimit = 0.3)
b<-unlist(b)
b<-sort(b, decreasing=TRUE)
write.csv(b, file = "sortir.csv")
str(b)
b<-data.frame(word = names(b),freq=b)
colnames(b)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(b$Correlation[1:10],  names.arg =b$Mots[1:10],
        col ="lightblue", main ="Mots les plus Correlés avec  CFA",
        ylab = "Correlation")
c<-findAssocs(tdm, terms = "rester", corlimit = 0.3)
c<-unlist(c)
c<-sort(c, decreasing=TRUE)
write.csv(c, file = "sortir.csv")
str(c)
c<-data.frame(word = names(c),freq=c)
colnames(c)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(c$Correlation[1:5],  names.arg =c$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec 'Rester' dans le CFA",
        ylab = "Correlation")
d<-findAssocs(tdm, terms = "reforme", corlimit = 0.3)
d<-unlist(d)
d<-sort(d, decreasing=TRUE)
write.csv(d, file = "sortir.csv")
d<-data.frame(word = names(d),freq=d)
colnames(d)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(d$Correlation,  names.arg =d$Mots,
        col ="lightblue", main ="Mots les plus Correles avec Reforme du CFA",
        ylab = "Correlation")
e<-findAssocs(tdm, terms = "bceao", corlimit = 0.3)
e<-unlist(e)
e<-sort(e, decreasing=TRUE)
write.csv(e, file = "sortir.csv")
str(e)
e<-data.frame(word = names(e),freq=e)
colnames(e)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(e$Correlation[1:5],  names.arg =e$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec BCEAO",
        ylab = "Correlation")
f<-findAssocs(tdm, terms = "beac", corlimit = 0.3)
f<-unlist(f)
f<-sort(f, decreasing=TRUE)
write.csv(f, file = "sortir.csv")
str(f)
f<-data.frame(word = names(f),freq=f)
colnames(f)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(f$Correlation[1:5],  names.arg =f$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec BEAC",
        ylab = "Correlation")
g<-findAssocs(tdm, terms = "france", corlimit = 0.3)
g<-unlist(g)
g<-sort(g, decreasing=TRUE)
write.csv(g, file = "sortir.csv")
str(g)
g<-data.frame(word = names(g),freq=g)
colnames(g)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(g$Correlation[1:5],  names.arg =g$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec France",
        ylab = "Correlation")
h<-findAssocs(tdm, terms = "euro", corlimit = 0.3)
h<-unlist(h)
h<-sort(h, decreasing=TRUE)
write.csv(h, file = "sortir.csv")
str(h)
h<-data.frame(word = names(h),freq=h)
colnames(h)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(h$Correlation[1:5],  names.arg =h$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec Euro",
        ylab = "Correlation")
i<-findAssocs(tdm, terms = "dopération", corlimit = 0.3)
i<-unlist(i)
i<-sort(i, decreasing=TRUE)
write.csv(i, file = "sortir.csv")
str(i)
i<-data.frame(word = names(i),freq=i)
colnames(i)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(i$Correlation[1:5],  names.arg =i$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec Compte d'operations CFA",
        ylab = "Correlation")
j<-findAssocs(tdm, terms = "imprimer", corlimit = 0.3)
j<-unlist(j)
j<-sort(j, decreasing=TRUE)
write.csv(j, file = "sortir.csv")
str(j)
j<-data.frame(word = names(j),freq=j)
colnames(j)<-c("Mots",  "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(j$Correlation[1:5],  names.arg =j$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec Imprimer CFA",
        ylab = "Correlation")
k<-findAssocs(tdm, terms = "veto", corlimit = 0.3)
k<-unlist(k)
k<-sort(k, decreasing=TRUE)
write.csv(k, file = "sortir.csv")
str(k)
k<-data.frame(word = names(k),freq=k)
colnames(k)<-c("Mots", "Correlation")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(k$Correlation[1:5],  names.arg =k$Mots[1:5],
        col ="lightblue", main ="Mots les plus Correlés avec droit de veto CFA",
        ylab = "Correlation")
# world clouds avec worldcloud2 for shape
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
wordcloud2(dat_ready, size = 4, minSize = 4, gridSize =  1,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
letterCloud(dat_ready, word="cfa", wordSize = 4)

###Ploting the most frequent words
colnames(dat_ready)<-c("word", "freq")
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
barplot(dat_ready$freq[1:5], names.arg =dat_ready$word[1:5],
        col ="lightblue", main ="Les mots les plus frequents",
        ylab = "Frequnences des mots")

# Play with settings until get a cloud you like
# Can adjust colors, min.freq, max.words, rotation percentage, etc.

# Finally, save to png
png("CFA", width = 4, height = 4, units = "in", res = 400)
set.seed(20)
wordcloud(words = dat_ready$word, freq = dat_ready$freq, 
          min.freq = 12,max.words=100, 
          random.order=FALSE, rot.per=0.40, #family = "serif", font = 3,
          colors=brewer.pal(8, "Dark2"))
dev.off()







