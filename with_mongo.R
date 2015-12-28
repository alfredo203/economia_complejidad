library(rmongodb)

mongo <- mongo.create()
query <- mongo.bson.from.JSON('{"stars": 1}')
system.time(t <- mongo.find.batch(mongo, "bs.review", query, limit=500)) ##limit=5000L
# res <- ldply(res, data.frame)

# mongoexport --csv -d bs -c review -q '{"stars":5}, {"text":1, "stars":1}' -f "text","stars" -o data_rev.csv


library(tm)
library(wordcloud)
system.time(t_clean <- Corpus(VectorSource(t)))

##Select some samples
inspect(t_clean[1])

# Tidy data generation
system.time(t_clean<-tm_map(t_clean, removePunctuation))
t_clean<-tm_map(t_clean, content_transformer(tolower))
system.time(t_clean<-tm_map(t_clean, removeWords, stopwords("english")))
t_clean<-tm_map(t_clean, removeNumbers)
t_clean<-tm_map(t_clean, stripWhitespace)

# Remove the search words?
yelp_words = c("cool","listfunny", "review", "useful" )
t_clean<-tm_map(t_clean, removeWords, yelp_words)

system.time(wordcloud(t_clean, min.freq = 25,
                      random.order = F, 
                      scale = c(3, 0.2),
                      col= c("red","black", "orange"))) ##col =rainbow(5) brewer.pal(8, "Paired")

library(openNLP); library(RWeka); library(Rgraphviz)
token_t <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

system.time(crudeTDM <- TermDocumentMatrix(t_clean, control = list(tokenize = token_t)))
print(crudeTDMHighFreq <- findFreqTerms(crudeTDM, 14, Inf))

plot(crudeTDM, terms = c(crudeTDMHighFreq), corThreshold = 0.05, weighting = TRUE)


## Tokenize by hand, not fully worked
# library(sets)
# for(i in 1:4000){
# A <- set(as.list(t_clean[[1]]))
# B <- set(as.character(t_clean[[i+1]]))
# X[i]<-set_similarity(A, B)}
# 
# # meta<-NGramTokenizer(t_clean, Weka_control(min = 1, max = 1))
# A<-NGramTokenizer(t_clean, Weka_control(min = 1, max = 1))
# 
# metadata<-c("list","content","=","meta","author", "character",
#   "0", "datetimestamp", "sec", "min", "hour", "year", 
#   "wday", "description", "id", "language", "origin", "content")
# f<-0
# for (i in 1:length(A)){
#         f[i]<-paste(A[i],A[i+1], sep = "_")
# }
# f