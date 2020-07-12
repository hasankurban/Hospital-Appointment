# @Dr. Kurban, July 2020
# Analysis and Modeling of Disease Symptoms and Medical Treatment Areas by Text Mining and
# Data Science Methods
#############################################################################################
# Reading the data
term.matrix<- read.csv("termMatrix.csv", sep = ";",header =TRUE, row.names = 1,colClasses = )
term.matrix[is.na(hastane.data)]<-0
# medical treatment fields as columns
transpoz.term.matrix<-as.data.frame(t(term.matrix))
#frequency of symptoms over medical treatment fields
temp.data <- as.data.frame(sort(colSums(transpoz.term.matrix), TRUE),ncol=1)
temp.vector = row.names(temp.data) 
temp.data <- as.data.frame(cbind(temp.vector,temp.data))
colnames(temp.data) <- c("medicalTreatmentFields","SymptomsNumber")
#visualize frequency of symptoms over medical treatment fields
library(ggpubr)
theme_set(theme_pubr())
ggplot(temp.data, aes(x = reorder(medicalTreatmentFields, -SymptomsNumber), y = SymptomsNumber)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = SymptomsNumber), vjust = -0.3) + xlab("Tedavi Alanı")+
  ylab("Toplam Belirti Sayısı") +theme_pubclean()
###############################################################################
#                         HIERARCHICAL CLUSTERING
###############################################################################
#Visualize correlation matrix 
corrplot(cor(tranposz.term.matrix,method = "kendall"), type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue")
# Jaccard dissimariy matrix
jaccard.similarity <- dist(as.matrix(term.matrix),method ="binary")   
par(mfrow=c(3,2))
#hirarchical clustering 
hc1 <- hclust(jaccard.similarity, method = "complete")               
plot(hc1, main= "Complete Hiyerarşik Kümeleme")  
hc2 <- hclust(jaccard.similarity ,method = "single") 
plot(hc2, main= "Single Hiyerarşik Kümeleme") 
hc3 <- hclust(jaccard.similarity ,method = "average") 
plot(hc3, main= "Average Hiyerarşik Kümeleme") 
hc4 <- hclust(jaccard.similarity ,method = "ward.D") 
plot(hc4, main= "Ward Hiyerarşik Kümeleme") 
hc5 <- hclust(jaccard.similarity ,method = "mcquitty") 
plot(hc5, main= "Mcquitty Hiyerarşik Kümeleme") 
###############################################################################
#                          WORD CLOUD
###############################################################################
install.packages("tm")  #text mining
library("tm")
install.packages("wordcloud") #word-cloud 
library("wordcloud")
install.packages("RColorBrewer") #color 
library("RColorBrewer")

#read data
#203 symptoms and 13 fields, titles are removed
text <- readLines("wordCountData.txt")
#data as a corpus
todocs <- Corpus(VectorSource(text))
inspect(todocs)

#remove unnecessary symbols
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
todocs <- tm_map(todocs, toSpace, "/")
todocs <- tm_map(todocs, toSpace, "\\|")
# text transformation: to lower case
todocs <- tm_map(todocs, content_transformer(tolower))
# stopwords 
todocs <- tm_map(todocs, removeWords, c("bir", "eden","herhangi", "iki", "hastalıklarda", "ile","gibi",
                                    "gibi", "kendini","olarak" ,"hastalıkları","hastalığı", "hastalık","şikayetleri", "fakat", "için", "yada","olarak", "buna", "veya")) 

todocs <- tm_map(todocs, removePunctuation) # Remove punctuations
# Eliminate extra white spaces
todocs <- tm_map(todocs, stripWhitespace)
freq.matrix <- sort(rowSums(as.matrix(TermDocumentMatrix(todocs))), decreasing =TRUE) # term matrix
final.data <- data.frame(word = names(freq.matrix),freq=freq.matrix)
head(final.data, 10)
#tag cloud
wordcloud(
  words = final.data$word, 
  freq = final.data$freq, 
  min.freq = 2, max.words=500,
  colors = brewer.pal(8, 'Dark2'))
# frequency bar plot
barplot(final.data[1:10,]$freq, las = 2, names.arg = final.data[1:10,]$word,
        col ="lightblue", main ="En Sık Kullanılan Kelimeler",
        ylab = "Kelime  Görünme Sıklıkları")
###############################################################################