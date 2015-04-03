options(warn=-1)

library(tm)
library(RWeka)
library(wordcloud)
library(plyr)
library(stringr)
options(warn=0)



set.seed(4150)

filepre<-function(dir, filename, chunks){
    fullfile<-readLines(paste(dir, "/", filename, sep=""))
    len<-length(fullfile)
#    sampfile<-sample(1:len,len*rate)


    newbase<-str_sub(filename,1,str_length(filename)-4)
    start<-1
    for(i in 1:chunks){
        output<-paste(dir, "/split/split",i, "/", newbase, i, ".txt", sep="")
        end<-round(i*len/chunks)   
        print(output)
        write(fullfile[start:end], output)
    }
 
}



# need to make and if this dir exists and move this code into the pre function.
dir.create("data/split")


chunks<-10
for(i in 1:chunks){
    newdir<-paste("data/split/split", i, sep="")
    dir.create(newdir)
}

filepre("data", "en_US.news.txt", chunks)
filepre("data", "en_US.blogs.txt", chunks)
filepre("data", "en_US.twitter.txt", chunks)


corp<-VCorpus(DirSource("data/split/split1"), readerControl=list(readPlain))

corp<-tm_map(corp, removeWords, stopwords("english"))
corp<-tm_map(corp, content_transformer(tolower))
corp<-tm_map(corp, removePunctuation)
corp<-tm_map(corp, removeNumbers)
corp<-tm_map(corp, PlainTextDocument)

#Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
#unigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

unigram

# this function aggregates the counts by document and orders the table in descending frequency

gramalyzer<-function(tdm){ 
    df<-as.data.frame(rowSums(as.matrix(tdm)))
    colnames(df)[1]<-'Frequency'
    df$Phrase<-row.names(df)
    df<-arrange(df, desc(Frequency))
}

df1<-gramalyzer(unigram)
df2<-gramalyzer(bigram)
df3<-gramalyzer(trigram)
df4<-gramalyzer(quadgram)

df2[1:20]
