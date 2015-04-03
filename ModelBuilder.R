options(warn=-1)

library(tm)
library(RWeka)
library(wordcloud)
library(plyr)
library(stringr)
options(warn=0)



set.seed(4150)

filepre<-function(filename, chunks){
    fullfile<-readLines(filename)
    len<-length(fullfile)
#    sampfile<-sample(1:len,len*rate)
    newbase<-str_sub(filename,1,str_length(filename)-4)
    start<-1
    for(i in 1:chunks){
        print(start)
        output<-paste(newbase, i, ".txt", sep="")
        end<-round(i*len/chunks)   
        print(end)
        write(fullfile[start:end], output)
        start<-end
    }
 #   write(fullfile[sampfile], output)
}


corp<-VCorpus(DirSource("C:/Users/elarsen/Coursera work/Capstone/data/samp2"), readerControl=list(readPlain))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
unigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))


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


