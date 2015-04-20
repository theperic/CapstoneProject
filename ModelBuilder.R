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
        start<-end
    }
    
}

gramalyzer<-function(tdm){ 
    df<-as.data.frame(rowSums(as.matrix(tdm)))
    colnames(df)[1]<-'Frequency'
    df$Phrase<-row.names(df)
    df<-arrange(df, desc(Frequency))
}

maketable<-function(ngram){
    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram, max = ngram))
    xgram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))
    df<-gramalyzer(xgram)
    write.table(df[df$Frequency>1,], paste("df",ngram, "_", i,".txt", sep=""))
    df<-NULL
}


# need to make and if this dir exists and move this code into the pre function.
dir.create("data/split")


chunks<-20
for(i in 1:chunks){
    newdir<-paste("data/split/split", i, sep="")
    dir.create(newdir)
}

filepre("data", "en_US.news.txt", chunks)
filepre("data", "en_US.blogs.txt", chunks)
filepre("data", "en_US.twitter.txt", chunks)
chunks<-20
for(i in 1:chunks)
    #    i<-3
    
{
    print(i)
    dir<-paste("data/split/split",i,sep="")
    
    corp<-VCorpus(DirSource(dir), readerControl=list(readPlain))
    
    corp<-tm_map(corp, removeWords, stopwords("english"))
    corp<-tm_map(corp, content_transformer(tolower))
#    corp<-tm_map(corp, removePunctuation)
#    corp<-tm_map(corp, removeNumbers)
    corp<-tm_map(corp, function(x) gsub("[^a-zA-Z ]", "", x))  #removes junk characters
#    corp<-tm_map(corp, function(x) iconv(x, "latin1", "ASCII", sub=""))  #removes junk characters
    corp<-tm_map(corp, PlainTextDocument)

#    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
#    unigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))
    
#     Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#     bigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))
#     df2<-gramalyzer(bigram)
#     write.table(df2[1:70000,], paste("df2_", i,".txt", sep=""))
#     bigram<-NULL
#     df2<-NULL
#     
#     Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#     trigram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))
#     df3<-gramalyzer(trigram)
#     write.table(df3[1:70000,], paste("df3_", i,".txt", sep=""))
#     trigram<-NULL
#     df3<-NULL
#     
#     Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
#     quadgram<-TermDocumentMatrix(corp, control = list(tokenize = Tokenizer))
#     df4<-gramalyzer(quadgram)
#     write.table(df4[1:70000,], paste("df4_", i,".txt", sep=""))
#     df4<-NULL
#     quadgram<-NULL
#     
#    quadgram<-NULL

     maketable(1)
#     maketable(2)
#     maketable(3)
#     maketable(4)
#     maketable(5)
#     maketable(6)
    
    #quadgram<-NULL

}




buildtable<-function(stem){
    tbl<-read.table(paste(stem, 1, ".txt", sep=""))
    #fix the stop at 3 affter testing!!!!
    for(j in 2:chunks){
        tbl_n<-read.table(paste(stem, j, ".txt", sep=""))
        
        tbl<-join(tbl, tbl_n, type="full")
        tbl<-aggregate(Frequency~Phrase, FUN=sum, data=tbl)
        
        dim(tbl)
    }
    tbl<-arrange(tbl, desc(Frequency))
    
    tbl
}

unitbl<-buildtable("df1_")
write.table(unitbl, "unitbl.txt")


tritbl<-buildtable("df3_")
write.table(tritbl, "tritbl.txt")

bitbl<-buildtable("df2_")
write.table(bitbl, "bitbl.txt")

quadtbl<-buildtable("df4_")
write.table(quadtbl, "quadtbl.txt")

fivetbl<-buildtable("df5_")
write.table(fivetbl, "fivetbl.txt")

sixtbl<-buildtable("df6_")
write.table(sixtbl, "sixtbl.txt")

quadtbl<-read.table("quadtbl.txt")

prepare<-function(input){
    input<-gsub("[^a-zA-Z ]", "", input)
    input<-tolower(input)
    input<-paste("^",input, sep="")
    
 #   print(input)
}

predict<-function(input){
    input<-prepare(input)
    wordcount<-sapply(gregexpr("\\W+", input), length)

    result<-grepl(input,quadtbl$Phrase, ignore.case=TRUE)
    answer<-quadtbl[result,][1:10,]
    nextword<-gsub(input, "", answer$Phrase)
    #    print(nextword)
    print(answer)

    result<-grepl(input,tritbl$Phrase, ignore.case=TRUE)
    answer<-tritbl[result,][1:10,]
    nextword<-gsub(input, "", answer$Phrase)
    #    print(nextword)
    print(answer)
}


found<-FALSE

while(found==FALSE){
    result<-grepl(input,quadtbl$Phrase, ignore.case=TRUE)
    if(sum(result>0)){
        found<-TRUE
        answer<-quadtbl[result,][1,]
        nextword<-gsub(input, "", answer$Phrase)
        
    }    
}

# result2<-grepl(input,bitbl$Phrase, ignore.case=TRUE)
# result3<-grepl(input,tritbl$Phrase, ignore.case=TRUE)
# result4<-grepl(input,quadtbl$Phrase, ignore.case=TRUE)
# result<-cbind(result2, result3, result4)
# colnames(result)<-c('Bi', 'Tri', 'Quad')
# result

dim(df4)

df1_4<-read.table("df1_4.txt")
df4_2<-read.table("df1_4.txt")
