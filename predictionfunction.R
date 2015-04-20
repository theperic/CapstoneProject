library(stringr)

tbl_6<-read.table("sixtbl.txt",  colClasses=c("Phrase"="character"))
tbl_5<-read.table("fivetbl.txt",  colClasses=c("Phrase"="character"))
tbl_4<-read.table("quadtbl.txt",  colClasses=c("Phrase"="character"))
tbl_3<-read.table("tritbl.txt",  colClasses=c("Phrase"="character"))
tbl_2<-read.table("bitbl.txt",  colClasses=c("Phrase"="character"))

tbl_6<-cbind(tbl_6, set=6)
tbl_5<-cbind(tbl_5, set=5)
tbl_4<-cbind(tbl_4, set=4)
tbl_3<-cbind(tbl_3, set=3)
tbl_2<-cbind(tbl_2, set=2)

bigtbl<-rbind(tbl_6,tbl_5,tbl_4,tbl_3,tbl_2)
write.table(bigtbl, "bigtbl.txt")



texttrim<-function(phrase, newlen){
    spacestr<-gsub(' {2,}',' ',phrase)
    wordcount <- length(strsplit(spacestr,' ')[[1]])    
        while (wordcount>newlen){
            
            spacestr<-substr(spacestr, str_locate(spacestr, " ")[1]+1,200)
            wordcount <- length(strsplit(spacestr,' ')[[1]])
        }
    
    spacestr
    
}

prepare<-function(input){
    input<-gsub("[^a-zA-Z ]", "", input)
    input<-tolower(input)
    input<-paste("^",input, sep="")
    
    #   print(input)
}


textpredict<-function(text){
    newstr<-gsub(' {2,}',' ',text)
    print(newstr)
    wordcount <- length(strsplit(newstr,' ')[[1]])
    print(wordcount)
    
    foundword<-FALSE
 
    if(wordcount>5){
        targetgram<-5
    } else {
        targetgram<-wordcount
    }
    newstr<-texttrim(newstr,targetgram)
    
        
    while(foundword == FALSE){
            
        input<-prepare(newstr)
        result<-grepl(input,bigtbl[bigtbl$set==targetgram+1,1], ignore.case=TRUE)
        answer<-bigtbl[bigtbl$set==targetgram+1,][result,][1:10,]
        nextword<-texttrim(answer$Phrase, 1)[1]
        
            if(!is.na(nextword)){
                #case word found
                foundword<-TRUE
                print(nextword)
                print(answer)
            } else {
                targetgram<-targetgram-1
                print(targetgram)
                newstr<-texttrim(newstr,targetgram)
                print(newstr)
                
            }
            
            if(targetgram==1){
                #take a semi-random guess
                nextword<-topwords[sample(2:1000,1),1]
                print(nextword)
                print("guessed")
                foundword<-TRUE
                
            }

        
        }
    
}

