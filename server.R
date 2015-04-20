
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)



topwords<-read.table("unitbl.txt",  colClasses=c("Phrase"="character"))
topwords<-topwords[1:1000,]
bigtbl<-read.table("bigtbl.txt",  colClasses=c("Phrase"="character"))

set.seed(4150)

shinyServer(function(input, output) {
#    output$nextword<- renderText("Bugaboo")
    values <- reactiveValues(ncorrect=0, nwrong=0)
    
    observeEvent(input$predictnow, {
    
        nextword<-isolate(textpredict(input$iText))
        output$nextword<- renderText(nextword)
        
    })
    observeEvent(input$Scorefeedback, {
        
        
        values$ncorrect <- values$ncorrect + as.numeric(input$Accuracy)
        values$nwrong <- values$nwrong + !as.numeric(input$Accuracy)
        
        output$tally<- renderText(c(values$ncorrect,"/",values$ncorrect+values$nwrong))
        
    })
    
    
    
})


#functions

texttrim<-function(phrase, newlen){
    spacestr<-gsub(' {2,}',' ',phrase)
    wordcount <- length(strsplit(spacestr,' ')[[1]])    
    while (wordcount>newlen){
        
        spacestr<-substr(spacestr, str_locate(spacestr, " ")[1]+1,200)
        wordcount <- length(strsplit(spacestr,' ')[[1]])
    }
    
    spacestr
    
}

prepare<-function(myinput){
    myinput<-gsub("[^a-zA-Z ]", "", myinput)
    myinput<-tolower(myinput)
    myinput<-paste("^",myinput, sep="")
    
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
        
        newinput<-prepare(newstr)
        result<-grepl(newinput,bigtbl[bigtbl$set==targetgram+1,1], ignore.case=TRUE)
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
    nextword    
}
