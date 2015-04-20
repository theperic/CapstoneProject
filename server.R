
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
    ntries<-0
    values <- reactiveValues(ntries = 0,ncorrect=0)
    #ncorrect<-0
    
    
    
    observeEvent(input$predictnow, {
    
        wordcount <- length(strsplit(gsub(' {2,}',' ',input$iText),' ')[[1]])
        
        
        
        
 #       output$nextword<- renderText(topwords[sample(2:1000,1),1])
        output$nextword<- renderText(textpredict(input$iText))
 
        isolate({
            values$ntries <- values$ntries + 1
        })
        
        output$tally<- renderText(c(values$ncorrect,"/",values$ntries))
        
        
    
    })
    observeEvent(input$Scorefeedback, {
        
        
        values$ncorrect <- values$ncorrect + as.numeric(input$Accuracy)
        
        output$tally<- renderText(c(values$ncorrect,"/",values$ntries))
        
        
        
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
    nextword    
}
