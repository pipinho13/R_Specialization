library(readr)
library(magrittr)
library(tidyr)


###Read the Data 
data <- read_csv("MIE.csv")


###Create a function which converts the data to a LongitudinalData class




make_LD<-function(dataset) {
  
  structure(list(
    df=dataset,
    id=unique(dataset$id),
    visit=unique(dataset$visit),
    room=unique(dataset$room)
    
    
  ),
  class="LongitudinalData")
  
}


###Create a print method to the LongitudinalData class

print.LongitudinalData<-function(x) {
  
  paste("Longitudinal dataset with", length(unique(x$id)), "subjects")
}




####Create the Generic functions which filter the dataset and return the unique values subject, visits and room respectively
#subject
subject <- function(x,y) UseMethod("subject")

subject.LongitudinalData <- function(x,y){
  data <- filter(x$df, id == y)
  output <-structure(list(
    data = data, id = y),
    class = "subject")
  output
 
}




#visit
visit <- function(x,y) UseMethod("visit")

visit.subject <- function(x,y){
  data <- filter(x$data, visit == y)
  output <-structure(list(
    data = data, id = x$id, visit = y),
    class = "visit")
  output
  
}




#room
room <- function(x,y) UseMethod("room")

room.visit <- function(x,y){
  data <- filter(x$data, room == y)
  output <-structure(list(
    data = data, id = x$id, visit = x$visit, room=y),
    class = "room")
  output
  
}



#####Print methods 

#Print method for subject
print.subject<-function(x) {
  
  if(dim(x$data)[1]<1) {
    cat("NULL")
  }
  else {
    paste("Subject ID:", x$id) }
}




#Print method for visit
print.visit<-function(x) {
  
  if(dim(x$data)[1]<1) {
    cat("NULL")
  }
  else
  {
      cat(paste("Subject ID:", x$id), "\n", paste("Visit:", x$visit))
    }
}




#Print method for room
print.room<-function(x) {
  
  if(dim(x$data)[1]<1) {
    cat("NULL")
  }
  else
  {
    cat(paste("Subject ID:", x$id), "\n", paste("Visit:", x$visit), "\n", paste("room:", x$room))
  }
}


####summary methods


#for the subject
summary.subject<-function(x) {
  
  if(dim(x$data)[1]<1){return(NULL)}
  
  summary_output<-x$data%>%group_by(id, visit, room)%>%summarize(v=mean(value))%>%spread(room, v)
  output<-structure(list(
    summary_output=summary_output,
    id=x$id),
    
    class="subject_summary"
    
  )
  
}


#for the room

summary.room<-function(x) {
  
  if(dim(x$data)[1]<1){return(NULL)}
  summary_output<-summary(x$data$value)
  
  output<-structure(list(
    summary_output=summary_output,
    id=x$id),
    
    class="room_summary"
    
  )
  
}



####Print methods for summary classes

print.subject_summary<-function(x) {
  
      print(paste("ID:", x$id))
      print(x$summary_output)
  
}



print.room_summary<-function(x) {
  
  print(paste("ID:", x$id))
  print(x$summary_output)
  
}

