#---
#LIBRARIES
#---
library(data.table)
library(stringr)
library(foreach)

#---
#FUNCs
#---
pipesplit <- function(strg) {str_split(strg,"\\|")[[1]]}


#---
#DATA
#---
setwd("~/R/MetObjectsTask")
dta <- fread("MetObjects.csv")

#---
#Just looking inside...
#---
colnames(dta)

###getting the "class" of the objects
class <- sapply(dta$Classification,pipesplit)
class_table <- sort(table(unlist(class)),decreasing = TRUE)
head(class_table,15)

###Which objects are photos (or negs)
IsPhoto <- sapply(class, function(cl) {"Photographs" %in% cl | "Negatives" %in% cl })
dta2 <- dta[IsPhoto,] #new data on Photos (or Negs) only

### getting the tags
all_tags   <- sapply(dta$Tags,pipesplit)
tags_table <- sort(table(unlist(all_tags)),decreasing=TRUE)

###most frequent tags
head(tags_table,20)

###unique tags, excluding the empty tag ""
uni_tags <- names(tags_table)[-1]
tag_freq <- as.numeric(tags_table)[-1]

###same tags for photos
photo_tags   <- sapply(dta2$Tags,pipesplit)
photo_tags_table <- sort(table(unlist(photo_tags)),decreasing=TRUE)
uni_photo_tags <- names(photo_tags_table)[-1] 
photo_tag_freq <- as.numeric(photo_tags_table)[-1]

head(photo_tags_table,20)

###Which objects are being exposed in one of the Galleries
IsExposed  <- dta$`Gallery Number`!=""
IsExposed2 <- dta2$`Gallery Number`!=""

###Diffs in tags of exposed and not exposed photos
photo_tags_ex1   <- sapply(dta2$Tags[IsExposed2],pipesplit)
photo_tags_ex0   <- sapply(dta2$Tags[!IsExposed2],pipesplit)

photo_tags_ex1_table <- sort(table(unlist(photo_tags_ex1)),decreasing=TRUE)
photo_tags_ex0_table <- sort(table(unlist(photo_tags_ex0)),decreasing=TRUE)

photo_tags_ex1_table[-1]
head(photo_tags_ex0_table,20)[-1]

###Diffs in tags of exposed and non-exposed objects of all classes
tags_ex1   <- sapply(dta$Tags[IsExposed],pipesplit)
tags_ex0   <- sapply(dta$Tags[!IsExposed],pipesplit)

tags_ex1_table <- sort(table(unlist(tags_ex1)),decreasing=TRUE)
tags_ex0_table <- sort(table(unlist(tags_ex0)),decreasing=TRUE)

head(tags_ex1_table,20)[-1]
head(tags_ex0_table,50)[-1]

###Diffs of freq for each tag among exposed and not esxposed objects
Tags_by_Exposed <- foreach(tag = uni_tags, .combine=rbind) %do% {
  data.frame(InExposed    = sum(sapply(tags_ex1, function(tags) {tag %in% tags}))/sum(IsExposed),
             InNotExposed = sum(sapply(tags_ex0, function(tags) {tag %in% tags}))/sum(!IsExposed))
}
uni_tags[order(Tags_by_Exposed[,1]-Tags_by_Exposed[,2],decreasing=TRUE)[1:20]] 
uni_tags[order(Tags_by_Exposed[,1]-Tags_by_Exposed[,2],decreasing=FALSE)[1:20]]



###How the tags are combined together
tags100 <- uni_tags[order(Tags_by_Exposed[,1]-Tags_by_Exposed[,2],decreasing=TRUE)[1:100]] 

tags100df <- foreach(tag=tags100,.combine=cbind) %do% {
  as.numeric(sapply(all_tags, function(tags) {tag %in% tags}))
}

tags100km <- kmeans(tags100df, centers=5)

tags100[tags100km$centers[1,]>0.0025]
tags100[tags100km$centers[2,]>0.0025]
tags100[tags100km$centers[3,]>0.0025]
tags100[tags100km$centers[4,]>0.005]
tags100[tags100km$centers[5,]>0.005]
