# get the data from scopus
## from scopus csv to dataframe
# Authors                     - AU
# Author(s) ID                - AUID
# Title                       - TI
# Year                        - PY
# Source title                - SRC
# Cited by                    - C
# DOI                         - DOI
# Link                        - L
# Affiliations                - AFF
# Authors with affiliations   - AUAFF
#	Abstract                    - AB
#	Author Keywords             - AUKW
#	Index Keywords              - IKW
#	References                  - REF
#	Document Type               - DOCTYPE
#	Publication Stage           - PS
#	Access Type                 - ACCTYPE
#	Source                      - SRCTYPE
#	EID                         - 

## I just want
# Authors                     - AU
# Title                       - TI
# Year                        - PY
# Cited by                    - C
# Affiliations                - AFF
#	Abstract                    - AB
#	Author Keywords             - AUKW
#	Index Keywords              - IKW
#	References                  - REF
#	Document Type               - DOCTYPE

# IMORTANT NOTE TO SELF: the incidence matrices should be logical!! logical! for igraph to understand them. 

library(rapidraker)
library(parallel)
library(doParallel)
library(foreach)
#-------------
# for reports # 
library(xlsx)
#-------------
# for analysis
library(Matrix)
library(igraph)

ScopusCsv2dataframe <- function (fname){
  output <- read.csv(file = fname, header = T, stringsAsFactors = F,encoding = "UTF-8")
  return(output)
}
Format_dataframe <- function (x){
  colnames(x) <- c("AU","AUID","TI","PY","SRC","C","DOI","L","AFF","AUAFF","AB","AUKW","IKW","REF","DOCTYPE"
                   ,"PS","ACCTYPE","SRCTYPE","EID")
  output <- x
  return(output)
}
Subsetting_dataframe <- function(x){
  output <- x[,c("AU","TI","PY","C","AFF","AB","AUKW","IKW","REF","DOCTYPE")]
  return(output)
}

# getting KW datset
Get_nonemptyKW_data <- function(x){
  output <- x[x[,"AUKW"]!="",]
  return(output)
}

# getting AB dataset
Get_nonemptyAB_data <- function(x){
  # 4 words means [no abstract supplied] or empty string
  output <- x[lengths(strsplit(x[,"AB"], " "))>4,]
  return(output)
}

# getting kw.compare
# Keyword processing tools ---------------------
Remove_punctuation <- function(x){
  output <- gsub("[[:punct:]]", "", x)
  return(output)
}

Remove_whitespace <- function(x){
 output <- gsub("[[:space:]]", "", x)
 return(output)
}

Strsplit_unlisted <- function(x,s=""){
  output <- trimws(unlist(strsplit(x,split = s)))
  return(output)
}

Kw_preproc <- function(x){
   output <- paste0(Remove_whitespace(Remove_punctuation(trimws(tolower(x)))), collapse = ";")
   return(output)
}
##-----------------------------------------

Get_AUKW.compare  <- function(x){
  # for each document get keyword cell
  #x <- data.merged[1][[1]]
  # Split the keywords
  AUKW.list <- lapply(x[,"AUKW"],Strsplit_unlisted,s = ";")
  # transform them, paste them together with the ;(semicolon) and deliver the vector
  x$AUKWCOMP <- unlist(lapply(AUKW.list,Kw_preproc))
  output <- x
  return(output)
  #rm(x)
  #rm(AUKW.list)
  #rm(AUKW.list.proccessed)
}
Get_CON <- function(x){
  output <- tolower(x[length(x)])
  return(output)
}
Get_AFF <- function(x){
  # split the info in the affitiliation
  #x <- AFF.list[1][[1]]
  y <- lapply(x,Strsplit_unlisted,s = ",")
  # get the country
  output <- paste0(unlist(lapply(y,Get_CON)),collapse = ";")
  return(output)  
  #rm(x)
  #rm(y)
  #rm(CON.list)
  #rm(CON)
}
Get_countries <- function(x){
  #x <- data.merged[1][[1]]
  # for each element, split the affilitations ;
  AFF.list   <- lapply(x[,"AFF"],Strsplit_unlisted,s = ";")
  #AFF.list[1][[1]]
  CON.vector <- unlist(lapply(AFF.list,Get_AFF))
  x$CON      <- CON.vector
  output <- x
  return(x)
  # split the info in the affitiliation ,
  # get the country
  # paste the countries with ; 
  # deliver the vector country
  #rm(x)
  #rm(AFF.list)
}
#attributes(data.merged[1])

# applying RAKE to get the keywords
Get_ABKW <- function(x){
  # setting up RAKE
  output  <- rapidrake(x[,"AB"])
  # using rake
  return(output)
}
# not working well
Get_ABKW_parell <- function(x){
  cores <- detectCores()-2
  # Make txt vector larger so we can more easily see the speed improvement of parallelization
  y <- x[,"AB"]
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  
  output <- foreach(i = 1:length(y)) %dopar% {
    rapidraker::rapidrake(y[i])
  }
  
  stopCluster(cl)
  return(output)
  
}

p <- "unmerged_data/"
i <- 1:19
Get_data <- function(i,p){
  #p <- "unmerged_data/"
  #i <- 1 
  rawdata                 <- ScopusCsv2dataframe(paste0(p,i,".csv"))
  formated                <- Format_dataframe(rawdata)
  data                    <- Subsetting_dataframe(formated)
  data.kw                 <- Get_countries(Get_AUKW.compare(Get_nonemptyKW_data(data)))
 
  #data.AB                 <- Get_nonemptyAB_data(data.Agritourism)
  save(data.kw,file = paste0("rdata/",i,".rdata"))
  #output                  <- data.kw.tagged
  output                  <- data.kw
  print(i)
  #data.AB.RAKEKw          <- Get_ABKW(data.AB.Agritourism)
  #data.AB.RAKEKw.parallel <- Get_ABKW_parell(data.AB.Agritourism)
  return(output)
}
data        <- lapply(i,Get_data,p)

data.merged <- list(data[1][[1]],
                    data[2][[1]],
                    data[3][[1]],
                    data[4][[1]],
                    data[5][[1]],
                    do.call(rbind,data[6:9]),
                    data[10][[1]],
                    data[11][[1]],
                    data[12][[1]],
                    data[13][[1]],
                    data[14][[1]],
                    data[15][[1]],
                    data[16][[1]],
                    data[17][[1]],
                    do.call(rbind,data[18:19])
                    )
###########################################################################
############################### DATA ANALYSIS #############################
###########################################################################
## SET TAGGS
tags <- c("Agritourism","Agrotourisim","Alternative_Tourisim","Community_Tourisim",
          "Cultural_Tourisim","Ecotourisim","Ethno-Tourism","Experiential_Tourisim",
          "Immersive_Tourisim","Nature_Tourisim","Responsible_Tourisim","Rural_Community_Tourism",
          "Rural_Tourisim","Social_Tourisim","Sustainable_Tourisim")
Set_tag            <- function(x,tag){
  #x[1][[1]]$TI
  #x <- temp
  #i <- 1
  #print(tag)
  tag.vector    <- vector("character",nrow(x))
  tag.vector[]  <- as.character(tag)
  x$TAG  <- tag.vector
  output <- x
  return(output)
  #rm(x)
  #rm(y)
  #rm(tag)
  #rm(output)
}

data.merged.tagged <- mapply(Set_tag,x = data.merged,tags,SIMPLIFY = F)

###########################################################################
############################## REPORTS ####################################
###########################################################################
## Get all the elements per datasdddet
Report_numberOfElements <- function(x){
  output <- nrow(x)
  return(output)
}
## Descriptive of years
Report_PY <- function(x){
  #do a table for the years. 
  output <- table(x[,"PY"])
  return(output)
}
## Descriptive of authors
Report_AU <- function(x){
  #do a table for the years
  # split the autors
  AU <- trimws(unlist(strsplit(x[,"AU"],",")))
  # do a table.
  # report
  output <- table(AU)
  return(output)
}
## Bag of words
Report_wordCloud <- function(x){
  #do a table for the years
  # split the autors
  AUKWCOMP <- trimws(unlist(strsplit(x[,"AUKWCOMP"],";")))
  # do a table.
  # report
  output <- table(AUKWCOMP)
  return(output)
}
## Descriptive of affiliations
 ## PENDING

## Descriptive of countries
Report_countries <- function(x){
  # for each element, split the countries
  CON <- trimws(unlist(strsplit(x[,"CON"],";")))
  # get the country
  # report the country
  output <- table(CON)
  return(output)
}

## Saving the Reports
Report.save <- function(i,E,PY,AU,WC,CON){
  p <- paste0("reports/",i,"/report.xlsx")
 
  write.xlsx(E,   file = p, sheetName = "Elementos", append = FALSE)
  write.xlsx(PY,  file = p, sheetName="Anos", append=TRUE)
  write.xlsx(AU,  file = p, sheetName="Autores", append=TRUE)
  write.xlsx(WC,  file = p, sheetName="palabras", append=TRUE)
  write.xlsx(CON, file = p, sheetName="paises", append=TRUE)
  print(i)
}

data.merged.elements  <- lapply(data.merged, Report_numberOfElements)
data.merged.PY        <- lapply(data.merged, Report_PY)
data.merged.AU        <- lapply(data.merged, Report_AU)
data.merged.wordCloud <- lapply(data.merged, Report_wordCloud)
data.merged.CON       <- lapply(data.merged, Report_countries)

i <- 1:15
mapply(Report.save,i= i,
       E   = data.merged.elements,
       PY  = data.merged.PY,
       AU  = data.merged.AU,
       WC  = data.merged.wordCloud,
       CON = data.merged.CON)

###########################################################################
####################            ANALYSIS             #####################
#################### TAGGIN DATA SET - Interceptions ######################
###########################################################################

Analysis.data <- data.merged.tagged
# identify interceptions, tag interceptions, identify hierarchical dependence.
# BASIC INTERCEPTIONS
# see intercepts on each side with %in %
# update the TAG on each side
# give only the titles vector and the TAG vector
# title vector to compare
# TAG vector to update

GET_data2Intercept     <- function(x){
  output <- x[,c("TI","TAG")]
  return(output)
}
data.intercept         <- lapply(Analysis.data,GET_data2Intercept)
Analysis.intercept     <- function(x){
  #x <- data.intercept
  #i <- 1
  #j <- 2
  
  for (i in 1:length(x)){
    i.tag <- x[i][[1]]$TAG[1]
    #i.index.to.update <- NA
    
    for(j in 1:length(x)){
      j.tag <- x[j][[1]]$TAG[1]
      #j.index.to.update <- NA
      
      if(i!=j){
        # do the interception and tag update
        ## interception
        i.index.to.update <- x[i][[1]]$TI%in%x[j][[1]]$TI
        if(any(i.index.to.update)){# si esa vacia la intercepcion que?
          #do update
          x[i][[1]]$TAG[which(i.index.to.update)] <- paste(x[i][[1]]$TAG[which(i.index.to.update)],j.tag,sep = ";")
        }else{
          # skip update
        }

      }else{
        #skip interception and update 
      }
    }
  }
  output <- x
  return(output)
  #m(i)
  #rm(i.index.to.update)
  #rm(i.tag)
  #rm(index.to.update)
  #rm(j.index.to.update)
  #rm(j)
  #rm(j.tag)
  #rm(x)
}
Analysis.TAG.to.update <- Analysis.intercept(data.intercept)

Mapply.Update_TAG                 <- function(x,uTAG){
  x$TAG <- uTAG$TAG
  output <- x
  return(x)
}
Analysis.data.Tagged              <- mapply(Mapply.Update_TAG,x = Analysis.data,uTAG = Analysis.TAG.to.update,SIMPLIFY = F)
Analysis.data.Tagged.Fused        <- do.call(rbind,Analysis.data.Tagged)
Analysis.data.Tagged.Fused.Unique <- Analysis.data.Tagged.Fused[-which(duplicated(Analysis.data.Tagged.Fused$TI)),]

###########################################################################
####################            ANALYSIS             ######################
################# Adjacency matrix - network of links #####################
###########################################################################

Analysis.data.Tagged.Fused.Unique
nrow(Analysis.data.Tagged.Fused.Unique)
# build the adjancency matrix of docs V TAG
# get the unique TAGS
Analysis.tags <- tags
i <- 1:nrow(Analysis.data.Tagged.Fused.Unique)
# for each document compare the TAGs present
## get the document, disasemble the TAGS and compare them.

Lapply.Get.incidence.Matrix.j <- function(x,t){
 #x    <- Analysis.data.Tagged.Fused.Unique$TAG[1]
 #t    <- Analysis.tags
 x.compare <- Strsplit_unlisted(x,s = ";")
 output  <- which(t%in%x.compare)
 #Analysis.data.Tagged.Fused.Unique$
 return(output)
}
Mapply.Get.incidence.Matrix.i <- function(x,i){
  #x    <- Analysis.data.Tagged.Fused.Unique$TAG[1]
  #t    <- Analysis.tags
  x.output   <- vector("numeric",length(x))
  x.output[] <- i
  output     <- x.output
  #Analysis.data.Tagged.Fused.Unique$
  return(output)
}

Analysis.incidence.Matrix.j <- lapply(Analysis.data.Tagged.Fused.Unique$TAG,Lapply.Get.incidence.Matrix.j, t = Analysis.tags)

Analysis.incidence.Matrix.i <- mapply(Mapply.Get.incidence.Matrix.i,x = Analysis.incidence.Matrix.j, i = i,SIMPLIFY = F)
# build adjacency matrix.
Analysis.incidence.Matrix <-  sparseMatrix(i = unlist(Analysis.incidence.Matrix.i), 
                                           j = unlist(Analysis.incidence.Matrix.j),
                                           x = T,
                                           dimnames = list(1:nrow(Analysis.data.Tagged.Fused.Unique),Analysis.tags)
                                           )
Analysis.incidence.Matrix*1
write.csv(as.matrix(Analysis.incidence.Matrix*1),file = "incidence_matrix.csv")
Analysis.g <- graph_from_incidence_matrix(Analysis.incidence.Matrix)
## set the node properties

Analysis.g.tagnodes.index <- grep("[a-z,A-Z]",names(V(Analysis.g)))
Analysis.g.docnodes.index <- grep("[0-9]+",names(V(Analysis.g)))

V(Analysis.g)$nt     <- vector("character",length = length(V(Analysis.g)))
V(Analysis.g)$docti  <- vector("character",length = length(V(Analysis.g)))
V(Analysis.g)$docabs <- vector("character",length = length(V(Analysis.g)))
V(Analysis.g)$docpy  <- vector("character",length = length(V(Analysis.g)))
V(Analysis.g)$docc   <- vector("character",length = length(V(Analysis.g)))

## filling the properties of the nodes
V(Analysis.g)$nt[Analysis.g.docnodes.index] <- "DOC"
V(Analysis.g)$nt[Analysis.g.tagnodes.index] <- "TAG"

V(Analysis.g)$docti[Analysis.g.docnodes.index] <- Analysis.data.Tagged.Fused.Unique$TI
V(Analysis.g)$docti[Analysis.g.tagnodes.index] <- "NA"

V(Analysis.g)$docabs[Analysis.g.docnodes.index] <- Analysis.data.Tagged.Fused.Unique$AB
V(Analysis.g)$docabs[Analysis.g.tagnodes.index] <- "NA"

V(Analysis.g)$docpy[Analysis.g.docnodes.index] <- Analysis.data.Tagged.Fused.Unique$PY 
V(Analysis.g)$docpy[Analysis.g.tagnodes.index] <- "NA"

V(Analysis.g)$docpy[Analysis.g.docnodes.index] <- Analysis.data.Tagged.Fused.Unique$PY 
V(Analysis.g)$docpy[Analysis.g.tagnodes.index] <- "NA"

V(Analysis.g)$docc[Analysis.g.docnodes.index] <- Analysis.data.Tagged.Fused.Unique$C 
V(Analysis.g)$docc[Analysis.g.tagnodes.index] <- "NA"
# export to gephi

Analysis.g.np<-cbind(as.character(V(Analysis.g)$name),#ID-1
          V(Analysis.g)$name,# label-2
          V(Analysis.g)$nt,
          V(Analysis.g)$docti,
          V(Analysis.g)$docabs,
          V(Analysis.g)$docpy,
          V(Analysis.g)$docc
          )
colnames(Analysis.g.np) <- c("Id",#-1
                "Label",#-2
                "N_type[z]",#-3
                "ti",#-4
                "ab",#-5
                "py",#-6
                "c")#
save(Analysis.g.np,file=paste0("multimode-network/np.RData"))
write.csv(Analysis.g.np,file=paste0("multimode-network/NOP.csv"))

Analysis.g.eg.edgelist <- get.edgelist(Analysis.g, names=TRUE)# getting the Edgelist

eT<-vector(mode = "character",length=nrow(Analysis.g.eg))
eT[]<-"Undirected"

Analysis.g.eg.type <- eT

Analysis.g.eg <- cbind(Analysis.g.eg.edgelist,Analysis.g.eg.type)
colnames(Analysis.g.eg) <- c("Source","Target","Type")

write.csv(Analysis.g.eg,file=paste0("multimode-network/EOP.csv"),row.names=FALSE)



 
# 