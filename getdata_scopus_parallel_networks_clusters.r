
library(httr)
library(jsonlite)
#install.packages("igraph")
#install.packages("linkcomm")
#library(linkcomm)
library(igraph)
library('Matrix')
library("foreach")
library("doParallel")
#
# Data gathering issues
# Sequencially get all the search results (max per page 200)
## solved with the function
# get from the Json object the needed pieces
## solved identifying the json structure
# The output object is given as a list, we need a data frame. to save it
## soved by unlisting the items
# By unlisting we are losing the Null values
## solved by null2na function
# problem(SOLVED): the rbind in scopusSearch function, creating the output did not work
## solved: changed to c, concatenate, becasue it is a vector not a list or dataframe. 
# PROBLEM: vector index should be given in numeric, or else it thinks those are names....
# match solved the exact keywords matching.
# PROBLEM match only gives the first match.... 

# 10/21/2015
# Problem(SOLVED) object dimentions of affiliation was not the same,
# Solved:a special function was needed to format affiliation data into R. 
# 1)Divide by pieces, 2)then stick back as a string and 3) create the character vector per article
# Problem(SOLVED) with the function parameters, just give the function all the paramaters.
# Problem(SOLVED) with NA on getting affiliation names, the object in the input was already a NULL
# solved: making NA if the object given is already a NULL.
# Next step: elimnate NA from affiliation and afiliation country and work with those keywords. 
# 11-04-2015
# PROBLEM(SOLVED), API has a limit of 5000 documents, i have to split the query when bigger to 5000.

# 11-11-2015, still stoped with the reverse vector... but i saw i can also filter by subject area
# which extend the numbers of API queries per year per page by the number of scientific areas
# you would like to explore, but makes the number of results more lesser. 
# SOLVED: using title as a sort index it worked smoothly. 

# PROBLEM: native regitries from the DB are repeated, to check on these we have to standardize
# all the fields of the raw data base(SOLVED).


# 11-13-2015
# PROBLEM(SOLVED): some registries might not have the fields i am asking for...
# So i should first check all the fields and if they are there get them, if not fill
# them with NA
# SOLVED: by askng directly if the field exists in the Json object if not then assign a NA
# PROBLEM(SOLVED): Articles have different subject fields assigned, so i am getting repeated registries
# Sort the main vector into title. 
# create a function to detect repeated titles... then divide the main vector
# we have to combine (repeated title) and left alone (non repetated)
# Create a function to combine the repeated registries, comparing their variables.

#11-15-2015
#Problem: check docvsKw for the role of match, it might be working in a non desired way. 
#PROBLEM(SOLVED): for loops on matrix do not use length (vector).. use nrow... :S

# WISH(SOLVED): been able to divide the query by topic fields.
# WISH(SOLVED): i also can do enrichment by using Scopus defined subjects for each paper. but then i would have to make a search for each one of the topics 
# and then reorganize by date in the raw data base. 
# we can check with the 4-6 most realted.
# Query cost:  27 subject X per year X  per page - 200 per query. 
# LIMIT of request per week 20.000

# PROBLEM(TEMP SOLVED): native regitries from the DB are repeated, to check on these we have to standardize
# all the fields of the raw data base.
# Solution: function to follow up the natural repeated registries
# PROBLEM(SOLVED): vector index should be given in numeric, or else it thinks those are names....
# match solved the exact keywords matching.
# Solution: using as.numeric

#11-16-2015
#PROBLEM(SOLVED): the registry count for 2011 with combination and without combination did not match
#SOLVED: had to add MULT to the remaining subject field. 

#11-19-2015
# Issue(SOLVED): I have repeated subject fields and country affiliations... we count them only once in our analysis
# so we use them only as indication of the interaction, not its intensity. and avoid the problem of 
# natural repeated registries.
#SOLVED: using lapply and unlist

#11-20-2015
#ISSUE: create for the analysis the Keyword meta-data enriched db. subject and country

#11-22-2015
#ISSUE(SOLVED): regular expressions to detect the keywords 
# PROBLEM(SOLVED) match only gives the first match.... 

#11-23-2015
#VISUALIZATION
# 1)Create 2 more tables (SOLVED), kwVsSub and kwVsCount for each keyword show how many times each country 
# or each subject is present that will allow to build histograms and profiles of country 
# and subject for each keyword, clusters and even the network
# 2)add to the Network data the time lines so you can see the dynamic network 
# 2.1)(SOLVED)Creating a good graphs for the SUBJECT and COUNTRY profiles
# 2.2)Apply it to NETWORK (SOLVED)
# 2.3)CLUSTER (SOLVED) pending test with other years


#11-25-2015
# 2.4)word level - connect the cluster with the papers(SOLVED)

#11-27-2015
# Sensei feedback
# Make sense of the cluster....
# 1) Gather the papers touched by each cluster (SOLVED)
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.

#12-13-2015
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.

#12-15-2015
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.

#12-17-2015
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.

#12-18-2015
# PROBLEM ESCAPE REGEX characters... in the keywords... (SOLVED)
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.(WORKING)

#12-22-2015
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.(WORKING)
# - Obtained the graph per cluster (WORKING) line 1007 
# - Node properties per cluster    (WORKING)

#01-05-2016
# 2) find a way to detect patterns in them. (WORKING)
# 3) Find a nice way to show the network and the histogram together.(WORKING)
# - Obtained the graph per cluster (SOLVED)
# - Node properties per cluster    (SOLVED)
# - Analysis test, the cluster size increase by using all keywords vs top 10 (SOLVED)
# - - the number of papers increases but the importance of keywords still the same.
# 4) Generate for each cluster a node list and edge list to export to gephi
# - ReportCg on work
# 5) Filter by ASIA

#01-06-2016
# 4) Generate for each cluster a node list and edge list to export to gephi(SOLVED)
# - ReportCg on work on line 1124(SOLVED)

#01-07-2016
# 5) Filter by ASIA

#01-08-2016
#Analysis issues
#only the 10 top central keywords and their related papers were
#used to fix a name for the cluster GetTitles function
#IMPROVEMENT: use the cluster top 10 words to GeTitles and define the cluster's name

# Data pre-processing issues
# PROBLEM: also some words like bigdata and big data are being detected as different (SOLVED).
# SOLUTION?:using reg expressions, when finding special characters? best shot, .(DOT)
# SOLUTION?:elminating special characters when doing the comparison of word-co ocurrence (SOLVED)
# SOLUTION: implementation of remove white space and special characters and then compare (SOLVED)
## did not implement stemming:
## it present problem with the abbreviations 
## some words like analytic and analytics or evaluation and evaluations might acutally mean something different

# Main Db issues
# PROBLEM: in the main DB keyword recovery. detecting; in the keyword field(SOLVED)

#01-08-2016
#Analysis issues
#only the 10 top central keywords and their related papers were
#used to fix a name for the cluster GetTitles function
#IMPROVEMENT: use the cluster top 10 words to GeTitles and define the cluster's name
#PROBLEM:  if the betweeness is cero, #(SOLVED) less than 3 sized netowrks are set to 0 in their edge betweness

#PROBLEM: all keyword comparison have to be re-worked, solution create 
###TO UPDATE THESE FUNCTIONS THE MODIFIED KEWYWORD FIELD IS REQUIERED TO SIMPLIFY comparisons
###working line 703
##in accs a new field with the modified keywords (SOLVED)

#01-09-2016
#Analysis issues
## using the 10 edgebetweness central words to define cluster names. 
## Countries are taken for each author's country. 
# Main Db issues
# PROBLEM: in the main DB keyword recovery. detecting; in the keyword field(SOLVED)
# NOTE:  in titles "RETRACTED ARTICLE" word: take care have to eliminate it
# NOTE: <sup> </sup> found these html envelopers so we can get them out.

#01-10-2016
#Analysis issues
## using the 10 edgebetweness central words to define cluster names. 
## Countries are taken for each author's country. 
## I was eliminating the big data word... is not good for analysis

# Main Db issues
# NOTE:  in titles "RETRACTED ARTICLE" word: take care have to eliminate it(Working)
# ISSUE: # issue not workin yet, not matching the string
# NOTE: <sup> </sup> found these html envelopers so we can get them out.

# RE-CLUSTER(WISH):for big clusters of keywords, do another loop of clustering. 

#for the next version
# 1) EFFICIENCY: UPDATE DB after keyword comparison
# After you find the transformed keywords, you could change them all to a single word in the main DB 
# like that you do not have to compare or search for uniques anymore...
# 2) CLUSTER ANALYSIS: Comparison between clusters, for each cluster
# keywords, mark their relation with previous years clusters
# this would help con the incremental analysis of cluster, by bypassing already known keywords.

# 03/10/2016
##------------------Analysis issues---------------
## using the 10 edgebetweness central words to define cluster names. 
## Countries are taken for each author's country. 
## I was eliminating the big data word... is not good for analysis
##------------------CURRENT WORK-------------------
# fusing corpus (abstracts) word analysis to the cluster profile

## IN TIME KEYWORD CLUSTER ANALYSIS

#  give a name and a nickname to the clusters
## REAL DEAL
## do the big table, KW vs MD vs Year
## define core capabilites as keywords... who they are, the most central?
## follow how did they move between clusters in time
## which dissapeared
## which stayed
## which emerged
## was it convergence or divergence and how both phenomenas were important. 
## do these general all data base
## then filter for asian countries and compare. 

##------------------FUTURE WORK--------------------
# Main Db issues
# NOTE:  in titles "RETRACTED ARTICLE" word: take care have to eliminate it(Working)
# ISSUE: # issue not workin yet, not matching the string
# NOTE: <sup> </sup> found these html envelopers so we can get them out.

# RE-CLUSTER(WISH):for big clusters of keywords, do another loop of clustering. 

# For the next version
# 1) EFFICIENCY: UPDATE DB after keyword comparison
# After you find the transformed keywords, you could change them all to a single word in the main DB 
# like that you do not have to compare or search for uniques anymore...
# 2) CLUSTER ANALYSIS: Comparison between clusters, for each cluster
# keywords, mark their relation with previous years clusters
# this would help con the incremental analysis of cluster, by bypassing already known keywords.
##-------------------------------------------------------------------------------------
null2na<- function(input){# function to transfor NULL to NA to keep them after unlisting
    if(is.null(input)){return(NA)} #if the vector is already NULL no need to look for its values. 
    input[unlist(lapply(input, is.null))] <- NA
    output<-unlist(input)
    return(output)
}
serialChar<-function(input){
    x<-null2na(input)
    if(length(x)<=1){
        if(is.na(x)){return(NA)}
    }# do not serialize if they are NA values. 
    output<-gsub(",","|",toString(x),fixed=TRUE)
    return(output)
} # to get keywords field
getAffilname<-function(x){
    #if(is.na(x)){return(NA)}
    output<-NULL
    for(i in 1:length(x)){
        buff<-serialChar(null2na(x[[i]]$'affilname'))
        output<-c(output,buff)
    }
    return(output)
}   # to get affil field
getAffilcountry<-function(x){
    #if(is.na(x)){return(NA)}
    output<-NULL
    for(i in 1:length(x)){
        buff<-serialChar(null2na(x[[i]]$'affiliation-country'))
        output<-c(output,buff)
    }
    return(output)
}# to get country field
getAuthor_fullname<-function(x){
    #buff<-json3$`search-results`$entry$author[1][[1]]
    fullname<-paste(x$`given-name`,x$surname)
    u_fullname<-unique(fullname)
    f_fullname<-paste(u_fullname,collapse = " | ")
    output<-f_fullname
    return(output)
}# to get the author sur and last name
getAuthor_stdname<-function(x){
    #buff<-json3$`search-results`$entry$author[1][[1]]
    fullname<-x$`authname`
    u_fullname<-unique(fullname)
    f_fullname<-paste(u_fullname,collapse = " | ")
    output<-f_fullname
    return(output)
}

getAuthorname<-function(x){
    output<-NULL
    #output<-do.call(rbind,lapply(x,getAuthor_fullname))
    output<-do.call(rbind,lapply(x,getAuthor_stdname))
    return(output)
} # to get the author name field

getnoNA<-function(x){output <- x[complete.cases(x),]}#Getting the non NA KW fields
getnoNAKW<-function(x){output <- x[!is.na(x[,"outputKw"]),]}#Getting the non NA KW fields

kwRecovery<-function(accs){
    log<-accs[grep(";",accs[,"outputKw"]),"outputKw"]#who were changed
    accs[,"outputKw"]<-gsub(";"," |",accs[,"outputKw"])
    output<-accs
}#Recovering keywords from bad processed registries in the main Db
rmRetracted<-function(accs){# issue not workin yet, not matching the string
    buff<-grepl("RETRACTED ARTICLE",prePmix(accs[,"outputTitle"]),ignore.case = T)# the non retracted
    if(length(buff)!=0){
        if(any(buff)){
            warning("detected retracted")
            accs<-accs[!buff,]
        }else{
            
        }
    }else{
        warning ("check the vector")#did not find any match
    }
    output<-accs
    return(output)
}#forget retracted articles from the DB 
# one article in 2010

#Word preprocessing functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rmSpace<-function(x){gsub("\\s", "", x)}
rmPunct<-function(x){gsub("[[:punct:]]","",x)}
prePmix<-function(x){rmPunct(rmSpace(trim(tolower(x))))}
all_duplicated<-function(vec){
    duplicated(vec) | duplicated(vec, fromLast=TRUE)
}
# taken from stack overflow

#------------------------------------------------------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#This girl gives me the other set of search results missing from the first call
# t = total of hits
# p = items per page
# Q = query
# D = year to query for
# K = Api key
# QF= list of fields to query
scopusPage <- function(t=0,p=20,Q="TITLE-ABS-KEY({Big data}) AND (DOCTYPE(ar) OR DOCTYPE(ip) OR DOCTYPE(cp) )"
                       , D="2015",K="",QF="",S=""){ 

        limit<-5000# system limit for the scopus API
        revind<-0
        revmax<-0
        revp<-0
        revorder<-"+title"
        #if(K=="" || Fields==""){return(warning("please insert your API key or the FIELDS"))}
        
        t<-as.numeric(t)
        p<-as.numeric(p)
        #give more
        st=t-p
        
        #calculate the number of times to do the loop
        times<-ceiling(st/p)
        output<-vector("list",times)
        #making a tracking bar
        pb <- winProgressBar(title = "progress bar", min = 0,
                             max = times, width = 300)
        #do the loop
        for(i in 1:times){
            ind<-p*i
            r<-NULL
            # NORMAL VERSION in case of query results bellow the limit
            if(ind+p <= limit){
                r <- GET("http://api.elsevier.com/content/search/scopus",
                         query = list(apiKey=paste(K),
                                      query=paste(Q),
                                      date=paste(D),
                                      field=paste(QF),
                                      start=paste(ind),
                                      #sort="-artnum",
                                      #sort="-relevancy",
                                      sort="-title",
                                      subj=paste(S),
                                      count=paste(p)
                         )
                )
            }else{
                # REVERSE VERSION in case of more than limit elements
                revmax<-t-limit   # adapt maximum index to the reverse vector-15
                revind<-ind-limit # adapt index to the reverse vector 0
                revp<-p           # adapt pagination to the reverse vector-200
                
                if((revind+revp)>(revmax)){# setting a stop to the count 
                    revp<-revmax-revind
                }
                #get the reverse part and continue conting 
                r <- GET("http://api.elsevier.com/content/search/scopus",
                         query = list(apiKey=paste(K),
                                      query=paste(Q),
                                      date=paste(D),
                                      field=paste(QF),
                                      start=paste(as.numeric(revind)),
                                      #sort="+relevancy",
                                      #sort="+artnum",
                                      sort=paste(revorder),
                                      subj=paste(S),
                                      count=paste(as.numeric(revp))
                         )
                )
                
            }
            
            #visualizing the tracking bar
            if(status_code(r)==200){
                warning('The excution is ok" ',status_code(r)," index: ",paste(ind),"/",paste(revind),"total hits: ",paste(t),"/",paste(revmax),"pages: ",paste(p),"/",paste(revp))
            } else if(status_code(r)!=200){
                warning('The excution is wrong" ',status_code(r)," index: ",paste(ind), "total hits: ",paste(t))
            }
            
            setWinProgressBar(pb, i, title=paste( round(i/times*100, 0),
                                                  "% done"))
            #
            buff1=content(r)
            buff3=jsonlite::fromJSON(toJSON(buff1))
            output[i]<-buff3
            #returns a list of all the json objects
        }
    close(pb)
    return(output)
}

#Gets the research results from scopus and put them into a matrix 
#This girl starts the query to the Scopus API
# Q = query
# D = year to query for
# K = Api key
# QF= list of fields to query
#OPTIMIZATION: make the table fields free, and dinamic, according to user's input
scopusSearch<-function(Q="TITLE-ABS-KEY({Big data}) AND (DOCTYPE(ar) OR DOCTYPE(ip) OR DOCTYPE(cp) )",
                        D,K,QF,S=""){
    #Q<-nhk_Q
    #D<-"2000"
    #argQf<-strsplit(QF,split=",",fixed = T)# OPTIMIZATION 
    #Create a vector version of QF to ask about their existence in the result vector
    
    #if(K=="" || QF==""){return(warning("please insert your API key or the FIELDS"))}
    r <- GET("http://api.elsevier.com/content/search/scopus",
             query = list(apiKey=paste(K),
                          query=paste(Q),
                          date=paste(D),
                          field=paste(QF),
                          #sort="-relevancy",
                          #sort="-artnum",
                          sort="-title",
                          subj=paste(S),
                          count="200"
             )
    )
    
    #assign(paste0("V", "1"), 5)# this to assign values to dinamic variables. 
    #paste("v1")<-34
    #if the query returns empty then there is no registries , return a NULL
    json1=content(r)
    json3=jsonlite::fromJSON(toJSON(json1))
    
    total<-as.numeric(json3$`search-results`$`opensearch:totalResults`)
    #PROBLEM: some registries might not have the fields i am asking for...
    # So i should first check all the fields and if they are there get them, if not fill
    # them with NA
    
    
    # if there is no results, do not do anything and return NULL
    if(total>=1){
        #logic comparison
        # i am going to search if the fields i ask came back in the Json object
        # if they do not, place NA if they do take the data. 
        jsonNames<-attributes(json3$`search-results`$entry)[1]
        pag<-as.numeric(json3$`search-results`$`opensearch:itemsPerPage`)
       
        #Start asking if in the coming vector has the field i am requesting, 
        #if not then fill it with a NA
        #does it has title?
        if(grepl("dc:title",jsonNames,fixed =TRUE)){
            outputTitle<-null2na(json3$`search-results`$entry["dc:title"][[1]])    
        }else{
            outputTitle<-NA
        }
        
        if(grepl("dc:description",jsonNames,fixed =TRUE)){
            outputAbs<-null2na(json3$`search-results`$entry["dc:description"][[1]])    
        }else{
            outputAbs<-NA
        }
        
        #grepl("authkeywords",jsonNames,fixed =TRUE)
        if(grepl("authkeywords",jsonNames,fixed =TRUE)){
            outputKw<-null2na(json3$`search-results`$entry["authkeywords"][[1]])
        }else{
            outputKw<-NA
        }
        #outputCited<-null2na(json3$`search-results`$entry["citedby-count"][[1]])
        #outputScore<-null2na(json3$`search-results`$entry["intid"][[1]])
        #outputDOI<-null2na(json3$`search-results`$entry["prism:doi"][[1]])
        #here i need as ouput a char vector not a single string.
        if(grepl("affil",jsonNames,fixed =FALSE)){
            #outputAffname<-getAffilname(json3$`search-results`$entry["affiliation"][[1]])
            outputAffcou<-getAffilcountry(json3$`search-results`$entry["affiliation"][[1]])    
        }else{
            #outputAffname<-NA
            outputAffcou<-NA
        }
        
        if(grepl("author",jsonNames,fixed =TRUE)){
            outputAuthor<-getAuthorname(json3$`search-results`$entry["author"][[1]])
        }else{
            outputAuthor<-NA
        }

        # do we need more pages of data? 
        if(total>pag){
            x<-scopusPage(total,pag,Q,D,K,QF,S)
            for(i in 1:length(x)){
                outputTitle<-c(outputTitle,null2na(x[[i]]$entry["dc:title"][[1]]))
                outputAbs<-c(outputAbs,null2na(x[[i]]$entry["dc:description"][[1]]))
                outputKw<-c(outputKw,null2na(x[[i]]$entry["authkeywords"][[1]]))
                # have to create a function to get the author field
                
                
                #outputCited<-c(outputCited,null2na(x[[i]]$entry["citedby-count"][[1]]))
                #outputScore<-c(outputScore,null2na(x[[i]]$entry["intid"][[1]]))
                #outputDOI<-c(outputDOI,null2na(x[[i]]$entry["prism:doi"][[1]]))
                #outputAffname<-c(outputAffname,
                #                 getAffilname(x[[i]]$entry["affiliation"][[1]]))
                outputAffcou<-c(outputAffcou,getAffilcountry(x[[i]]$entry["affiliation"][[1]]))
                outputAuthor<-c(outputAuthor,getAuthorname(x[[i]]$entry["author"][[1]]))
            }
        }
        #output<-cbind(outputTitle,outputAbs,outputKw,outputCited,outputScore,outputDOI)
        #-------------------------------------------------------------------------------------
        # Create a constant vector of the size of the registries with the constant
        # of their subject
        if(S!=""){
            outputSubject<-vector("character",length(outputTitle))
            outputSubject[]<-S 
            #output<-cbind(outputTitle,outputKw,outputAffname,outputAffcou,outputSubject)
            output<-cbind(outputTitle,outputKw,outputAbs,outputAffcou,outputAuthor,outputSubject)
            colnames(output)<-c("outputTitle","outputKw","outputAbs","outputAffcou","outputAuthor","outputSubject")
        }else{
            #output<-cbind(outputTitle,outputKw,outputAffname,outputAffcou)
            output<-cbind(outputTitle,outputKw,outputAbs,outputAffcou,outputAuthor)
            colnames(output)<-c("outputTitle","outputKw","outputAbs","outputAffcou","outputAuthor")
        }
        
        #-------------------------------------------------------------------------------------
        
    }else{
        output<-NULL
    }
    
    return(output) 
}

#Gets the search results organized by fields and subject
# Q = query
# D = year to query for
# K = Api key
# QF= list of fields to query
# # internal variable SCOPUS subject table.
scopusSSubject<-function(Q="TITLE-ABS-KEY({Big data}) AND (DOCTYPE(ar) OR DOCTYPE(ip) OR DOCTYPE(cp) )",
                         D,K,QF){
    #variable initialization
    #constant for scopus fields
    #AGRI -    Agricultural and Biological Sciences 
    #ARTS -	Arts and Humanities 
    #BIOC -	Biochemistry, Genetics and Molecular Biology 
    #BUSI -	Business, Management and Accounting 
    #CENG -	Chemical Engineering 
    #CHEM -	Chemistry 
    #COMP -	Computer Science 
    #DECI -	Decision Sciences 
    #DENT -	Dentistry 
    #EART -	Earth and Planetary Sciences 
    #ECON -	Economics, Econometrics and Finance 
    #ENER -	Energy 
    #ENGI -	Engineering 
    #ENVI -	Environmental Science 
    #HEAL -	Health Professions 
    #IMMU -	Immunology and Microbiology 
    #MATE -	Materials Science 
    #MATH -	Mathematics 
    #MEDI -	Medicine 
    #NEUR -	Neuroscience 
    #NURS -	Nursing 
    #PHAR -	Pharmacology, Toxicology and Pharmaceutics 
    #PHYS -	Physics and Astronomy 
    #PSYC -	Psychology 
    #SOCI -	Social Sciences 
    #VETE -	Veterinary
    output<-NULL
    scopusSubjects<-c("AGRI","ARTS","BIOC","BUSI","CENG","CHEM","COMP","DECI","DENT",
                      "EART","ECON","ENER","ENGI","ENVI","HEAL","IMMU","MATE","MATH",
                      "MEDI","NEUR","NURS","PHAR","PHYS","PSYC","SOCI","VETE","MULT")
    
    for(i in 1:length(scopusSubjects)){
        S<-scopusSubjects[i]
#---------------------------------------------------------------------------#        
        buff<-scopusSearch(Q,D,K,QF,S)
#---------------------------------------------------------------------------#        
        if(is.null(buff)){# skip
            
        }else{# do the operation
            if(is.null(output)){# for the first result it creates the base of the matrix
                output<-buff
            }else{# for the others 
                output<-rbind(output,buff)
            }
        }
    }
    #if scopusSearch returns NULL i do not add it to the output vector
    return(output)
}

# Tool function to combine registries that have more than subject area.
# Input scopusSubject output - matrix of documents and subject.
# Combine Registry no Repeat
cRnoRepeat<-function(abs){
    #------------------------------------------#
    #Ordering by title saving ties. applying field standardization, for comparison
    tabs<-abs
    tabs[,"outputTitle"]<-trim(tolower(tabs[,"outputTitle"]))
    #------------------------------------------#
    #Then divide the main vector
    tdabs<-tabs[duplicated(tabs[,"outputTitle"]),]
    #ouab<-oabs[!duplicated(oabs[,"outputTitle"]),]
    tduabs<-tdabs[!duplicated(tdabs[,"outputTitle"]),]
    #ind<-sort(grep(tduabs[1,"outputTitle"],tabs[,"outputTitle"],fixed = TRUE))
    #------------------------------------------#
    #Create a function to combine the repeated registries, comparing their variables.
    for(i in 1:nrow(tduabs)){# for each original repeated.
    #search in the full original vector and combining making and exclusive search. 
        ind<-sort(grep(tduabs[i,"outputTitle"],tabs[,"outputTitle"],fixed = TRUE)) # searching repeated in the main vector
        #ind<-sort(grep(tduabs[1,"outputTitle"],tabs[,"outputTitle"],fixed = TRUE)) # searching repeated in the main vector
        #Combining elements on the original matriz
        tabs[ind[1],"outputSubject"]<-paste(tabs[ind,"outputSubject"],collapse= "|")
        #abs[ind[1],"outputSubject"]<-paste(tabs[ind,"outputSubject"],collapse= "|")
    }
    #t is used for comparisons, and get the index corresponding to the repeated fields
    # after getting these we can use them on the original database. 
    
    tuabs<-tabs[!duplicated(tabs[,"outputTitle"]),]# here we depend on that the "duplicate" function
    #only count for repeated so the first occurence is not present, and we are combining
    #the information in the first occurence of the repeated registries. 
    output<-tuabs
    return(output)
}

#------------------------------------------------------------------------------#
#This function is a tool to track down the Database natural duplicates, to be aware of what info
#I might not be tracking.
naturalDuplicates<-function(ab){
    #warning when it is only one repeated registry, it will trow error
    #applying field standardization to apply comparison
    tab<-ab
    tab[,"outputTitle"]<-trim(tolower(tab[,"outputTitle"]))
    
    dtab<-tab[duplicated((tab[,"outputTitle"])),]# lowcase?
    dutab<-dtab[!duplicated((dtab[,"outputTitle"])),]
    ind<-vector("numeric")
    #------------------------------------------#
    #Create a function to combine the repeated registries, comparing their variables.
    for(i in 1:nrow(dutab)){# for each original repeated.
        #search in the full original vector and combining making and exclusive search. 
        buff<-sort(grep(dutab[i,"outputTitle"],tab[,"outputTitle"],fixed = TRUE)) # searching repeated in the main vector
        ind<-c(ind,buff)
    }
    output<-ab[ind,]
    return(output)
}

#------------------------------------------------------------------------------#
#Optimization: i could just pass all the colums i want to eliminate repeated and use the list 
#configuration to separate them 
rmRepeatSubandCou<-function(input){# general
    #get the vector, split it,  
    y<-trim(tolower(unlist(strsplit(input,"|",fixed = TRUE))))
    #and remove duplicates
    yu<-y[!duplicated(y)]
    #then put the vector again
    yup<-paste(yu,collapse = " | ")
    output<-yup
    return(output)
}

rmRSC<-function(absc){
    input<-absc
    affCou<-unlist(lapply(absc[,"outputAffcou"],rmRepeatSubandCou))
    subject<-unlist(lapply(absc[,"outputSubject"],rmRepeatSubandCou))
    input[,"outputAffcou"]<-affCou
    input[,"outputSubject"]<-subject
    output<-input
    return(output)
}

#-----------------------ANALYSIS SECTION----------------------------------

##------------------Calculating the adjacency matrix----------------------
#OPTIMIZATIONS:
#Apply leveinstein distance to reduce repeated words

# a function to detect co-word relationship, create the docVskeyword matrix.
# issues(SOLVED): check match SOLVED:using reg expresions
# OPTIMIZE: check if you can optimize it by removing the loops
# FUSE THE WOS VERSION AND THIS SCOPUS VERSION
# SO WE WORK WITH THE SAME FIELDS AND INFO
#accs<-s16_data
#ac<-s16_data[1:10,]
docvsKw_sparse<-function(ac, rmkey="no"){#ac
    #rmkey="no"
    zltue <- GetKw(ac, rmkey)#all unique keywords
    ## REGULAR  expression proof comparison
    
    #Creating the Doc Vs word matrix
    #number of keywords length(zu)
    #number of docs dim(ac)[1]
    dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
    #h14_dID<-as(h14_dID, "sparseMatrix")
    kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
    # This loop in each document, or element of ac
    #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
    #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
    #all(ylt == yltb)
    for(i in 1:dim(ac)[1]){
        #i=1
        ylt<-prePmix(unlist(strsplit(ac[i,"outputKw"],"|",fixed = T)))#all keywords of a document
        kwv[i]<-fieldMerger(ylt)
        ##This line is for the regular expression matching between keywords.
        #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
        ##-------------------------------------------------------------------
        thing<-vector("numeric")
        #for the keywords of each document search for the matchs
        for(j in 1:length(ylt)){        
            #j=3
            buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
            thing<-c(thing,buff)#the index of the words that are present
        }
        ##-------------------------------------------------------------------
        dvk[i][[1]]<-thing
    }
    output<-list(v=dvk,n=zltue,k=kwv)
    return(output)
    #return(thing)
}
#h14_dID<-as(h14_dID, "sparseMatrix")
docvsKw<-function(ac, rmkey="no"){#ac
    zltue <- GetKw(ac, rmkey)
    ## REGULAR  expression proof comparison
    #ac<-accs zltue
    #ac[,"outputKw"]<-gsub("\\+","(PLUS)",ac[,"outputKw"])#getKw already transformed the + to plus
    #ylt<-trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))
    ####
    #Creating the Doc Vs word matrix
    #number of keywords length(zu)
    #number of docs dim(ac)[1]
    dvk<-matrix(0,nrow = dim(ac)[1], ncol=length(zltue), dimnames = list(c(1:dim(ac)[1]),names(zltue)))
    kwv<-vector("character",dim(ac)[1])
    # This loop in each document, or element of ac
    #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
    #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
    #all(ylt == yltb)
    for(i in 1:dim(ac)[1]){
        ylt<-prePmix(unlist(strsplit(ac[i,"outputKw"],"|",fixed = T)))
        kwv[i]<-fieldMerger(ylt)
        ##This line is for the regular expression matching between keywords.
        #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
        ##-------------------------------------------------------------------
        thing<-vector("numeric")
        #for the keywords of each document search for the matchs
        for(j in 1:length(ylt)){        
            buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
            thing<-c(thing,buff)
        }
        ##-------------------------------------------------------------------
        dvk[i,thing]<-1
    }
    output<-list(v=dvk,n=zltue,k=kwv)
    return(output)
    #return(thing)
}

format_keyword_field<-function(kw_f){
    if(kw_f==""){ # to take into account those docs that do not have one ID or DE
        y<-kw_f
    }else{
        x<-fieldSplitter(kw_f)
        y<-prePmix(x)
    }
    output<-y
    return(output)
}
get_dvkw_WOS<-function(v_kw_f){
    #v_kw_f<-doc_kws[,"ID"]
    ## get all Keywords
    ## apply premix
    v_keywords<-unlist(lapply(v_kw_f,format_keyword_field))
    ## get uniques
    u_v_keywords<-unique(v_keywords)
    ## create the buffer matrix [row docs, col unique kw]
    b_matrix<-matrix(0,nrow=length(v_kw_f),ncol=length(u_v_keywords),dimnames=list(1:length(v_kw_f),u_v_keywords))
    for(i in 1:length(v_kw_f)){
        # get the keywords
        # premix them
        x1<-format_keyword_field(v_kw_f[i])
        # check positions of who is in who
        indexs<-which(u_v_keywords%in%x1)
        # mark these positions into the big matrix
        b_matrix[i,indexs]<-1
    }
    output<-b_matrix
    return(output)
}


#---------------These functions Depend on ad----------------------


##-----------KEYWORDS General characteristics----------------##
#function to set the country and subject to each keyword

##TO UPDATE THESE FUNCTIONS THE MODIFIED KEWYWORD FIELD IS REQUIERED TO SIMPLIFY comparisons

getkwProp<-function(adopn,accsk){
    #steal the keywords after optimization 
    zltue<-adopn
    ltacKw<-trim(tolower(accsk[,"outputKw"]))
    kwp<-matrix(0,nrow = length(zltue), ncol=2, dimnames = list(zltue,c("outputAffcou","outputSubject")))
    #search which documents they appear 
    for(i in 1:length(zltue)){
        y<-zltue[i]
        thing<-grep(paste0("^",y,"\\s\\||\\|\\s",y,"\\s\\||\\|\\s",y,"$|^",y,"$"), ltacKw)
        kwpCou<-rmRepeatSubandCou(paste(accsk[thing,"outputAffcou"],collapse = " | "))
        kwpSub<-rmRepeatSubandCou(paste(accsk[thing,"outputSubject"],collapse = " | "))
        #concatenate the answer vectors
        
        
        kwp[i,"outputAffcou"]<-kwpCou
        kwp[i,"outputSubject"]<-kwpSub
    }
    #get the country and the subject.
    #and then paste that info to the network nodes
    output <- kwp
    return(output)
}#ac can be accs, accsk
#gets unique countries
getUCoun<-function(accsk){
    z<-unlist(strsplit(accsk[,"outputAffcou"],"|",fixed = TRUE)) # sapply to get the big vector
    zl<-tolower(z)
    zlt<-zl<-trim(zl)
    zltu<-unique(zlt)
    output<-zltu
    return(output)
}

#gets unique subjects
getUSubj<-function(accsk){
    z<-unlist(strsplit(accs[,"outputSubject"],"|",fixed = TRUE)) # sapply to get the big vector
    zl<-tolower(z)
    zlt<-zl<-trim(zl)
    zltu<-unique(zlt)
    output<-zltu
    return(output)
}

#General version of unique field splitter plus retrieval of unique elements
fieldSplitterUnique<-function(x){
    Y<-unlist(strsplit(x,"|",fixed = TRUE))
    Yl<-tolower(Y)
    Ylt<-Yl<-trim(Yl)
    Yltu<-unique(Ylt)
    output<-Yltu
    return(output)
}

#General version of field splitter
fieldSplitter<-function(x){
    Y<-unlist(strsplit(x,"|",fixed = TRUE))
    Yl<-tolower(Y)
    Ylt<-trim(Yl)
    output<-Ylt
    return(output)
}
#helper to get the first elements of a merged vector
#used to get the names of the network nodes
fieldSplitterFirst<-function(x){
    Y<-unlist(strsplit(x,"|",fixed = TRUE))[1]
    Yl<-tolower(Y)
    Ylt<-trim(Yl)
    output<-Ylt
    return(output)
}

GetadNames<-function(cadjm){
    output<-unlist(lapply(cadjm,fieldSplitterFirst))
    return(output)
}
#Merges the field, 
#Input a character vector
fieldMerger<-function(x){
    output<-paste(x,collapse = " | ")
    return(output)
}

#construct the kwvscountry matrix
#length(adopn_s)
getkwVsCoun<-function(adopn,accsk){
    #adopn                     #keywords to be assigned their properties
    #adopn<-adopn_s
    #rm(adopn)
    kw<-accsk[,"comparKw"]    #index that allows me to locate the keywords on all documents
    uCoun<-getUCoun(accsk)
    #a container matrix, keywords Vs countries.
    kwVsCoun<-matrix(0,nrow = length(adopn), ncol=length(uCoun), dimnames = list(adopn,uCoun))
    
    for(i in 1:length(adopn)){
        #i<-1
        y<-adopn[i]#unique keyword
        thing<-grep(paste0("^",y,"\\s\\||\\|\\s",y,"\\s\\||\\|\\s",y,"$|^",y,"$"), kw)#position
        #of the document with the keyword of interest.
        kwpCou<-paste(accsk[thing,"outputAffcou"],collapse = " | ")#all countries belonging to that keyword
        cou<-fieldSplitter(kwpCou)
        #Count each field. 
        couCount<-table(cou)#contingency table to count each element, like the histogram
        #targets the country columns of the putput matrix
        kwVsCoun[i,unlist(attributes(couCount)$dimnames$cou)]<-couCount
        
    }
    #rTotal<-rowSums(kwVsCoun)
    #output<-cbind(kwVsCoun,rTotal)
    output<-kwVsCoun
    
    return(output)
}

#construct the kwvssubject matrix
getkwVsSubj<-function(adopn,accsk){
    #adopn                     #keywords to be assigned their properties
    kw<-accsk[,"comparKw"]    #index that allows me to locate the keywords on all documents
    subju<-getUSubj(accsk)     #unique subjects to count
    #adopn<-adopn_n
    #rm(adopn)
    kwVsSubj<-matrix(0,
                     nrow = length(adopn),#a container matrix, keywords Vs subj.
                     ncol=length(subju),
                     dimnames = list(adopn,subju)
                     )
    #as.character(adopn)
    for(i in 1:length(adopn)){
        #i<-10
        y<-adopn[i]#unique keyword
        #y<-adopn[1]
        thing<-grep(paste0("^",y,"\\s\\||\\|\\s",y,"\\s\\||\\|\\s",y,"$|^",y,"$"), kw)#position
        #of the document with the keyword of interest.
        kwpSubj<-paste(accsk[thing,"outputSubject"],collapse = " | ")#all subjects belonging to that keyword
        #accsk[thing,]
        subj<-fieldSplitter(kwpSubj)
        #Count each field.
        #attributes() 
        subjCount<-table(subj)#contingency table to count each element, like the histogram
        #targets the country columns of the output matrix
        kwVsSubj[i,unlist(attributes(subjCount)$dimnames$subj)]<-subjCount
    }
    #rTotal<-rowSums(kwVsSubj)
    #output<-cbind(kwVsSubj,rTotal)
    output<-kwVsSubj
    return(output)
}

getAllduplicates<-function(x){
    output<-duplicated(x) | duplicated(x, fromLast = TRUE)
    return(output)
    
}
#------------------------------------------------------------------------------#
#---------------------NETWORK TOPOLOGY Analysis--------------#
#Function to create the list of cluster profiles 
#OPTIMIZATION: we can get some data from the community object
##PREPARING THE DATA
#accs the working slice of db
#np the keywords i am working on
#output a brand to the titles of which clusters they belong to. 
#OPTIMIZATION: automatic detection of calculated modularities
rdbSetKwCluster<-function(accsk,np,type){
    #Create an empty vertor of same size than accs rows
    mv<-vector("character",nrow(accsk))
    #em<-vector("character",nrow(accs))
    #get the titles for each cluster and each keyword
    ## get the keywords of cluster 
    cd<-length(unique(np[,paste(type)]))
    #start the loop for the clusters
    #do that for every cluster
    for(i in 1:cd){
        kwList<-np[np[,paste(type)]==i,c("kwValue")]# have to pass the real matching keywords
        #for each keyword find the titles and set the cluster number they belong too.
        ind<-unique(unlist(lapply(kwList,GetTibyKw,accsk)))# have all the index of titles that belong to that keyword
        #here have to set the empty vectors with values that by this
        ##detect if the corrsponding empty vector is null
        ##set the modularity to each title
        ##when repeated, append 
        mv[ind]<-paste(mv[ind],i, sep = "|")
    }
    output<-cbind(accsk,mv)
    return(output)
}

##GETTING CLUSTER PROFILE
# Getting the cluster subject component
GetClusterSu<-function(cd,np,kwVsSubj,type){
    buff<-kwVsSubj[np[,paste(type)]==cd,]
    if(is.matrix(buff)){
        output<-colSums(kwVsSubj[np[,paste(type)]==cd,])
    }else{
        output<-buff
    }
    return(output)
}

#Getting the cluster country component
GetClusterCo<-function(cd,np,kwVsCoun,type){
    buff<-kwVsCoun[np[,paste(type)]==cd,]
    if(is.matrix(buff)){
        output<-colSums(kwVsCoun[np[,paste(type)]==cd,])
    }else{
        output<-buff
    }
    return(output)
}

#Getting the cluster more in between words
GetClusterKb<-function(cd,np,top,type){
    #buff<-np[np[,paste(type)]==cd,c("Betweenness CentralityI","Keyword")]
    
    buff<-np[np[,paste(type)]==cd,c("Eigenvector Centrality","Keyword")]
    #------------getting all keywords-----------------------
    if(top=="all"){
        top<-nrow(buff)
    }
    #--------end of getting all keywords--------------------------
    
    if(is.matrix(buff)){#is a matrix
        
        #buff<-buff[order(as.numeric(buff[,"Betweenness CentralityI"]),decreasing = T),]
        
        buff<-buff[order(as.numeric(buff[,"Eigenvector Centrality"]),decreasing = T),]
        if(top>nrow(buff)){
            output<-buff
        }else{
            output<-buff[1:top,]# if there are more keywrods than TOP
        }
    }else{# is a vector
        output<-buff
    }
    return(output)
}

#Getting the number of words in the cluster and its percentage
GetClusterKN<-function(cd,np,type,totalKw){
    ckw<-length(np[np[,paste(type)]==cd,1])
    ckp<-(ckw/totalKw)*100
    output=c(ckw,ckp)
    return(output)
}

#gets title index by kw
#returns the index of titles that have the keyword
GetTibyKw<-function(kw,accsk){
    ltacKw<-accsk[,"comparKw"]
    thing<-grep(paste0("^",kw,"\\s\\||\\|\\s",kw,"\\s\\||\\|\\s",kw,"$|^",kw,"$"), ltacKw)#position
    output<-thing
    return(output)
}

#sub function of GetClusterTi
#added the keyword addition
GetTiSubyKw<-function(kw,accp,kwVsSubj=""){
    #check for valid fields
    #define and index according to the X field 
    #get the data from the needed tables. 
    #kw<-"machine learning"
    ltacKw<-accp[,"comparKw"]
    thing<-grep(paste0("^",kw,"\\s\\||\\|\\s",kw,"\\s\\||\\|\\s",kw,"$|^",kw,"$"), ltacKw)#position
    if(kwVsSubj==""){
      buff<-accp[thing,c("outputTitle","outputAbs","mv")]
    }else{
      buff<-accp[thing,c("outputTitle","outputSubject","outputAbs","mv")]
    }
    kwc<-vector("character",length = nrow(buff))# adding the keyword to see later the link
    kwc[]<-kw                                   # adding the keyword to see later the link
    
    buff<-cbind(buff,kwc)
    output<-buff
    return(output)
}

# sub function of GetClusterTi
# OPTIMIZATION: check if we can simply this function
RmReComb<-function(ct,kwVsSubj=""){
    # find the registries you want with the magic logical filter
    #ct<-GetClusterTi(cb[,"Keyword"],ac)
    # get unique titles
    # absorbe all their keywords one by one and paste them to them
    dx<-!duplicated(ct[,"outputTitle"])
    ind<-ct[dx,"outputTitle"]
   
    des<-ct[dx,"outputAbs"]
    mv<-ct[dx,"mv"]
    buff<-output<-unlist(lapply(ind,function(x){fieldMerger(ct[ct[,"outputTitle"]==x,"kwc"])}))
    if(kwVsSubj==""){
      output<-cbind(outputTitle=ind,kwc=buff,dcdescription=des,mv)
    }else{
      sub<-ct[dx,"outputSubject"]
      output<-cbind(outputTitle=ind,outputSubject=sub,kwc=buff,dcdescription=des,mv)
    }
    return(output)
}
#Gets the papers of the cluster
#input: keywords of the cluster and raw data base
#OPTIMIZATION: do not know how to avoid the loop
#Issue (SOLVED): pending to eliminate repeated and combine the keyword elements so we get the papers and their link
#Analysis assupmtion only the 10 top central keywords and their related papers were used to fix a name for the cluster
GetClusterTi<-function(cb,accp,kwVsSubj=""){
    #get the titles per keyword
    #put them together
    #eliminate repeated
    cb<-prePmix(cb)
    buff<-(lapply(cb,GetTiSubyKw,accp,kwVsSubj))
    
    for(i in 1:length(buff)){# getting the elements from the list
        if(i==1){
            ct<-buff[[i]]
        }else{
            ct<-rbind(ct,buff[[i]])
        }
    }
    output<-RmReComb(ct,kwVsSubj)
    return(output)
}
#Create a sub graph 
GetClusterNe<-function(cd,np,g1,type="Modularity ClassIeb"){
    buff<-np[np[,paste(type)]==cd,1]#getting the keywords of say cluster
    cIndex<-which(V(g1)$name %in% buff)# getting the index of the sub graph
    gc<-induced_subgraph(g1,cIndex)
    output<-gc
    return(output)
}

#Gets the inside cluster centrality
# last modified 10/07/2016
# by commenting or un-commenting you can get edge betweeness centrality or 
# eigen vector centrality
GetClustergc<-function(cg,top,i){
    #verify if there are more than 3 nodes (minimum condition to have betweenes)
    #if there are less than 3 we put their betweeness in one just to get all the words
    #Create a numeric vector the same size as
    if(top=="all"){ # if we want all words
        top<-length(V(cg)$name)
    }
    if(top>length(V(cg)$name)){# if there are less words than top asked for
        top<-length(V(cg)$name)
    }
    
    if(length(V(cg)$name)<3){
        buff<-vector("numeric",length(V(cg)$name))
        names(buff)<-V(cg)$name
        warning("for cluster: ",paste(i),"the centrality was appoximated")
    }else{
        # edgebetweenes centrality
        #buff<-sort(betweenness(cg,directed = F, normalized = T),decreasing = T)
        
        # eigen vector centrality. 
        # i have to get the names... 
        ceg<-centr_eigen(cg, directed = FALSE, scale = TRUE, normalized = TRUE)
        buff<-ceg$vector
        names(buff)<-V(cg)$name
        buff<-sort(buff,decreasing = T)
    }
    
    output<-buff[1:top]
    return(output)
}
#Gets the inside cluster the degree
GetClustergd<-function(cg,top){
    buff<-sort(degree(cg,mode = c("all")),decreasing = T)
    if(top=="all"){
        top<-length(buff)
    }
    if(top>length(V(cg)$name)){# if there are less words than top asked for
        top<-length(V(cg)$name)
    }
    output<-buff[1:top]
    return(output)
}

#Executes all the property getting functions
# Last modified 10/07/2016
GetClusterData<-function(cd,np,kwVsCoun="",kwVsSubj="",accp,g1,type="Modularity ClassIeb",top=10){
    #cd<-length(unique(np[,4]))
    #cd<-length(unique(np[,paste(type)]))
    
    totalKw<-nrow(np)
    cmeta<-vector("list",cd)
    #type<-"Modularity ClassIL"
    #top<-10
    for(i in 1:cd){
        
        cw<-GetClusterKN(i,np,type,totalKw)#ok
        #GetClusterSu(1,np,kwVsSubj,type)#ok
        if(kwVsSubj==""){
          cs<-NULL
        }else{
          cs<-GetClusterSu(i,np,kwVsSubj,type)#ok
        }
        #GetClusterCo(1,np,kwVsCoun,type)#ok
        if(kwVsCoun==""){
          cc<-NULL
        }else{
          cc<-GetClusterCo(i,np,kwVsCoun,type)#ok
        }
        
        cg<-GetClusterNe(i,np,g1,type)#ok search in np for the words of a cluster and then generate the sb graph
        #cg<-GetClusterNe(1,np,g1,type)
        #GetClusterNe(1,np,g1,type)
        # to get cluster node properties is better to align them with the keywords and their number
        # so use a little function to format them before giving them to the profile.
        #cg<-GetClusterNe(8,np,g1,type)
        
        cb<-GetClusterKb(i,np,top,type)#Here have to update
        
        #top<-"all"
        #GetClusterKb(1,np,top,type)
        #cb<-GetClusterKb(1,np,top,type)
        
        #class(cb)
        #
        if(is.matrix(cb)){#multi keyword clusters
            cgc<-GetClustergc(cg,top,i)# here i have to update
            #comes without names..... gotta do something about it. 
            #GetClustergc(cg,top,1)
            
            cgd<-GetClustergd(cg,top)
            #Correct here for 2013, if the betweeness is cero, 
            #select the degree, for networks of two elements.
            ct<-GetClusterTi(names(cgc),accp,kwVsSubj)#ok
        }else{# one keyword clusters
            #cgc<-cb[2] real
            # to make them operational i have to fill them with dummies
            dummy<-vector("numeric",length(cb[2]))
            names(dummy)<-cb[2]
            cgc<-dummy
            cgd<-dummy
            ct<-GetClusterTi(cb[2],accp)
        }
        cmeta[[i]]<-list(cb=cb,cw=cw,cs=cs,cc=cc,ct=ct,cg=cg,cgc=cgc,cgd=cgd)
    }
    output<-cmeta
    return(output)
}
#np: properties of the general network
#KwVsCoun,kwVsSubj the histogram of subjects and countries of this cluster
#accp: improved database with the keyword cluster paper tag
#g1: general network graph
#type: type of clustering algorithm we want to get
#top: number of keywords that will be analized to define cluster names. 
GetClusterP<-function(np,kwVsCoun="",kwVsSubj="",accp,g1,type,top=10){
    #top<-10
    #typeA<-"Modularity ClassIeb"
    #typeB<-"Modularity ClassIL"
    #to filter as all of the vector have the same order, make a mask
    # the mask is going to be only the elements belonging to each cluster
    cd<-length(unique(np[,paste(type)]))#number of edge betweenes clusters 
    #cd<-length(unique(np[,paste(typeB)]))
    cprofile<-GetClusterData(cd,np,kwVsCoun,kwVsSubj,accp,g1,type,top)
    #locprofile<-GetClusterData(cd,np,kwVsCoun,kwVsSubj,type,top)
    output<-cprofile
    return(output)
}

#function to save the papers and the abstracts from each clusters and their time
ReportCt<-function(D,cp,type){
    if(type=="Modularity ClassIeb"){
        ty<-"eb"
    }
    if(type=="Modularity ClassIL"){
        ty<-"lo"
    }else{
        ty<-"un"
    }
    if(!dir.exists(D)){
        #create the folder
        dir.create(paste0(D,ty,collapse=""))
    }else{
        #do nothing
    }
    #save the files
    
    mp<-"C:/Users/santiago/Dropbox/Doctorado/R/scopus API"
    dn<-paste0(mp,"/",paste0(D,ty))
    
    for(i in 1:length(cp)){
        write.csv(cp[[i]]$ct,paste0(dn,"/Cluster",i,".csv"))
    }
}

Export2GephiN<-function(cg){
    #if(length(cg)>1){#is a network
    npb<-betweenness(cg,directed = F, normalized = T)# gives in the same order
    npd<-degree(cg,mode = c("all"))#,loops = TRUE, normalized = FALSE)
    np<-cbind(as.character(unlist(attributes(npb)[1]))
              ,as.character(unlist(attributes(npb)[1]))#list of keywords
              ,as.numeric(npb)  # Node betweenness
              ,as.numeric(npd)) # Node degree
    colnames(np) <- c("Id","Label","Betweenness CentralityI","DegreeI")
    output<-np
    
    
    #}else{# is only a node
    #         npb<-1
    #         npd<-1
    #         np<-cbind(as.character(unlist(attributes(npb)[1]))#list of keywords
    #                   ,as.numeric(npb)  # Node betweenness
    #                   ,as.numeric(npd)) # Node degree
    #         colnames(np) <- c("Keyword","Betweenness CentralityI","DegreeI")
    #         output<-np
    #             #save only one file
    #     }
    #     npb<-betweenness(cg,directed = F, normalized = T)# gives in the same order
    #     npd<-degree(cg,mode = c("all"))#,loops = TRUE, normalized = FALSE)
    #     np<-cbind(as.character(unlist(attributes(npb)[1]))#list of keywords
    #               ,as.numeric(npb)  # Node betweenness
    #               ,as.numeric(npd)) # Node degree
    #     colnames(np) <- c("Keyword","Betweenness CentralityI","DegreeI")
    #     output<-np
    return(output)
}
Export2GephiE<-function(cg){
    edge1 <- get.edgelist(cg, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(cg,"c", index=E(cg))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    edgefull1= cbind(edge1,edgeW,eT)# joining them 
    dimnames(edgefull1) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    output<-edgefull1
    return(output)
}

ReportCg<-function(D,cp){
    cname<-vector("character",length(cp))
    buffs<-matrix("",nrow =length(cp),ncol = 2,dimnames = list(NULL,c("kw#","%")))
    
    mp<-"C:/Users/santiago/Dropbox/Doctorado/R/scopus API"
    dn<-paste0(mp,"/",paste0(D))
    if(!dir.exists(D)){
        #create the folder
        dir.create(paste0(D,collapse=""))
    }else{
        #do nothing
    }#CREATING THE DIRECTORY
    for(i in 1:length(cp)){#for each cluster get the grap and format the network for export
        #NODES make a function for this general for any network
        # create a vector of the best word and the size
        
        buffs[i,]<-cp[[i]]$cw 
        if(length(V(cp[[i]]$cg))>1){#is a network
            cname[i]<-attributes(cp[[i]]$cgd[1])$names
            nl<-Export2GephiN(cp[[i]]$cg)
            el<-Export2GephiE(cp[[i]]$cg)
            write.csv(nl,paste0(dn,"/exN",i,".csv"))
            write.csv(el,paste0(dn,"/exE",i,".csv"))
            write.csv(cp[[i]]$cw,paste0(dn,"/cw",i,".csv"))
            #save both files
        }else{# is only a node.
            cname[i]<-cp[[i]]$cgd[1]
            
            nl<-Export2GephiN(cp[[i]]$cg)
            write.csv(nl,paste0(dn,"/exN",i,".csv"))
            write.csv(cp[[i]]$cw,paste0(dn,"/cw",i,".csv"))
            #save only one file
        }
    }
    output<-cbind(cname,buffs)
    write.csv(output,paste0(dn,"/NETclusters.csv"))
}
ReportNt<-function(D,g1,np,netKSHop,netKCHop){
    mp<-"C:/Users/santiago/Dropbox/Doctorado/R/scopus API"
    dn<-paste0(mp,"/",paste0(D))
    if(!dir.exists(D)){
        #create the folder
        dir.create(paste0(D,collapse=""))
    }else{
        #do nothing
    }
    np[,"kwValue"]<-np[,"Keyword"]#barillazo
    colnames(np) <- c("Id","Label","Betweenness CentralityI","DegreeI","Modularity ClassIeb"
                      ,"Modularity ClassIL")
    
    el<-Export2GephiE(g1)
    write.csv(np,paste0(dn,"/NETWORKexN.csv"))
    write.csv(el,paste0(dn,"/NETWORKexE.csv"))
    write.csv(netKSHop,paste0(dn,"/NETWORKsub.csv"))
    write.csv(netKCHop,paste0(dn,"/NETWORKcou.csv"))
}

#-------------------------DATA EXPLORATION-WOS---------------------#


####----------END OF WEB OF SCIENCE TRANSFORMATION-----------#

####---------------- SCOPUS DATA OBTENTION ------------------#
K="730bd9ca0b537c19a0ea8763fd2734e4"
bbc_Q<-"AFFILORG ( bbc ) 
AND (DOCTYPE (\"ar\") OR DOCTYPE (\"ip\") OR DOCTYPE (\"cp\"))
AND (SRCTYPE (\"j\") OR SRCTYPE (\"p\"))
AND NOT AFFILORG ( \"BBC Research & Consulting\" OR 
\"BBC Aktiengesellschaft\" OR \"BBC Forschungszentrum\" OR
\"BBC&M Engineering Inc\" OR \"BBC and M Engineering, Inc\" OR
\"BBC and M Engineering Inc.\" OR \"BBC Mannheim\" OR 
\"BBC Forschungszentrum\"  OR \"BBC-Forschungs-zentrum\" OR 
\"ABB Group\" OR \"BBC Baden\" OR  \"*Biotherapeutics and Bioinnovation Center*\"  OR  \"*BBC Research Unit, USDA*\"
OR  \"*CLAHRC*\" OR \"*BBC-Campus*\"  OR  \"*Biotechnological-Biomedical Center*\" 
OR  \"*BBC AD*\" OR \"BENEO-BBC*\" OR \"BBC Industrial Chemicals Co*\")"

nhk_Q<-"AFFILORG(NHK*)AND (DOCTYPE (\"ar\") OR DOCTYPE (\"ip\") OR DOCTYPE (\"cp\"))AND (SRCTYPE (\"j\") OR SRCTYPE (\"p\"))AND NOT AFFILORG (\"NHK Spring Co., Ltd.\" OR\"NHK Spring Co., Ltd.\" OR\"NHK Spring Co., Ltd.\" OR\"NHK Spring Co., Ltd\" OR\"NHK Spring Co, Ltd\" OR\"R and D Center NHK Spring Inc.\" OR\"NHK Spring Co., Ltd\" OR\"NHK Spring Co., Ltd.\" OR\"Research Institute of the NHKG Steelworks\" OR\"NHK Spring Co, Ltd\" OR\"NHK Spring Co., Ltd.\" OR\"R and D Center NHK Spring Inc.\" OR\"NHK Spring Co, Ltd\" OR\"NHK Spring Co., Ltd.\" OR\"NHKG\" OR  \"NHKC\" OR \"*NHK SPRING*\")"

QF="dc:title,authkeywords,dc:description,affiliation-country,author"

###-------FUNCTION TO CALL AND SAVE FOR DIFFERNT YEARS------###
#lapply function that runs the query on each year,
# inputs
# D year
# Q query
# K apu kye
# QF queried fields
# mode NHK or BBC
run_search_by_year<-function(D,Q,K,QF,mode){
    abs<-scopusSSubject(Q,D,K,QF)
    absc<-cRnoRepeat(abs)
    if(mode=="NHK"){
        nhk_scopus<-absc
        save(nhk_scopus,file=paste0("nhk_scopus_",D,".rdata"))
    }else{
        bbc_scopus<-absc
        save(bbc_scopus,file=paste0("bbc_scopus_",D,".rdata"))
    }
    return(NULL)
}
mode<-"BBC"
D<-as.character(2000:2016)
lapply(D,run_search_by_year,bbc_Q,K,QF,mode)
mode<-"NHK"
lapply(D,run_search_by_year,nhk_Q,K,QF,mode)

load("bbc_scopus_2016.rdata")
###------------------------fusing WOS and scopus---------------------##
# first make the same column names
# # MATRIX
# # "outputTitle" - >TI
# # "outputKw"    - >DE
# # "outputAbs"   - >AB
# # "outputAffcou"- >AU
# # "outputSubject"->WC
# valid_columns <- c("UT", #WOS ID
#                    "AU", #Authors
#                    "TI", #Title
#                    "SO", #Publication name
#                    "J9", #Standard name of journal
#                    "DE", #Author keywords
#                    "ID", #WOS keywords
#                    "AB", #Abstract
#                    "CR", #Cited References
#                    "NR", #Number of references
#                    "TC", #Times cited
#                    "PY", #Publication Year
#                    "DI", #DOI
#                    "WC", #WOS categories
#                    "SC") #Research Areas

## Load both files 
### load WOS data for BBC

load("WOS/a.RData")
load("WOS/b.RData")
load("WOS/c.RData")
load("WOS/d.RData")
load("WOS/e.RData")
load("WOS/f.RData")
load("WOS/g.RData")
ai_wos_data <- rbind(a,b,c,d,e,f,g)#250300
ai_wos_data <- unique(ai_wos_data) #248361/249300 (original)


ai_wos_d<-ai_wos_data[,c("TI","DE","AB","AU","PY")]
colnames(ai_wos_d)<-c("outputTitle","outputKw","outputAbs","outputAuthor","PY")

### load scopus data for NHK
#input
## lapply function
## p= NHK or BBC
## x= years to get
load_absc<-function(x,p){
    if(p=="NHK"){
        #NHK_scop
        load(file=paste0(p,"_scop/nhk_scopus_",x,".rData"))#getting the stored data
        buff<-nhk_scopus
    }else{
        load(file=paste0(p,"_scop/bbc_scopus_",x,".rData"))#getting the stored data
        buff<-bbc_scopus
    }
    # Change the column names
    #p<-"NHK"
    #x<-"2001"
    #load(file=paste0(p,"_scop/nhk_scopus_",x,".rData"))
    PY<-vector("character",nrow(buff))
    PY[]<-x
    buff<-cbind(buff,PY)
    y<-buff[,c("outputTitle","outputKw","outputAbs","outputAuthor","PY")]
    #colnames(y)<-c("TI","DE","AB","AU","PY")
    #return the output
    output<-y
    return(output)
}
format_absc<-function(x,p){
    y<-lapply(x,load_absc,p)
    output<-do.call(rbind,y)
    return(output)
}
x<-as.character(2000:2016)
p<-"NHK"
nhk_scopus_data<-format_absc(x,p)
save(nhk_scopus_data,file="nhk_scopus_data.rdata")
p<-"BBC"
bbc_scopus_data<-format_absc(x,p)
save(bbc_scopus_data,file="bbc_scopus_data.rdata")
## organize them in one data set

### ---------HOMOGENOUS FORMATING-----------####

format_wos_DE<-function(x){
    #x<-bbc_wos_data
    x[,"outputKw"]<-gsub(";"," |",x[,"outputKw"])
    # remove ; for our standard separator |
    output<-x
    return(output)
}
format_wos_AU<-function(x){
    #x<-bbc_wos_data
    # remove , after first name and 
    x[,"outputAuthor"]<-gsub(",","",x[,"outputAuthor"])
    # remove ; for our standard separator |
    x[,"outputAuthor"]<-gsub(";"," |",x[,"outputAuthor"])
    output<-x
    return(output)
}
homogeneous_wos<-function(x){format_wos_DE(format_wos_AU(x))}

bbc_wos_data_f<-homogeneous_wos(bbc_wos_data)
nhk_wos_data_f<-homogeneous_wos(nhk_wos_data)

format_sco_AU<-function(x){
    #x<-bbc_scopus_data
    #remove the dot, after the names
    x[,"outputAuthor"]<-gsub("\\.","",x[,"outputAuthor"])
    output<-x
    return(output)
}
format_sco_DE<-function(x){
    #change NA to ""
    #x<-bbc_scopus_data
    x[is.na(x[,"outputKw"]),"outputKw"]<-""
    output<-x
    return(output)
}
homogeneous_sco<-function(x){format_sco_DE(format_sco_AU(x))}


bbc_scopus_data_f<-homogeneous_sco(bbc_scopus_data)
nhk_scopus_data_f<-homogeneous_sco(nhk_scopus_data)

### ---------HOMOGENOUS FORMATING-----------####
# subset into years.
# recover keywords
recover_kw<-function(d,ws_ex){
    # get all prePmixed titles corresponding to d
    #d<-"automatingthedesignofhighrecirculationairliftreactorsusingablackboardframework"
    # from them check their kws
    #d
    #ind<-which(ws_ex[,"ws_titles"]==d[1])
    ind<-which(ws_ex[,"ws_titles"]==d)
    recovered_kw<-vector("character",length(ind))
    
    kws<-ws_ex[ind,"outputKw"]
    flag<-kws[kws!=""]
    if(any(kws!="")){# if the DE field is different from ""
        ##ISSUE: see methodlogical notes, google drive "data"
        if(length(flag)>1){
            recovered_kw[]<-kws[kws!=""][1]
        }else{
            recovered_kw[]<-kws[kws!=""]
        }
    }else{# if not, then leave them in "" and go to the next.
        recovered_kw[]<-""
    }
    output<-cbind(ind,recovered_kw)
    return(output)
}
updating_ws<-function(d,ws_ex){
    buff<-ws_ex
    x<-lapply(d,recover_kw,buff)
    y<-do.call(rbind,x)
    buff[as.numeric(y[,"ind"]),"outputKw"]<-y[,"recovered_kw"]
    output<-buff
    return(output)
}
y<-as.character(2000:2016)
recovering_kw_by_year<-function(y,wos_data,scop_data){
    wos<-wos_data[wos_data[,"PY"]==y,]
    scop<-scop_data[scop_data[,"PY"]==y,]
    
    #wos<-nhk_wos_data_f[nhk_wos_data_f[,"PY"]=="2004",]
    #scop<-nhk_scopus_data_f[nhk_scopus_data_f[,"PY"]=="2004",]

    ws<-rbind(wos,scop)
    
    ### apply premix to the titles and then compare. 
    # homogenizing titles in order to compare them
    ws_titles<-prePmix(ws[,"outputTitle"])
    ws_ex<-cbind(ws,ws_titles)
    # converting the w_titles column from factor to character
    ws_ex[,"ws_titles"]<-as.character(ws_ex[,"ws_titles"])
    # check duplicates, from that list, one by one check if you can get the kw
    ws_d_ind<-duplicated(ws_ex[,"ws_titles"])
    d<-ws_ex[ws_d_ind,"ws_titles"]
    #lapply function
    #input d all the duplicated preprocessed titles
    # ws_ex, the extention of the original matrix plus the preprocessed titles
    ws_ex_up<-updating_ws(d,ws_ex)
    output<-ws_ex_up
    return(output)
}
bbc_ws_data_l<-lapply(y,recovering_kw_by_year,bbc_wos_data_f,bbc_scopus_data_f)
nhk_ws_data_l<-lapply(y,recovering_kw_by_year,nhk_wos_data_f,nhk_scopus_data_f)
######--------------END RECOVERING KEYWORDS--------------######

######--------------ELIMINATE NO KeYWORD REGISTRIES --------------######

#lapply function
#input
# ws : most recent updated version of the ws matrix
remove_no_keyword<-function(ws){
    output<-ws[ws[,"outputKw"]!="",]
    return(output)
}
remove_no_keyword_report<-function(ws){
    b<-nrow(ws)
    a<-nrow(ws[ws[,"outputKw"]!="",])
    output<-cbind(a,b)
    return(output)
}
remove_repeated<-function(ws){
    #
    output<-ws[!duplicated(ws[,"ws_titles"]),]
    return(output)
}
remove_repeated_report<-function(ws){
    b<-nrow(ws)
    a<-nrow(ws[!duplicated(ws[,"ws_titles"]),])
    output<-cbind(a,b)
    return(output)
}

ai_wos_kwdata <- remove_no_keyword(ai_wos_d)#189827(kw)/248361(unique)/249300(original)
ai_wos_kwdata <- format_wos_DE(ai_wos_kwdata)

py <- sort(unique(ai_wos_kwdata[,"PY"]),decreasing = F)
#py_l <- list(py[1:24],py[25:length(py)])
format_to_list <- function(x,d){
    output <- d[d[,"PY"]==x,]
    return(output)
}

ai_wos_kwdata_l<- lapply(py,format_to_list,ai_wos_kwdata)
ai_fb_kwd_l<- ai_wos_kwdata_l[1:24]
ai_sb_kwd_l<- ai_wos_kwdata_l[25:length(ai_wos_kwdata_l)]
ai_fb_kwd_m <- do.call(rbind,ai_fb_kwd_l)
ai_sb_kwd_m <- do.call(rbind,ai_sb_kwd_l)


#### THE OUTPUT ##### THESE ARE MY ACCS
save(ai_wos_kwdata,file="ai_all_data_m.rdata")
save(ai_wos_kwdata_l,file="ai_all_data_l.rdata")
save(ai_fb_kwd_m,file="ai_fb_kwd_m.rdata")
save(ai_sb_kwd_m,file="ai_sb_kwd_m.rdata")

#### THE OUTPUT #####

######-----------END ELIMINATE NO KeYWORD REGISTRIES -------------######

###-----END FUNCTION TO CALL AND SAVE FOR DIFFERENT YEARS---###

####---------------END -SCOPUS DATA OBTENTION ---------------#


##********************************************##
##    CREATING THE DOC VS KEYWORDS MATRIXES   ##
##********************************************##
# while testing will work on parallel with both spare and normal matrix
ai_wos_kwdata_l

reconstruct_i<-function(index,l){
    y<-vector("numeric",length(l))
    y[]<-index
    output<-y
    return(output)
}
get_dvkw_sparse<-function(index,l,n){
    temp_i<-mapply(reconstruct_i,index,l,SIMPLIFY=F)
    i<-do.call("c",temp_i)
    j<-do.call("c",l)
    buff<-sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(index,names(n)))
    output<-buff
    return(output)
}
docvsKw_sparse<-function(ac, rmkey="no"){#ac
  #rmkey="no"
  zltue <- GetKw(ac, rmkey)#all unique keywords
  ## REGULAR  expression proof comparison
  
  #Creating the Doc Vs word matrix
  #number of keywords length(zu)
  #number of docs dim(ac)[1]
  dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
  #h14_dID<-as(h14_dID, "sparseMatrix")
  kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
  # This loop in each document, or element of ac
  #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
  #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
  #all(ylt == yltb)
  for(i in 1:dim(ac)[1]){
    #i=1
    ylt<-prePmix(unlist(strsplit(ac[i,"outputKw"],"|",fixed = T)))#all keywords of a document
    kwv[i]<-fieldMerger(ylt)
    ##This line is for the regular expression matching between keywords.
    #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
    ##-------------------------------------------------------------------
    thing<-vector("numeric")
    #for the keywords of each document search for the matchs
    for(j in 1:length(ylt)){        
      #j=3
      buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
      thing<-c(thing,buff)#the index of the words that are present
    }
    ##-------------------------------------------------------------------
    dvk[i][[1]]<-thing
  }
  output<-list(v=dvk,n=zltue,k=kwv)
  return(output)
  #return(thing)
}
test <- ai_fb_kwd_m[1:10000,]
kwSearch<-function(x,v,z){
  #Here we can get all the versions of the words or just one to name the vector
  #v <- zltsp
  #x <- zltspu[1]
  output<-fieldMerger(unique(z[v%in%x]))
  return(output)
}
#ac<-accs
#ac<- test[1:10000,]
GetKw<-function(ac, rmkey="no"){
  z<-unlist(strsplit(ac[,"outputKw"],"|",fixed = TRUE)) # sapply to get the big vector 5052
  #Data processing
  # Db pedia
  # leveisntein distance
  # General word processing
  ## lower case
  zl<-tolower(z)#2761 - 2012
  ##Recovering keywords
  
  ## remove trailing and leading white space
  zlt<-zl<-trim(zl)#2761 - 2012
  ## remove all white spaces
  zlts<-rmSpace(zlt)#2741 - 2012
  ## Removing punctuation&special characters
  zltsp<-rmPunct(zlts)#2705 - 2012
  
  ## stemming - not very well taken by the keywords
  #zltsps<-wordStem(zltsp, language = "porter")
  #write.csv(cbind(zltsps,zltsp), file = "words.csv")
  
  ## getting unique keywords
  
  zltspu<-unique(zltsp)
  #parallelize this thing.
  # to expensive to add in big data sets context. 
  #names(zltspu)<-unlist(lapply(zltspu,kwSearch,v=zltsp,z=z))
  names(zltspu) <- zltspu
  
  ## show how the words were grouped
  ### value = final word after processing, name = all the words grouped by that one
  ### value = unique words, name = words belonging to the index with that same unique word in z
  #rmkey="big data"
  
  #Remplace the special characters (+) (PLUS)
  #zltspus<-gsub("\\+","(PLUS)",zltspu)
  if(rmkey=="no"){
    output <- zltspu
  }else{
    output <- zltspu[zltspu != rmkey]
  }
  
  return(output)
}
get_dvk <- function(x){return(x$dvk)}
get_kwv <- function(x){return(x$kwv)}
docvsKw_sparse_parallel<-function(ac, rmkey="no"){#ac
  #rmkey="no"
  #ac <- test
  
  zltue <- GetKw(ac, rmkey)#all unique keywords
  ## REGULAR  expression proof comparison
  
  #Creating the Doc Vs word matrix
  #number of keywords length(zu)
  #number of docs dim(ac)[1]
  #dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
  
  #kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
  # This loop in each document, or element of ac
  #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
  #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
  #all(ylt == yltb)
  #ac[1,"outputKw"]
  cores=detectCores()
  cl <- makeCluster(cores[1]-2)
  registerDoParallel(cl)
  poutput<- foreach (i=1:dim(ac)[1]) %dopar% {
    #i=1
    fieldMerger<-function(x){
      output<-paste(x,collapse = " | ")
      return(output)
    }
    trim <- function (x) {gsub("^\\s+|\\s+$", "", x)}
    rmSpace<-function(x){gsub("\\s", "", x)}
    rmPunct<-function(x){gsub("[[:punct:]]","",x)}
    prePmix<-function(x){rmPunct(rmSpace(trim(tolower(x))))}
    ylt<-prePmix(unlist(strsplit(ac[i,"outputKw"],"|",fixed = T)))#all keywords of a document
    
    #kwv[i]<-fieldMerger(ylt)
    #closeAllConnections()
    ##This line is for the regular expression matching between keywords.
    #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
    ##-------------------------------------------------------------------
    
    thing<-vector("numeric")
    
  #}
  #stopCluster(cl)
    #for the keywords of each document search for the matchs
    for(j in 1:length(ylt)){        
      #j=3
      buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
      thing<-c(thing,buff)#the index of the words that are present
    }
    ##-------------------------------------------------------------------
    #dvk[i][[1]]<-thing
    list(dvk=thing,kwv=fieldMerger(ylt))
  }
  stopCluster(cl)
  closeAllConnections()
  
  dvk<- lapply(poutput,get_dvk)
  kwv <- unlist(lapply(poutput,get_kwv))
  
  output<-list(v=dvk,n=zltue,k=kwv)
  return(output)
}
do_networks_Kw_sparse<-function(accs,mode="NHK"){
    #accs<-bbc_ws_data_l_ok_u[7][[1]]
    y<-accs[,"PY"][1]#getting the year
    x<-docvsKw_sparse(accs)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse(index,l,n)
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0(mode,"_fused/ad_s",y,".RData"))
    
    ##-------------Ready to compare raw database--------------
    accsk<-cbind(accs,ad_s$k)
    colnames(accsk)<-c(colnames(accs),"comparKw")
    save(accsk,file=paste0(mode,"_fused/acck",y,".RData"))
    ##-------------------------------------------------------
    ##---------------Creating the network----------------------
    
    ### Optimizing network
    ### decided to ommit optimization as some networks will be small
    ### also allows to use the future code as it is, no need to 
    ## adapt to use desriptive keywords as the network will have all
    ## keywords of the year.
    if(nrow(adv_s)>0){#the matrix has more than one document ?
        #adsum_s<-colSums(adv_s)# i get the index of the colums that dont sum and ignore them
        #adop_s<-as.matrix(adv_s[,which(adsum_s>1)])# okey!
        #adopn_s<-adn_s[which(adsum_s>1)]
        adop_s<-adv_s
        adopn_s<-adn_s
        #colnames(adop_s)<-adopn_s
        
        #adop_s<-adv_s
        #class(adop_s)
        #class(adv_s)
        
        
        if(ncol(adop_s)==0){#if there are no keywords with more than 2 co-ocurrence we can not do the network
            output<-NULL
        }else{
            ## following up with the names of the words 
            
            #adopn_s<-adn_s
            #length(adopn_s)
            ## Getting adjacency matrix
            adjm_s<-t(adop_s)%*%(adop_s)
            #class(adjm_s)
            #attributes(adjm_s)
            #colnames(adjm_s)<-GetadNames(colnames(adjm_s))
            #rownames(adjm_s)<-GetadNames(colnames(adjm_s))
            
            ## CREATING THE NETWORK
            g1_s <- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an edge attribute, to use them later on gephi. in this step was the problem, the NA value was taken as a valid connection for the network!!
            save(g1_s,file=paste0(mode,"_fused/g1_s",y,".RData"))
            ## Communities
            clouv_s<-cluster_louvain(g1_s)
            g1_s$comm<-clouv_s
            ## Centrality
            ceg_s<-centr_eigen(g1_s, directed = FALSE, scale = TRUE, normalized = TRUE)
            
            npcml_s<-membership(clouv_s)
            npcmlv_s<-as.numeric(npcml_s)
            
            #STRUCTURAL PROPERTIES
            npb_s<-betweenness(g1_s,directed = FALSE, normalized = TRUE)# gives in the same order
            
            npd_s<-degree(g1_s,mode = c("all"))#,loops = TRUE, normalized = FALSE)
            
            np_s<-cbind(as.character(V(g1_s)$name)#list of keywords
                        ,as.character(adopn_s)# keywords to compare
                        ,as.numeric(npb_s)  # Node betweenness
                        ,as.numeric(npd_s)  # Node degree
                        #          ,as.numeric(npcm_s) # Node edge betweeness community
                        ,npcmlv_s           # Lovain community
                        ,as.numeric(ceg_s$vector)) # eigen vector centrality
            colnames(np_s) <- c("Keyword",
                                "kwValue",
                                "Betweenness CentralityI",
                                "DegreeI",
                                #"Modularity ClassIeb",
                                "Modularity ClassIL",
                                "Eigenvector Centrality")
            save(np_s,file=paste0(mode,"_fused/np_s",y,".RData"))
            
            npexp_s<-cbind(as.character(unlist(attributes(npb_s)[1])),
                           as.character(unlist(attributes(npb_s)[1])),
                           as.numeric(npb_s),
                           as.numeric(npd_s),
                           #as.numeric(npcm_s),
                           npcmlv_s,
                           as.numeric(ceg_s$vector)
            )
            #npsTotal_s,
            #npcTotal_s)
            colnames(npexp_s) <- c("Id",
                                   "Label",
                                   "Betweenness CentralityI",
                                   "DegreeI",
                                   #"Modularity ClassIeb",
                                   "Modularity ClassIL",
                                   "Eigenvector Centrality")
            #"Subjects",
            #"Countries")
            write.csv(npexp_s,file=paste0(mode,"_fused/NOP_s",y,".csv"))
            
            edge1_s <- get.edgelist(g1_s, names=TRUE)# getting the Edgelist
            edgeW_s <- get.edge.attribute(g1_s,"c", index=E(g1_s))# getting the weights of each link
            
            eT_s<-vector(mode = "character",length=length(edgeW_s))
            eT_s[]<-"Undirected"
            
            edgefull1_s= cbind(edge1_s,edgeW_s,eT_s)# joining them 
            dimnames(edgefull1_s) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
            
            write.csv(edgefull1_s,file=paste0(mode,"_fused/EOP_s",y,".csv"))
            #write.csv(edgefull1_s,"Convergence/2008/EOP_s_2008.csv")
            output<-g1_s
        }
    }else{
        output<-NULL
    }
    ##-------------------------------------------------------
    return(output)
}

#lapply function
# get the accs list and returns the accsk list
# in the process saving the ad and accsk objects
Ausearch<-function(x,v,z){
    #Here we can get all the versions of the words or just one to name the vector
    output<-fieldMerger(unique(z[grep(paste0("^",x,"$"),v)]))
    return(output)
}
#ac<-accs
GetAu<-function(ac, rmkey="no"){
    z<-unlist(strsplit(ac[,"outputAuthor"],"|",fixed = TRUE)) # sapply to get the big vector 5052
    #Data processing
    # Db pedia
    # leveisntein distance
    # General word processing
    ## lower case
    zl<-tolower(z)#2761 - 2012
    ##Recovering keywords
    
    ## remove trailing and leading white space
    zlt<-zl<-trim(zl)#2761 - 2012
    ## remove all white spaces
    zlts<-rmSpace(zlt)#2741 - 2012
    ## Removing punctuation&special characters
    zltsp<-rmPunct(zlts)#2705 - 2012
    
    ## stemming - not very well taken by the keywords
    #zltsps<-wordStem(zltsp, language = "porter")
    #write.csv(cbind(zltsps,zltsp), file = "words.csv")
    
    ## getting unique keywords
    
    zltspu<-unique(zltsp)
    names(zltspu)<-unlist(lapply(zltspu,Ausearch,v=zltsp,z=z))
    
    ## show how the words were grouped
    ### value = final word after processing, name = all the words grouped by that one
    ### value = unique words, name = words belonging to the index with that same unique word in z
    #rmkey="big data"
    
    #Remplace the special characters (+) (PLUS)
    #zltspus<-gsub("\\+","(PLUS)",zltspu)
    if(rmkey=="no"){
        output <- zltspu
    }else{
        output <- zltspu[zltspu != rmkey]
    }
    
    return(output)
}
docvsAu_sparse<-function(ac, rmkey="no"){#ac
    #rmkey="no"
    #ac<-bbc_ws_data_l_ok_u[4][[1]]
    #rmkey<-"no"
    zltue <- GetAu(ac, rmkey)#all unique keywords
    ## REGULAR  expression proof comparison
    
    #Creating the Doc Vs word matrix
    #number of keywords length(zu)
    #number of docs dim(ac)[1]
    dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
    #h14_dID<-as(h14_dID, "sparseMatrix")
    kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
    # This loop in each document, or element of ac
    #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
    #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
    #all(ylt == yltb)
    for(i in 1:dim(ac)[1]){
        #i=1
        ylt<-prePmix(unlist(strsplit(ac[i,"outputAuthor"],"|",fixed = T)))#all keywords of a document
        kwv[i]<-fieldMerger(ylt)
        ##This line is for the regular expression matching between keywords.
        #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
        ##-------------------------------------------------------------------
        thing<-vector("numeric")
        #for the keywords of each document search for the matchs
        for(j in 1:length(ylt)){        
            #j=1
            buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
            thing<-c(thing,buff)#the index of the words that are present
        }
        ##-------------------------------------------------------------------
        dvk[i][[1]]<-thing
    }
    output<-list(v=dvk,n=zltue,k=kwv)
    return(output)
    #return(thing)
}

# INPUTS
## accs: vector with the ready to process data
get_accsk_sparse<-function(accs,mode="NHK"){
    #accs<-bbc_ws_data_l_ok_u[3][[1]]
    y<-accs[,"PY"][1]#getting the year
    x<-docvsKw_sparse(accs)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse(index,l,n)
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0(mode,"_fused/ad_s",y,".RData"))
    
    ##-------------Ready to compare raw database--------------
    accsk<-cbind(accs,ad_s$k)
    colnames(accsk)<-c(colnames(accs),"comparKw")
    save(accsk,file=paste0(mode,"_fused/acck",y,".RData"))
    return(NULL)
}
#mapply
do_networks_Kw_sparse<-function(x,y,mode="NHK"){
    #accs<-bbc_ws_data_l_ok_u[7][[1]]
    #x <- aifb_docvsKw_l[1][[1]]
    #y <- 1990
    #y<-accs[,"PY"][1]#getting the year
    #x<-docvsKw_sparse(accs)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse(index,l,n)
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0(mode,"_fused/ad_s",y,".RData"))
    
    ##-------------Ready to compare raw database--------------
    accsk<-cbind(accs,ad_s$k)
    colnames(accsk)<-c(colnames(accs),"comparKw")
    save(accsk,file=paste0(mode,"_fused/acck",y,".RData"))
    ##-------------------------------------------------------
    ##---------------Creating the network----------------------
    
    ### Optimizing network
    ### decided to ommit optimization as some networks will be small
    ### also allows to use the future code as it is, no need to 
    ## adapt to use desriptive keywords as the network will have all
    ## keywords of the year.
    if(nrow(adv_s)>0){#the matrix has more than one document ?
        #adsum_s<-colSums(adv_s)# i get the index of the colums that dont sum and ignore them
        #adop_s<-as.matrix(adv_s[,which(adsum_s>1)])# okey!
        #adopn_s<-adn_s[which(adsum_s>1)]
        adop_s<-adv_s
        adopn_s<-adn_s
        #colnames(adop_s)<-adopn_s
            
        #adop_s<-adv_s
        #class(adop_s)
        #class(adv_s)
        
        
        if(ncol(adop_s)==0){#if there are no keywords with more than 2 co-ocurrence we can not do the network
            output<-NULL
        }else{
            ## following up with the names of the words 
            
            #adopn_s<-adn_s
            #length(adopn_s)
            ## Getting adjacency matrix
            adjm_s<-t(adop_s)%*%(adop_s)
            #class(adjm_s)
            #attributes(adjm_s)
            #colnames(adjm_s)<-GetadNames(colnames(adjm_s))
            #rownames(adjm_s)<-GetadNames(colnames(adjm_s))
            
            ## CREATING THE NETWORK
            g1_s <- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an edge attribute, to use them later on gephi. in this step was the problem, the NA value was taken as a valid connection for the network!!
            save(g1_s,file=paste0(mode,"_fused/g1_s",y,".RData"))
            ## Communities
            clouv_s<-cluster_louvain(g1_s)
            g1_s$comm<-clouv_s
            ## Centrality
            ceg_s<-centr_eigen(g1_s, directed = FALSE, scale = TRUE, normalized = TRUE)
            
            npcml_s<-membership(clouv_s)
            npcmlv_s<-as.numeric(npcml_s)
            
            #STRUCTURAL PROPERTIES
            npb_s<-betweenness(g1_s,directed = FALSE, normalized = TRUE)# gives in the same order
            
            npd_s<-degree(g1_s,mode = c("all"))#,loops = TRUE, normalized = FALSE)
            
            np_s<-cbind(as.character(V(g1_s)$name)#list of keywords
                        ,as.character(adopn_s)# keywords to compare
                        ,as.numeric(npb_s)  # Node betweenness
                        ,as.numeric(npd_s)  # Node degree
                        #          ,as.numeric(npcm_s) # Node edge betweeness community
                        ,npcmlv_s           # Lovain community
                        ,as.numeric(ceg_s$vector)) # eigen vector centrality
            colnames(np_s) <- c("Keyword",
                                "kwValue",
                                "Betweenness CentralityI",
                                "DegreeI",
                                #"Modularity ClassIeb",
                                "Modularity ClassIL",
                                "Eigenvector Centrality")
            save(np_s,file=paste0(mode,"_fused/np_s",y,".RData"))
            
            npexp_s<-cbind(as.character(unlist(attributes(npb_s)[1])),
                           as.character(unlist(attributes(npb_s)[1])),
                           as.numeric(npb_s),
                           as.numeric(npd_s),
                           #as.numeric(npcm_s),
                           npcmlv_s,
                           as.numeric(ceg_s$vector)
                           )
                           #npsTotal_s,
                           #npcTotal_s)
            colnames(npexp_s) <- c("Id",
                                   "Label",
                                   "Betweenness CentralityI",
                                   "DegreeI",
                                   #"Modularity ClassIeb",
                                   "Modularity ClassIL",
                                   "Eigenvector Centrality")
                                   #"Subjects",
                                   #"Countries")
            write.csv(npexp_s,file=paste0(mode,"_fused/NOP_s",y,".csv"))
            
            edge1_s <- get.edgelist(g1_s, names=TRUE)# getting the Edgelist
            edgeW_s <- get.edge.attribute(g1_s,"c", index=E(g1_s))# getting the weights of each link
            
            eT_s<-vector(mode = "character",length=length(edgeW_s))
            eT_s[]<-"Undirected"
            
            edgefull1_s= cbind(edge1_s,edgeW_s,eT_s)# joining them 
            dimnames(edgefull1_s) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
            
            write.csv(edgefull1_s,file=paste0(mode,"_fused/EOP_s",y,".csv"))
            #write.csv(edgefull1_s,"Convergence/2008/EOP_s_2008.csv")
            output<-g1_s
        }
    }else{
        output<-NULL
    }
    ##-------------------------------------------------------
    return(output)
}
do_networks_Au_sparse<-function(accs,mode="NHK"){
    accs<-bbc_ws_data_l_ok_u[7][[1]]
    y<-accs[,"PY"][1]#getting the year
    x<-docvsAu_sparse(accs)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse(index,l,n)
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0(mode,"_fused/authors/ad_s",y,".RData"))
    
    ##-------------Ready to compare raw database--------------
    accsk<-cbind(accs,ad_s$k)
    colnames(accsk)<-c(colnames(accs),"comparAu")
    save(accsk,file=paste0(mode,"_fused/authors/acck",y,".RData"))
    ##-------------------------------------------------------
    ##---------------Creating the network----------------------
    
    ### Optimizing network
    ### decided to ommit optimization as some networks will be small
    ### also allows to use the future code as it is, no need to 
    ## adapt to use desriptive keywords as the network will have all
    ## keywords of the year.
    if(nrow(adv_s)>0){#the matrix has more than one document ?
        #adsum_s<-colSums(adv_s)# i get the index of the colums that dont sum and ignore them
        #adop_s<-as.matrix(adv_s[,which(adsum_s>1)])# okey!
        #adopn_s<-adn_s[which(adsum_s>1)]
        adop_s<-adv_s
        adopn_s<-adn_s
        #colnames(adop_s)<-adopn_s
        
        #adop_s<-adv_s
        #class(adop_s)
        #class(adv_s)
        
        
        if(ncol(adop_s)==0){#if there are no keywords with more than 2 co-ocurrence we can not do the network
            output<-NULL
        }else{
            ## following up with the names of the words 
            
            #adopn_s<-adn_s
            #length(adopn_s)
            ## Getting adjacency matrix
            adjm_s<-t(adop_s)%*%(adop_s)
            #class(adjm_s)
            #attributes(adjm_s)
            #colnames(adjm_s)<-GetadNames(colnames(adjm_s))
            #rownames(adjm_s)<-GetadNames(colnames(adjm_s))
            
            ## CREATING THE NETWORK
            g1_s <- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an edge attribute, to use them later on gephi. in this step was the problem, the NA value was taken as a valid connection for the network!!
            save(g1_s,file=paste0(mode,"_fused/authors/g1_s",y,".RData"))
            ## Communities
            clouv_s<-cluster_louvain(g1_s)
            g1_s$comm<-clouv_s
            ## Centrality
            ceg_s<-centr_eigen(g1_s, directed = FALSE, scale = TRUE, normalized = TRUE)
            
            npcml_s<-membership(clouv_s)
            npcmlv_s<-as.numeric(npcml_s)
            
            #STRUCTURAL PROPERTIES
            npb_s<-betweenness(g1_s,directed = FALSE, normalized = TRUE)# gives in the same order
            
            npd_s<-degree(g1_s,mode = c("all"))#,loops = TRUE, normalized = FALSE)
            
            np_s<-cbind(as.character(V(g1_s)$name)#list of keywords
                        ,as.character(adopn_s)# keywords to compare
                        ,as.numeric(npb_s)  # Node betweenness
                        ,as.numeric(npd_s)  # Node degree
                        #          ,as.numeric(npcm_s) # Node edge betweeness community
                        ,npcmlv_s           # Lovain community
                        ,as.numeric(ceg_s$vector)) # eigen vector centrality
            colnames(np_s) <- c("Author",
                                "AuValue",
                                "Betweenness CentralityI",
                                "DegreeI",
                                #"Modularity ClassIeb",
                                "Modularity ClassIL",
                                "Eigenvector Centrality")
            save(np_s,file=paste0(mode,"_fused/authors/np_s",y,".RData"))
            
            npexp_s<-cbind(as.character(unlist(attributes(npb_s)[1])),
                           as.character(unlist(attributes(npb_s)[1])),
                           as.numeric(npb_s),
                           as.numeric(npd_s),
                           #as.numeric(npcm_s),
                           npcmlv_s,
                           as.numeric(ceg_s$vector)
            )
            #npsTotal_s,
            #npcTotal_s)
            colnames(npexp_s) <- c("Id",
                                   "Label",
                                   "Betweenness CentralityI",
                                   "DegreeI",
                                   #"Modularity ClassIeb",
                                   "Modularity ClassIL",
                                   "Eigenvector Centrality")
            #"Subjects",
            #"Countries")
            write.csv(npexp_s,file=paste0(mode,"_fused/authors/NOP_s",y,".csv"))
            
            edge1_s <- get.edgelist(g1_s, names=TRUE)# getting the Edgelist
            edgeW_s <- get.edge.attribute(g1_s,"c", index=E(g1_s))# getting the weights of each link
            
            eT_s<-vector(mode = "character",length=length(edgeW_s))
            eT_s[]<-"Undirected"
            
            edgefull1_s= cbind(edge1_s,edgeW_s,eT_s)# joining them 
            dimnames(edgefull1_s) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
            
            write.csv(edgefull1_s,file=paste0(mode,"_fused/authors/EOP_s",y,".csv"))
            #write.csv(edgefull1_s,"Convergence/2008/EOP_s_2008.csv")
            output<-g1_s
        }
    }else{
        output<-NULL
    }
    ##-------------------------------------------------------
    return(output)
}

## load all ads, and 
load("ai_fb_kwd_m.rdata")
load("ai_sb_kwd_m.rdata")
mode<-"ai_fb"
DVKWfb<-docvsKw_sparse_parallel(ai_fb_kwd_m,mode)
DVKWsb<-docvsKw_sparse_parallel(ai_sb_kwd_m,mode)
save(DVKWsb,file="DVKWsb.rdata")
save(DVKWfb,file="DVKWfb.rdata")

#you get all documeb vs matrxi fo each year and then do the thingy
aifb_docvsKw_l <- lapply(ai_fb_kwd_l,docvsKw_sparse_parallel)
save(aifb_docvsKw_l,file="aifb_docvsKw_l.rdata")
aisb_docvsKw_l <- lapply(ai_sb_kwd_l,docvsKw_sparse_parallel)
save(aisb_docvsKw_l,file="aisb_docvsKw_l.rdata")

## calculating the matrix multiplication for each year. 
py_fb<- py[1:24]
py_sb <- py[25:length(py)]
ai_fb_g_list<- mapply(do_networks_Kw_sparse,aifb_docvsKw_l[1:2],py_fb,MoreArgs=list(mode="ai_fb"),SIMPLIFY = F)


bbc_Au_g_list<-lapply(bbc_ws_data_l_ok_u,do_networks_Au_sparse,mode)

mode<-"NHK"
#nhk_ws_data_l_ok_u[1][[1]][,"outputAuthor"]
nhk_g_list<-lapply(nhk_ws_data_l_ok_u,do_Kw_networks_sparse,mode)
nhk_Au_g_list<-lapply(nhk_ws_data_l_ok_u,do_networks_Au_sparse,mode)
##MODIFY THESE function to accept BD.
## get the list for BD data
## calculate the accp for BD
## update accp for all data sets.

y<-as.character(2000:2016)
get_accsk_list<-function(y,mode){
    #BD_wos\2008
    if(mode=="BD"){
        load(paste0(mode,"_wos/",y,"/acck_",y,".RData"))
    }else{
        load(paste0(mode,"_fused/acck",y,".RData"))
    }
    output<-accsk
    return(output)
}
get_np_list<-function(y,mode){
    if(mode=="BD"){
        load(paste0(mode,"_wos/",y,"/np_s_",y,".RData"))
        #load("BD_wos/2008/np_s_2008.RData")
    }else{
        load(paste0(mode,"_fused/np_s",y,".RData"))
    }
    output<-np_s
  return(output)
}

y<-as.character(2000:2016)
mode<-"BBC"
bbc_accsk_list<-lapply(y,get_accsk_list,mode)
bbc_np_list<-lapply(y,get_np_list,mode)

mode<-"NHK"
nhk_accsk_list<-lapply(y,get_accsk_list,mode)
nhk_np_list<-lapply(y,get_np_list,mode)

mode<-"BD"
y<-as.character(2008:2016)
bd_accsk_list<-lapply(y,get_accsk_list,mode)
bd_np_list<-lapply(y,get_np_list,mode)
##mapply function
get_accp_list<-function(accsk_l,np_l,type){
  accp_s<-rdbSetKwCluster(accsk_l,np_l,"Modularity ClassIL")
  output<-accp_s
  return(output)
}

bbc_accsk_list
# recover mtrxi form
bd_np_list
colnames(bd_np_list[1][[1]])
table(bbc_accsk_m[,"PY"])

nhk_accsk_m <- do.call(rbind,nhk_accsk_list)
table(nhk_accsk_m[,"PY"])

bd_accsk_m <- do.call(rbind,bd_accsk_list)
table(bd_accsk_m[,"PY"])
colnames(bd_accsk_m)
get_docs_by_kw <- function(tkw,accsk_m){
    accsk_m <- bd_accsk_m
    tkw <- "augmentedreality"
    p <- paste0("\\b",tkw,"\\b")
    t <- grepl(p, accsk_m[,"comparKw"])
    o <- accsk_m[t,c("outputTitle","comparKw","PY")]
    to <- table(o[,"PY"])
    o[o[,"PY"]=="2012","outputTitle"]
    o[o[,"PY"]=="2013","outputTitle"]
    o[o[,"PY"]=="2014","outputTitle"]
    o[o[,"PY"]=="2015","outputTitle"]
    o[o[,"PY"]=="2016","outputTitle"]
}

nhk_accsk_list
bd_accsk_list

##color source 
#https://www.w3schools.com/colors/colors_picker.asp

type="Modularity ClassIL"
bbc_accp_list<-mapply(get_accp_list,bbc_accsk_list,bbc_np_list,type,SIMPLIFY = F)
nhk_accp_list<-mapply(get_accp_list,nhk_accsk_list,nhk_np_list,type,SIMPLIFY = F)
bd_accp_list<-mapply(get_accp_list,bd_accsk_list,bd_np_list,type,SIMPLIFY = F)

# function applied only to BBC and NHK 
# lapply function
get_unique_authors_by_year<-function(accp_l){
#    accp_l<-bbc_accp_list[1][[1]]
    # get the author fields for all papers
    a_set<-accp_l[,"outputAuthor"]
    # split them 
    # eliminate repeated. 
    output<-fieldSplitterUnique(a_set)
    # output the author list
    return(output)
}
get_unique_authors<-function(accp_l){
    #accp_l<-bbc_accp_list
    buff<-unlist(lapply(accp_l,get_unique_authors_by_year))
    output<-unique(buff)
    return(output)
}

bbc_authors<-sort(get_unique_authors(bbc_accp_list))#613
nhk_authors<-sort(get_unique_authors(nhk_accp_list))#1514

## TO DO
### compare this list with the list obtained by scopus
### ---------ready the author list obtained by scopus
### load lists
### First transform the list from scopus into last name one first name.
### eliminate dots and commas. 

#lapply function
#input n_v vector of names
#output
#names formated to wos and scopus
get_names_all_initials<-function(n_v){
    #n_v<-"Wright, Dan T."
    #first part is first name
    #second part is middle and last name info
    ## from second part get only the upper case letter characters.
    # fuse the resulting firs part and second part levaing a space between them
    # pass to lower case. 
    # add the column to the original table
    # eliminate repeated from the original table
    # check from the basic form, which other authors can be fused. 
    f_n<-unlist(strsplit(n_v,", "))
    first_n<-f_n[1]
    other_n<-gsub("[^A-Z]|\\.| ","",f_n[2])
    c_n<-paste(first_n,other_n)
    c_n_l<-tolower(c_n)
    output<-c_n_l
    return(output)
}
read_author_csv_file<-function(mode){
    a_raw<-read.csv(paste0(mode,"_authors/",mode,"_authors.csv"),header = T,stringsAsFactors=F)
    #class(a_raw)
    colnames(a_raw)<-c("author_names","auth_ID","docs_num","subject")
    a_data<-a_raw[,c("author_names","auth_ID","docs_num","subject")]
    a_data[,"subject"]<-gsub("\\n$","",a_data[,"subject"])
    a_data[,"subject"]<-gsub("\\n"," | ",a_data[,"subject"])
    ## creating a new column to compare 
    a_n<-a_data[,"author_names"]
    #split after the comma
    a_names<-unlist(lapply(a_n,get_names_all_initials))
    output<-cbind(a_data,auth_name_compar=a_names,stringsAsFactors=F)
    #data.frame()
    return(output)
}
get_unique_author_names<-function(ref_a_n){
    #ref_a_n<-nhk_ref_author_names
    ind<-which(!duplicated(ref_a_n[,"author_names"]))
    output<-ref_a_n[ind,]
    return(output)
}
get_target_authors<-function(author_l,ref_author_l){
    #author_l<-nhk_authors
    #ref_author_l<-nhk_ref_author_names_u
    ref_author_l[,"auth_name_compar"]<-as.character(ref_author_l[,"auth_name_compar"])
    #ind<-author_l%in%ref_author_l[,"auth_name_compar"]
    #length(which(ind))#506
    #unique(author_l[ind])
    ind<-ref_author_l[,"auth_name_compar"]%in%author_l
    #unique(ref_author_l[ind,"auth_name_compar"])
    repeated<-length(which(duplicated(ref_author_l[ind,"auth_name_compar"])))
    output<-ref_author_l[ind,]# the data i need, but contains repeated instances. 
    return(output)
}

#lapply function
#sa stands for simplified author
get_target_authors_broad_indetail<-function(sa,ref_author_l){
    # for each element in sal, search for the authors in ref that 
    #ref_author_l<-bbc_ref_author_names_u
    #sa<-sal[1]
    buff<-vector("character",ncol(ref_author_l))
    names(buff)<-colnames(ref_author_l)
    # match by regex. 
    ind<-grep(sa,ref_author_l[,"auth_name_compar"])
    # retreive their information, that will form the final matrix
    if(length(ind)!=0){
        #ok
        output<-ref_author_l[ind,]
    }else{
        output<-buff
    }
    return(output)
}
simplify_name<-function(name_v){
    #name_v<-author_l[540]author_l[539]
    fn<-unlist(strsplit(name_v," "))
    if(length(fn)>2){
        fstn<-paste(fn[1:(length(fn)-1)],collapse = " ")
    }else{
        fstn<-fn[1]
    }
    ln<-substring(fn[length(fn)],1,1)
    output<-paste(fstn,ln)
    return(output)
}
get_target_authors_broad<-function(author_l,ref_author_l){
    #i can simplify the seed of the search ... author_l
    #already expanded as much as possible the reference
    #to simplify the seed, get the names, break them by space
    #get the last element only the first character
    #sal stands for simplyfied author list
    sal<-unlist(lapply(author_l,simplify_name))
    #ref_author_l<-bbc_ref_author_names_u
    target_author_info_l<-lapply(sal,get_target_authors_broad_indetail,ref_author_l)
    target_author_info<-do.call(rbind,target_author_info_l)#624
    target_author_info_c<-target_author_info[target_author_info[,"auth_name_compar"]!="",]
    #have to eliminate an aditional step of repeated, 
    #is the effect of over simplification of the seed author name, some of them 
    #call more than one name and in these repeated names might come. 
    target_author_info_c_u<-target_author_info_c[!duplicated(target_author_info_c[,"author_names"]),]
    output<-target_author_info_c_u
    return(output)
}

mode<-"NHK"
nhk_ref_author_names<-read_author_csv_file(mode)#1555
nhk_ref_author_names_u<-get_unique_author_names(nhk_ref_author_names)#1224
##----Comparing the extracted authors VS the whole lists of authors----
#nhk_authors_data<-get_target_authors(nhk_authors,nhk_ref_author_names_u)#669
nhk_authors_data<-get_target_authors_broad(nhk_authors,nhk_ref_author_names_u)#761/705

mode<-"BBC"
bbc_ref_author_names<-read_author_csv_file(mode)#771
bbc_ref_author_names_u<-get_unique_author_names(bbc_ref_author_names)#753

##----Comparing the extracted authors VS the whole lists of authors----
#bbc_authors_data<-get_target_authors(bbc_authors,bbc_ref_author_names_u)#102
bbc_authors_data<-get_target_authors_broad(bbc_authors,bbc_ref_author_names_u)#134/130

## for these two variables add the R&D line information
save(bbc_authors_data,file="bbc_authors_data.rdata")
write.csv(bbc_authors_data,file="bbc_authors_data.csv")

save(nhk_authors_data,file="nhk_authors_data.rdata")
write.csv(nhk_authors_data,file="nhk_authors_data.csv")
## after reload them and pass that info as properties to the accsk object for each paper    

##---------------------------------------------------------------------

## Create co-author ship network of the researchers of interest only
## i can create the big network including all years
## i can create network by year

## i prefer to do all of them together
## like this we can see the connections of the key persons around time
## in one shot. 
## also the persons analized should be only those who belong to 
## the institution of interest, as we do not want to know about the others.
## and those add noise to the visualization. 
## HOW TO CREATE A BIG AUTHOR NETWORK
## FUSE all accsk
## run the author network functions only once for that big accsk.
## if you have a list of accsk, get it and do.call it.
nhk_accp_list
bbc_accp_list
bd_accp_list

nhk_accp_m<-do.call(rbind,nhk_accp_list)
bbc_accp_m<-do.call(rbind,bbc_accp_list)
bd_accp_m<-do.call(rbind,bd_accp_list)



nhk_authors_seed<-nhk_authors_data[,"auth_name_compar"]
bbc_authors_seed<-bbc_authors_data[,"auth_name_compar"]

#ac<-accs

reconstruct_i<-function(index,l){
    y<-vector("numeric",length(l))
    y[]<-index
    output<-y
    return(output)
}
get_dvkw_sparse<-function(index,l,n){
    temp_i<-mapply(reconstruct_i,index,l,SIMPLIFY=F)
    i<-do.call(c,temp_i)
    j<-do.call(c,l)
    buff<-sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(index,names(n)))
    output<-buff
    return(output)
}
docvsAu_sparse_all<-function(ac,as="",rmkey="no"){#ac
    #rmkey="no"
    #ac<-bbc_accp_m
    #as<-bbc_authors_seed
    #rmkey<-"no"
    #Yeah I already obtanied those researchers who are from the organization 
    #In the vector of author seed, no need to repeat. 
    #Now i just have to modify the regular expression.
    #ac[1,]
    zltue <- prePmix(as)#all unique keywords
    names(zltue)<-as
    ## REGULAR  expression proof comparison
    #Hudson AD
    #Creating the Doc Vs word matrix
    #number of keywords length(zu)
    #number of docs dim(ac)[1]
    dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
    #h14_dID<-as(h14_dID, "sparseMatrix")
    kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
    # This loop in each document, or element of ac
    #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
    #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
    #all(ylt == yltb)
    for(i in 1:dim(ac)[1]){
        #i=3
        ylt<-prePmix(unlist(strsplit(ac[i,"outputAuthor"],"|",fixed = T)))#all keywords of a document
        kwv[i]<-fieldMerger(ylt)
        ##This line is for the regular expression matching between keywords.
        #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
        ##-------------------------------------------------------------------
        thing<-vector("numeric")
        #for the keywords of each document search for the matchs
        for(j in 1:length(ylt)){        
            #j=3
            buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
            thing<-c(thing,buff)#the index of the words that are present
        }
        ##-------------------------------------------------------------------
        dvk[i][[1]]<-thing
    }
    output<-list(v=dvk,n=zltue,k=kwv)
    return(output)
    #return(thing)
}
do_networks_Au_sparse_all<-function(accs,mode="NHK",as=""){
    #mode="NHK"
    #accs<-nhk_accp_m
    #class(accs)
    #colnames(accs)
    #as<-nhk_authors_seed
    #y<-accs[,"PY"][1]#getting the year
    y<-"all"
    x<-docvsAu_sparse_all(accs,as)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse(index,l,n)
    
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0(mode,"_fused/authors/ad_s",y,".RData"))
    
    ##-------------Ready to compare raw database--------------
    accsk<-cbind(accs,ad_s$k)
    colnames(accsk)<-c(colnames(accs),"comparAu")
    save(accsk,file=paste0(mode,"_fused/authors/acck",y,".RData"))
    ##-------------------------------------------------------
    ##---------------Creating the network----------------------
    
    ### Optimizing network
    ### decided to ommit optimization as some networks will be small
    ### also allows to use the future code as it is, no need to 
    ## adapt to use desriptive keywords as the network will have all
    ## keywords of the year.
    if(nrow(adv_s)>0){#the matrix has more than one document ?
        #adsum_s<-colSums(adv_s)# i get the index of the colums that dont sum and ignore them
        #adop_s<-as.matrix(adv_s[,which(adsum_s>1)])# okey!
        #adopn_s<-adn_s[which(adsum_s>1)]
        adop_s<-adv_s
        adopn_s<-adn_s
        #colnames(adop_s)<-adopn_s
        
        #adop_s<-adv_s
        #class(adop_s)
        #class(adv_s)
        
        
        if(ncol(adop_s)==0){#if there are no keywords with more than 2 co-ocurrence we can not do the network
            output<-NULL
        }else{
            ## following up with the names of the words 
            
            #adopn_s<-adn_s
            #length(adopn_s)
            ## Getting adjacency matrix
            adjm_s<-t(adop_s)%*%(adop_s)
            #class(adjm_s)
            #attributes(adjm_s)
            #colnames(adjm_s)<-GetadNames(colnames(adjm_s))
            #rownames(adjm_s)<-GetadNames(colnames(adjm_s))
            
            ## CREATING THE NETWORK
            g1_s <- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an edge attribute, to use them later on gephi. in this step was the problem, the NA value was taken as a valid connection for the network!!
            save(g1_s,file=paste0(mode,"_fused/authors/g1_s",y,".RData"))
            ## Communities
            clouv_s<-cluster_louvain(g1_s)
            g1_s$comm<-clouv_s
            ## Centrality
            ceg_s<-centr_eigen(g1_s, directed = FALSE, scale = TRUE, normalized = TRUE)
            
            npcml_s<-membership(clouv_s)
            npcmlv_s<-as.numeric(npcml_s)
            
            #STRUCTURAL PROPERTIES
            npb_s<-betweenness(g1_s,directed = FALSE, normalized = TRUE)# gives in the same order
            
            npd_s<-degree(g1_s,mode = c("all"))#,loops = TRUE, normalized = FALSE)
            
            np_s<-cbind(as.character(V(g1_s)$name)#list of keywords
                        ,as.character(adopn_s)# keywords to compare
                        ,as.numeric(npb_s)  # Node betweenness
                        ,as.numeric(npd_s)  # Node degree
                        #          ,as.numeric(npcm_s) # Node edge betweeness community
                        ,npcmlv_s           # Lovain community
                        ,as.numeric(ceg_s$vector)) # eigen vector centrality
            colnames(np_s) <- c("Author",
                                "AuValue",
                                "Betweenness CentralityI",
                                "DegreeI",
                                #"Modularity ClassIeb",
                                "Modularity ClassIL",
                                "Eigenvector Centrality")
            save(np_s,file=paste0(mode,"_fused/authors/np_s",y,".RData"))
            
            npexp_s<-cbind(as.character(unlist(attributes(npb_s)[1])),
                           as.character(unlist(attributes(npb_s)[1])),
                           as.numeric(npb_s),
                           as.numeric(npd_s),
                           #as.numeric(npcm_s),
                           npcmlv_s,
                           as.numeric(ceg_s$vector)
            )
            #npsTotal_s,
            #npcTotal_s)
            colnames(npexp_s) <- c("Id",
                                   "Label",
                                   "Betweenness CentralityI",
                                   "DegreeI",
                                   #"Modularity ClassIeb",
                                   "Modularity ClassIL",
                                   "Eigenvector Centrality")
            #"Subjects",
            #"Countries")
            write.csv(npexp_s,file=paste0(mode,"_fused/authors/NOP_s",y,".csv"))
            
            edge1_s <- get.edgelist(g1_s, names=TRUE)# getting the Edgelist
            edgeW_s <- get.edge.attribute(g1_s,"c", index=E(g1_s))# getting the weights of each link
            
            eT_s<-vector(mode = "character",length=length(edgeW_s))
            eT_s[]<-"Undirected"
            
            edgefull1_s= cbind(edge1_s,edgeW_s,eT_s)# joining them 
            dimnames(edgefull1_s) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
            
            write.csv(edgefull1_s,file=paste0(mode,"_fused/authors/EOP_s",y,".csv"))
            #write.csv(edgefull1_s,"Convergence/2008/EOP_s_2008.csv")
            output<-g1_s
        }
    }else{
        output<-NULL
    }
    ##-------------------------------------------------------
    return(output)
}

mode<-"NHK"
nhk_an_all<-do_networks_Au_sparse_all(nhk_accp_m,mode,nhk_authors_seed)

mode<-"BBC"
bbc_an_all<-do_networks_Au_sparse_all(bbc_accp_m,mode,bbc_authors_seed)
### those who are missing might have joined after 2017.

##updating author np with the R&D profile data
# format the update files 
## create a modularity label
#### to build the profile
##### create a histogram of the labels they got that is their profile.
simplify_names_general<-function(a){
    as<-unlist(strsplit(a," "))
    aln<-as[length(as)]
    arn<-as[1:(length(as)-1)]
    if(length(arn)>1){
        arni<-vector("character",length(arn))
        for(i in 1:length(arn)){
            #get only the first letter
            arni[i]<-substring(arn[i],1,1)
        }
        arnis<-paste0(arni,collapse = "")
    }else{
        arnis<-substring(arn,1,1)
    }
    output<-tolower(paste(aln,arnis,collapse = " "))
    return(output)
}
get_mods_rd_profiles<-function(mod,new_np){
    #mod<-mods[32]
    #get all the RD elements with that mod
    buff<-new_np[new_np[,"Modularity_ClassIL"]==mod,"RD"]
    #separate empty and with values elements
    no_cero_data<-buff[buff!=""]
    no_cero_count<-length(which(buff!=""))
    cero_count<-length(which(buff==""))
    #count the empty elements of the community
    #those with values are used for the profile
    data<-sort(table(unlist(lapply(no_cero_data,fieldSplitter))),decreasing = T)
    
    ex_data<-c(data,no_cero_count,cero_count,mod)
    names(ex_data)<-c(names(data),"no_zero","cero","mod")
    
    output<-ex_data
    return(output)
}
acom_rd_profiles<-function(new_np){
    #colnames(new_nhk_np)
    #new_np<-new_bbc_np
    mods<-sort(unique(new_np[new_np[,"RD"]!="","Modularity_ClassIL"]))
    #for each mode, get the profiles
    
    #lapply function
    output<-lapply(mods,get_mods_rd_profiles,new_np)
    return(output)
}
np_rd_update<-function(mode){
    #load the update csv
    #mode<-"BBC"
    x<-read.csv(paste0(mode,"_authors/",mode,"_authors_data_rd_update.csv"),
                header = T,
                stringsAsFactors = F)
    
    
    #load np
    #add column
    y<-read.csv(paste0(mode,"_fused/authors/NOP_sall.csv"),
                header = T,
                stringsAsFactors = F)
    #add column
    
    z<-cbind(y,RD=x[,"labels"])
    data<-z[,c("Id",
               "Label",
               "Betweenness.CentralityI",
               "DegreeI",
               "Modularity.ClassIL",
               "Eigenvector.Centrality",
               "RD")]
    data[,"RD"]<-as.character(data[,"RD"])
    format_flag<-","
    if(mode=="BBC"){
        format_flag<-"\n"
    }
    data[,"RD"]<-gsub(format_flag," | ",data[,"RD"])
    colnames(data)<-c("Id",
                      "Label",
                      "Betweenness_CentralityI",
                      "DegreeI",
                      "Modularity_ClassIL",
                      "Eigenvector_Centrality",
                      "RD")
    output<-data
    return(output)
}

mode<-"NHK"
new_nhk_np<-np_rd_update(mode)
nhk_rd_profiles<-acom_rd_profiles(new_nhk_np)
save(nhk_rd_profiles,file=paste0("NHK_fused/authors/nhk_rd_profiles.RData"))

mode<-"BBC"
new_bbc_np<-np_rd_update(mode)
bbc_rd_profiles<-acom_rd_profiles(new_bbc_np)
save(bbc_rd_profiles,file=paste0("BBC_fused/authors/bbc_rd_profiles.RData"))

###-------------------------------------------------------------------###
# PASSING TO THE KEYWORD LINK DETECTION

##----------------------------------------------------#
##              CLUSTER INTERNAL OPERATIONS           #
##****************************************************#
kwVsCoun_s<-""
kwVsSubj_s<-""
type="Modularity ClassIL"
y<-as.character(2000:2016)
mode<-"BBC"
#mapply function
get_cp_list<-function(np_l,kwVsCoun_s,kwVsSubj_s,accp_l,g_l,type,y,mode){
  cp_s<-GetClusterP(np_l,kwVsCoun_s,kwVsSubj_s,accp_l,g_l,type,"all")
  save(cp_s,file=paste0(mode,"_fused/cp_s",y,".RData"))
  output<-cp_s
  return(output)
}
y<-as.character(2000:2016)
mode<-"BBC"
bbc_cp_list<-mapply(get_cp_list,bbc_np_list,kwVsCoun_s,kwVsSubj_s,bbc_accp_list,bbc_g_list,type,y,mode,SIMPLIFY = F)
y<-as.character(2000:2016)
mode<-"NHK"
nhk_cp_list<-mapply(get_cp_list,nhk_np_list,kwVsCoun_s,kwVsSubj_s,nhk_accp_list,nhk_g_list,type,y,mode,SIMPLIFY = F)

##----------------------------------------------------#
##            MULTIMODE NETWORK ALL DATA IN           #
##****************************************************#
# raw data sets to build the general multimode network. 
nrow(nhk_accp_m)#1065 - 1:1065
as.character(nhk_accp_m[nrow(nhk_accp_m),"comparKw"])

nrow(bbc_accp_m)#204  - 1066:1269
as.character(bbc_accp_m[nrow(bbc_accp_m),"comparKw"])

nrow(bd_accp_m)#33179 - 1270:34448
as.character(bd_accp_m[nrow(bd_accp_m),"comparKw"])



GetKw_opt<-function(ac, rmkey="no"){
    z<-unlist(strsplit(ac[,"outputKw"],"|",fixed = TRUE)) # sapply to get the big vector 5052
    #Data processing
    # Db pedia
    # leveisntein distance
    # General word processing
    ## lower case
    zl<-tolower(z)#2761 - 2012
    ##Recovering keywords
    
    ## remove trailing and leading white space
    zlt<-zl<-trim(zl)#2761 - 2012
    ## remove all white spaces
    zlts<-rmSpace(zlt)#2741 - 2012
    ## Removing punctuation&special characters
    zltsp<-rmPunct(zlts)#2705 - 2012
    
    ## stemming - not very well taken by the keywords
    #zltsps<-wordStem(zltsp, language = "porter")
    #write.csv(cbind(zltsps,zltsp), file = "words.csv")
    
    ## getting unique keywords
    
    zltspu<-unique(zltsp)
    #names(zltspu)<-unlist(lapply(zltspu,kwSearch,v=zltsp,z=z))
    
    ## show how the words were grouped
    ### value = final word after processing, name = all the words grouped by that one
    ### value = unique words, name = words belonging to the index with that same unique word in z
    #rmkey="big data"
    
    #Remplace the special characters (+) (PLUS)
    #zltspus<-gsub("\\+","(PLUS)",zltspu)
    if(rmkey=="no"){
        output <- zltspu
    }else{
        output <- zltspu[zltspu != rmkey]
    }
    
    return(output)
}
docvsKw_sparse_parallel<-function(ac, rmkey="no"){#ac
    #rmkey="no"
    #ac<-nhk_accp_e
    zltue <- GetKw_opt(ac, rmkey)#all unique keywords
    
    ## REGULAR  expression proof comparison
    
    #Creating the Doc Vs word matrix
    #number of keywords length(zu)
    #number of docs dim(ac)[1]
    dvk<-vector("list", dim(ac)[1])#contains the i and j elements that are different from 0
    #h14_dID<-as(h14_dID, "sparseMatrix")
    #kwv<-vector("character",dim(ac)[1])# contains the values to fill the said elements
    # This loop in each document, or element of ac
    #ylt<-rmPunct(rmSpace(trim(tolower(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE))))))
    #yltb<-prePmix(unlist(strsplit(ac[1,"outputKw"],"|",fixed = TRUE)))
    #all(ylt == yltb)
    cl <- makeCluster(4) # create a cluster with 2 cores
    registerDoParallel(cl) # register the cluster
    #.combine="rbind",.packages="quantreg"
    res = foreach(i = 1:dim(ac)[1]) %dopar% 
        {
            #i<-130
            # define premix
            trim <- function (x) gsub("^\\s+|\\s+$", "", x)
            rmSpace<-function(x){gsub("\\s", "", x)}
            rmPunct<-function(x){gsub("[[:punct:]]","",x)}
            prePmix<-function(x){rmPunct(rmSpace(trim(tolower(x))))}
            
            # define fieldMerger
            # #fieldMerger<-function(x){
            #     output<-paste(x,collapse = " | ")
            #     return(output)
            # }
            
            ylt<-prePmix(unlist(strsplit(ac[i,"outputKw"],"|",fixed = T)))#all keywords of a document
            #kwv[i]<-fieldMerger(ylt)
            ##This line is for the regular expression matching between keywords.
            #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
            ##-------------------------------------------------------------------
            thing<-vector("numeric")
            for(j in 1:length(ylt)){        
                #j=2
                #buff<-grep(paste0("^",ylt[j],"$"), zltue)##This lines is for exact match keywords
                buff1<-which(zltue==ylt[j])
                thing<-c(thing,buff1)#the index of the words that are present
            }
            ##-------------------------------------------------------------------
            dvk[i][[1]]<-thing
        }
    stopCluster(cl) # shut down the cluster
    output<-list(v=res,n=zltue)
    return(output)
    #return(thing)
}#grr

get_dvkw_sparse_opt<-function(index,l,n){
    temp_i<-mapply(reconstruct_i,index,l,SIMPLIFY=F)
    i<-do.call(c,temp_i)
    j<-do.call(c,l)
    buff<-sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(index,n))
    output<-buff
    return(output)
}#ok
do_multimode_Kw_sparse<-function(accs_a,accs_b,accs_c){
    accs_a<-nhk_accp_m
    accs_b<-bbc_accp_m
    accs_c<-bd_accp_m
    
    accs_a_e<-as.matrix(nhk_accp_m[,c("outputKw","comparKw")])
    accs_b_e<-as.matrix(bbc_accp_m[,c("outputKw","comparKw")])
    accs_c_e<-as.matrix(bd_accp_m[,c("outputKw","comparKw")])
    coor_a<-nrow(accs_a_e)#1065 / 1-1065
    coor_b<-nrow(accs_b_e)#204  / 1066-1269
    coor_c<-nrow(accs_c_e)#33179/ 1270-34448

    mn_accp<-rbind(accs_a_e,accs_b_e,accs_c_e)
    mn_accp[]<-as.matrix(mn_accp[,c("outputKw","comparKw")])

    accs<-mn_accp
    y<-"mm"#getting the year
    x<-docvsKw_sparse_parallel(accs)# takes time
    #stopCluster(cl)
    #x<-docvsKw_sparse(accs)
    l<-x$v
    index<-1:length(l)
    n<-x$n
    x$v<-get_dvkw_sparse_opt(index,l,n)
    z_s<-x
    #----------------------ANALYSIS PART-----------------#
    ad_s<-z_s
    adv_s<-ad_s$v#matrix doc vs keywords
    adn_s<-ad_s$n#names of the compared keywords
    save(ad_s,file=paste0("Multimode_Network/ad_s",y,".RData"))
    
    ##---------------Creating the network----------------------
    ### Optimizing network
    ## big network, better to optimize
    ## get only those keywords that generate links
    ## get only papers which are linked so the optimization is 
    ## >1
    adv_s_o<-adv_s[,colSums(adv_s)>1]
    adn_s_o<-adn_s[colSums(adv_s)>1]
    ## UPDATE TO A MULTIMODE NETWORK , INCIDENCE MATRIX CREATION
    ## COMMUNITIES AND CENTRALITIES MEASURES LOSE MEANING
    
    ## CREATING THE NETWORK
    g<-graph.incidence(adv_s_o, directed = F,weighted="c")
    
    ## Communities
    ## MARKING THE TYPE OF NODES
    # mark the type of nodes
    a_nodes<-vector("character",coor_a)#1065
    a_nodes[]<-"a"
    
    b_nodes<-vector("character",coor_b)#204
    b_nodes[]<-"b"
    
    c_nodes<-vector("character",coor_c)#33179
    c_nodes[]<-"c"
    
    kw_nodes<-vector("character",length(adn_s_o))
    kw_nodes[]<-"kw"
    #1-1065
    #1066-1269
    #1270-34448
    
    V(g)$nt[1:length(names(V(g)))]<-""#node type
    V(g)$py[1:length(names(V(g)))]<-""#publication year - papers
    V(g)$mv[1:length(names(V(g)))]<-""#by year modularity - papers
    V(g)$au[1:length(names(V(g)))]<-""#for a&b papers authors
    V(g)$al[1:length(names(V(g)))]<-0#number of links to a nodes
    V(g)$bl[1:length(names(V(g)))]<-0#number of links to b nodes
    V(g)$cl[1:length(names(V(g)))]<-0#number of links to c nodes
    V(g)$yla_c[1:length(names(V(g)))]<-""#label of the different years linked [a-c or b-c]
    V(g)$yna_c[1:length(names(V(g)))]<-0#number different years linked [a-c or b-c]
    V(g)$ylb_c[1:length(names(V(g)))]<-""#label of the different years linked [a-c or b-c]
    V(g)$ynb_c[1:length(names(V(g)))]<-0#number different years linked [a-c or b-c]
    
    ## GETTING PAPERS AND NODES's PROPERTIES 
    #Papers 

    # Filling node type property
    V(g)$nt[1:coor_a]<-a_nodes
    V(g)$nt[(coor_a+1):((coor_a)+(coor_b))]<-b_nodes
    V(g)$nt[(((coor_a)+(coor_b))+1):(((coor_a)+(coor_b))+coor_c)]<-c_nodes
    V(g)$nt[((((coor_a)+(coor_b))+coor_c)+1):length(names(V(g)))]<-kw_nodes
    
    # Filling publication year
    ## applies only to papers a,b,c paper nodes
    V(g)$py[1:coor_a]<-accs_a[,"PY"]
    V(g)$py[(coor_a+1):((coor_a)+(coor_b))]<-accs_b[,"PY"]
    V(g)$py[(((coor_a)+(coor_b))+1):(((coor_a)+(coor_b))+coor_c)]<-accs_c[,"PY"]
    
    # Filling by year modularity
    V(g)$mv[1:coor_a]<-accs_a[,"mv"]
    V(g)$mv[(coor_a+1):((coor_a)+(coor_b))]<-accs_b[,"mv"]
    V(g)$mv[(((coor_a)+(coor_b))+1):(((coor_a)+(coor_b))+coor_c)]<-accs_c[,"mv"]
    
    # Filling a and b sets authors
    V(g)$au[1:coor_a]<-accs_a[,"outputAuthor"]
    V(g)$au[(coor_a+1):((coor_a)+(coor_b))]<-accs_b[,"outputAuthor"]
    
    # Filling the keyword properties
    # get all keywords
    #which(V(g)$nt=="kw")
    v<-V(g)[V(g)$nt=="kw"]
    #lapply function to check the keywords connections
    get_kw_nodes_p<-function(v,g){
        #for each keyword
        #g<-g
        #v<-V(g)[V(g)$nt=="kw"][1]
        #get the ni
        #lapply function
        av<-as.numeric(unlist(adjacent_vertices(g,v)))
        avl<-V(g)$nt[av]
        # from the neighbors properties get the count for a,b and c 
        avla<-length(which(avl=="a"))
        avlb<-length(avl[avl=="b"])
        avlc<-length(avl[avl=="c"])
        # set them as a matrix and sent it as output
        
        output<-c(avla,avlb,avlc)
        names(output)<-c("al","bl","cl")
        return(output)
    }
    kwnp<-lapply(v,get_kw_nodes_p,g)# takes time
    kwnp_m<-do.call(rbind,kwnp)
    #update all keywords with their info
    V(g)$al[V(g)$nt=="kw"]<-as.numeric(kwnp_m[,"al"])
    V(g)$bl[V(g)$nt=="kw"]<-as.numeric(kwnp_m[,"bl"])
    V(g)$cl[V(g)$nt=="kw"]<-as.numeric(kwnp_m[,"cl"])
    
    # Filling the interest linking words properties.
    # get words which have connections to a and c
    which(V(g)$al!=0)
    which(V(g)$bl!=0)
    which(V(g)$cl!=0)
    
    #any((V(g)$nt=="kw")&(V(g)$al!=0)&(V(g)$cl!=0))
    #V(g)[(V(g)$nt=="kw")&(V(g)$al!=0)&(V(g)$cl!=0)]
    
    nac<-V(g)[(V(g)$nt=="kw")&(V(g)$al!=0)&(V(g)$cl!=0)]#752
    #nac<-V(g)[((V(g)$nt=="kw")&(V(g)$al!="0")&(V(g)$cl!="0"))]
    # get words which have connections to b and c
    #nbc<-V(g)[((V(g)$nt=="kw")&(V(g)$bl!="0")&(V(g)$cl!="0"))]
    nbc<-V(g)[((V(g)$nt=="kw")&(V(g)$bl!=0)&(V(g)$cl!=0))]#377
    
    #lapply to get the years each 
    ### TO DO 
    ## assign the properties to the keyword nodes
    ## start analysis of the network
    
    get_links_years<-function(nc,g,mode="a"){
        #nc<-nbc[1]
        ncv<-V(g)[V(g)==nc]
        #get the adjacent papers
        #neighbors(g,ncv)
        pncv<-as.numeric(unlist(adjacent_vertices(g,ncv)))
        #from these papers who are from b????
        #from the b papers get the dates
        from_name2index<-function(nn,g){
            #nn<-pncv[4]
            output<-which(V(g)==nn)
            return(output)
        }
        ni<-unlist(lapply(pncv,from_name2index,g))
        pn<-which(V(g)$nt[ni]==mode)#
        #from the adjacent papers get their PY
        pny<-V(g)$py[ni[pn]]
        #get the unique labels
        ty<-unique(pny)
        ny<-length(ty)
        ly<-paste0(unique(pny),collapse = " | ")
        output<-c(ly,ny)
        names(output)<-c("ly","ny")
        return(output)
    }
    #for the vertex equal to nac assign their ya
    mode<-"a"
    ya<-lapply(nac,get_links_years,g,mode)#takes time
    ya_m<-do.call(rbind,ya)
    V(g)$yla_c[nac]<-ya_m[,"ly"]
    V(g)$yna_c[nac]<-as.numeric(ya_m[,"ny"])
    

    #for the vertex equal to nbc assign their yb
    mode<-"b"
    yb<-lapply(nbc,get_links_years,g,mode)#takes time
    yb_m<-do.call(rbind,yb)
    V(g)$ylb_c[nbc]<-yb_m[,"ly"]
    V(g)$ynb_c[nbc]<-as.numeric(yb_m[,"ny"])
    
    # saving the g object after calculating all properties
    save(g,file=paste0("Multimode_Network/g",y,".RData"))
    summary(g)
    
    # ISSUE: to compare nodes and keywords some properties are missing
    # when creating the filter, it blows off the papers beacuse they do not
    # posses the keyword property. 
    # SOLUTION: in order to avoid this, the maximum value of the keyword
    # property will be assigned to the paper nodes so it wont be affected
    # by the filtering range. 
    # to be able to filter and compare the keywords nodes and the 
    ## SETTING REFERENCE VALUE FOR THE PAPER NODES
    ## Enabling filtering of keyword node properties without affecting paper nodes
    ynb_c_ref<-max(as.numeric(yb_m[,"ny"]))# to all paper nodes
    yna_c_ref<-max(as.numeric(ya_m[,"ny"]))
    al_ref<-max(V(g)$al)# to all paper nodes
    bl_ref<-max(V(g)$bl)
    cl_ref<-max(V(g)$cl)
    V(g)$al[1:(((coor_a)+(coor_b))+coor_c)]<-al_ref
    V(g)$bl[1:(((coor_a)+(coor_b))+coor_c)]<-bl_ref
    V(g)$cl[1:(((coor_a)+(coor_b))+coor_c)]<-cl_ref
    V(g)$yna_c[1:(((coor_a)+(coor_b))+coor_c)]<-yna_c_ref
    V(g)$ynb_c[1:(((coor_a)+(coor_b))+coor_c)]<-ynb_c_ref
    # configurating csv to export to gephi
    #g<-graph.incidence(adv_s_o, directed = F,weighted="c")
    save(g,file=paste0("Multimode_Network/g",y,".RData"))#50998
    summary(g)
    np<-cbind(as.character(V(g)$name),#ID-1
              V(g)$name,# label-2
              V(g)$nt,# Type of node-3
              V(g)$py,# paper node's publication year-4
              V(g)$mv,# paper node's keyword network they belong to.-5
              V(g)$au,# paper node's authors-6
              V(g)$al,# keyword node connections to a (NHK)-7
              V(g)$bl,# keyword node connections to b (BBC)-8
              V(g)$cl,# keyword node connections to c (BD)-9
              V(g)$yla_c,#keyword node, label of py of the kw connecting a-c-10
              V(g)$yna_c,#keyword node, number of py of the kw connecting a-c-11
              V(g)$ylb_c,#keyword node, label of py of the kw connecting b-c-12
              V(g)$ynb_c#keyword node, number of py of the kw connecting b-c-13
    )
            
    colnames(np) <- c("Id",#-1
                      "Label",#-2
                      "N_type[z]",#-3
                      "py",#-4
                      "mv",#-5
                      "au",#-6
                      "al",#-7
                      "bl",#-8
                      "cl",#-9
                      "yla_c",#-10
                      "yna_c",#-11
                      "ylb_c",#-12
                      "ynb_c")#-13
    save(np,file=paste0("Multimode_Network/np",y,".RData"))
    write.csv(np,file=paste0("Multimode_Network/NOP_mm.csv"))
    
    
    edge <- get.edgelist(g, names=TRUE)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them 
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    write.csv(edgefull,file=paste0("Multimode_Network/EOP_mm.csv"))
    output<-g
    ##-------------------------------------------------------
    return(output)
}#ok

## ANALYSIS 
## Create filters or subnetworks to analyze the connections. 

### FILTER ONE NHK VS BBC.
# get al!=0 and bl=!0 kw [interception of both knowledge bases]
# and these kw's neighbors
nhk_vs_bd_link<-V(g)[(V(g)$nt=="kw")&(V(g)$al!=0)&(V(g)$cl!=0)]#752 - nhk to bd
bbc_vs_bd_link<-V(g)[((V(g)$nt=="kw")&(V(g)$bl!=0)&(V(g)$cl!=0))]#377 - bbc to bd
nhk_vs_bbc_link<-V(g)[((V(g)$nt=="kw")&(V(g)$bl!=0)&(V(g)$al!=0))]#99 - nhk to bbc
nhk_vs_bbc_vs_bd_links<-V(g)[((V(g)$nt=="kw")&(V(g)$bl!=0)&(V(g)$al!=0)&(V(g)$cl!=0))]#79 nhk,bbc and bd

# GETTING SUB NETWORKS or FILTERS
# get all the adjacent papers to the keywords
nhkbd_snp<-as.numeric(unlist(adjacent_vertices(g,nhk_vs_bd_link)))#get the adjacent vertices
# transform from node names to graph element index
nhkbd_snip<-unlist(lapply(nhkbd_sn,from_name2index,g))
# get only those adjacent vertices that matter for the filter
# this case a,c
nhkbd_fg<-V(g)[nhkbd_snip]
nhkbd_snpf<-nhkbd_fg[nhkbd_fg$nt=="a"|nhkbd_fg$nt=="c"]
nhkbd_snipf<-as.numeric(unlist(lapply(nhkbd_snpf,from_name2index,g)))
# get the keyword nodes indexes
nhkbd_snikw<-as.numeric(unlist(lapply(nhk_vs_bd_link,from_name2index,g)))
# put them together
nhkbd_sniall<-c(nhkbd_snipf,nhkbd_snikw)
# generate the subgrap with the required nodes.
nhkbd_g<-induced_subgraph(g,V(g)[nhkbd_sniall])#10441
###
### FOR BBC VS BD
bbcbd_snp<-as.numeric(unlist(adjacent_vertices(g,bbc_vs_bd_link)))
bbcbd_snip<-unlist(lapply(bbcbd_sn,from_name2index,g))

bbcbd_fg<-V(g)[bbcbd_snip]
bbcbd_snpf<-bbcbd_fg[bbcbd_fg$nt=="b"|bbcbd_fg$nt=="c"]
bbcbd_snipf<-as.numeric(unlist(lapply(bbcbd_snpf,from_name2index,g)))

bbcbd_snikw<-as.numeric(unlist(lapply(bbc_vs_bd_link,from_name2index,g)))
bbcbd_sniall<-c(bbcbd_snipf,bbcbd_snikw)
bbcbd_g<-induced_subgraph(g,V(g)[bbcbd_sniall])#9499
##
### FOR NHK VS BBC
nhkbbc_snp<-as.numeric(unlist(adjacent_vertices(g,nhk_vs_bbc_link)))
nhkbbc_snip<-unlist(lapply(nhkbbc_sn,from_name2index,g))

nhkbbc_fg<-V(g)[nhkbbc_snip]
nhkbbc_snpf<-nhkbbc_fg[nhkbbc_fg$nt=="a"|nhkbbc_fg$nt=="b"]
nhkbbc_snipf<-as.numeric(unlist(lapply(nhkbbc_snpf,from_name2index,g)))

nhkbbc_snikw<-as.numeric(unlist(lapply(nhk_vs_bbc_link,from_name2index,g)))
nhkbbc_sniall<-c(nhkbbc_snipf,nhkbbc_snikw)
nhkbbc_g<-induced_subgraph(g,V(g)[nhkbbc_sniall])#430

### FOR NHK VS BBC VS BD
nhkbbcbd_snp<-as.numeric(unlist(adjacent_vertices(g,nhk_vs_bbc_vs_bd_links)))
nhkbbcbd_snip<-unlist(lapply(nhkbbcbd_sn,from_name2index,g))

nhkbbcbd_fg<-V(g)[nhkbbcbd_snip]
nhkbbcbd_snpf<-nhkbbcbd_fg[nhkbbcbd_fg$nt=="a"|nhkbbcbd_fg$nt=="b"|nhkbbcbd_fg$nt=="c"]
nhkbbcbd_snipf<-as.numeric(unlist(lapply(nhkbbcbd_snpf,from_name2index,g)))

nhkbbcbd_snikw<-as.numeric(unlist(lapply(nhk_vs_bbc_vs_bd_links,from_name2index,g)))
nhkbbcbd_sniall<-c(nhkbbcbd_snipf,nhkbbcbd_snikw)
nhkbbcbd_g<-induced_subgraph(g,V(g)[nhkbbcbd_sniall])#3794

## ISSUE: we can see the whole thing, but cannot say about the most important links
## or explain which topics group together the two elements
## SOLUTION: go for the incidence matrix and build the keyword vs keyword
## one on one network and recalculate the normal thingies


en2gephy_mn<-function(g,mode){
    np<-cbind(as.character(V(g)$name),#ID-1
              V(g)$name,# label-2
              V(g)$nt,# Type of node-3
              V(g)$py,# paper node's publication year-4
              V(g)$mv,# paper node's keyword network they belong to.-5
              V(g)$au,# paper node's authors-6
              V(g)$al,# keyword node connections to a (NHK)-7
              V(g)$bl,# keyword node connections to b (BBC)-8
              V(g)$cl,# keyword node connections to c (BD)-9
              V(g)$yla_c,#keyword node, label of py of the kw connecting a-c-10
              V(g)$yna_c,#keyword node, number of py of the kw connecting a-c-11
              V(g)$ylb_c,#keyword node, label of py of the kw connecting b-c-12
              V(g)$ynb_c#keyword node, number of py of the kw connecting b-c-13
    )
    colnames(np) <- c("Id",#-1
                      "Label",#-2
                      "N_type[z]",#-3
                      "py",#-4
                      "mv",#-5
                      "au",#-6
                      "al",#-7
                      "bl",#-8
                      "cl",#-9
                      "yla_c",#-10
                      "yna_c",#-11
                      "ylb_c",#-12
                      "ynb_c")#-13
    save(np,file=paste0("Multimode_Network/np",mode,".RData"))
    write.csv(np,file=paste0("Multimode_Network/NOP_",mode,".csv"))
    
    
    edge <- get.edgelist(g, names=TRUE)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them 
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    write.csv(edgefull,file=paste0("Multimode_Network/EOP_",mode,".csv"))
    return(NULL)
}
zoom2gephy_un<-function(sg,m){
    # get incidence graph
    #sg<-nhkbbc_g
    #m<-"nhk_bbc"
    im<-as_incidence_matrix(sg,sparse = T)
    adjm<-t(im)%*%(im)
    g <- graph.adjacency(adjm, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an ed
    save(g,file=paste0("Multimode_Network/um_zoom/g_",m,".RData"))
    ## Communities
    clouv<-cluster_louvain(g)
    #g$comm<-clouv
    ## Centrality
    ceg<-centr_eigen(g, directed = FALSE, scale = TRUE, normalized = TRUE)
    
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    
    #STRUCTURAL PROPERTIES
    npb<-betweenness(g,directed = FALSE, normalized = TRUE)# gives in the same order
    
    npd<-degree(g,mode = c("all"))#,loops = TRUE, normalized = FALSE)
    
    g$nb<-as.numeric(npb)
    g$nd<-as.numeric(npd)
    g$mod<-npcmlv
    g$egvc<-as.numeric(ceg$vector)
    
    np<-cbind(as.character(unlist(attributes(npb)[1])),
              as.character(unlist(attributes(npb)[1])),
              as.numeric(npb),
              as.numeric(npd),
              #as.numeric(npcm_s),
              npcmlv,
              as.numeric(ceg$vector)
    )
    #npsTotal_s,
    #npcTotal_s)
    colnames(np) <- c("Id",
                      "Label",
                      "Betweenness CentralityI",
                      "DegreeI",
                      #"Modularity ClassIeb",
                      "Modularity ClassIL",
                      "Eigenvector Centrality")
    #"Subjects",
    #"Countries")
    save(np,file=paste0("Multimode_Network/um_zoom/np_",m,".RData"))
    write.csv(np,file=paste0("Multimode_Network/um_zoom/NOP_",m,".csv"))
    
    edge <- get.edgelist(g, names=TRUE)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them 
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    write.csv(edgefull,file=paste0("Multimode_Network/um_zoom/EOP_",m,".csv"))
    #write.csv(edgefull1_s,"Convergence/2008/EOP_s_2008.csv")
    output<-g
    return(output)
}
mode<-"nhk_bd"
en2gephy_mn(nhkbd_g,mode)
m<-mode
nhk_bd_zg<-zoom2gephy_un(nhkbd_g,m)

mode<-"bbc_bd"
en2gephy_mn(bbcbd_g,mode)
m<-mode
bbc_bd_zg<-zoom2gephy_un(bbcbd_g,m)

mode<-"nhk_bbc"
en2gephy_mn(nhkbbc_g,mode)
m<-mode
nhk_bbc_zg<-zoom2gephy_un(nhkbbc_g,m)

mode<-"nhk_bbc_bd"
en2gephy_mn(nhkbbcbd_g,mode)
m<-mode
nhk_bbc_bd_zg<-zoom2gephy_un(nhkbbcbd_g,m)

# get keyword co-linkage network between the two paper sets

# get the clusters and centrality
# observe it
# get all the papers linked to each group
nhkbd_g
nhk_bd_zg
bbcbd_g
bbc_bd_zg
nhkbbc_g
nhk_bbc_zg
nhkbbcbd_g
nhk_bbc_bd_zg



# for each community get the keywords
#function
zg_get_kwbymod_indetail<-function(mo,zg){
    #mo<-modu[1]
    output<-V(zg)[zg$mod==mo]
    return(output)
}
zg_get_kwbymod<-function(zg){
    # all keywords and their modularities.
    #zg<-nhk_bd_zg
    modu<-unique(zg$mod)
    #lapply function
    
    output<-lapply(modu,zg_get_kwbymod_indetail,zg)
    names(output)<-modu
    return(output)
}
nhkbd_kwnymod<-zg_get_kwbymod(nhk_bd_zg)
bbcbd_kwnymod<-zg_get_kwbymod(bbc_bd_zg)


# for each cluster of keyword in get their neighbors in the submultimode network [filtered]


#### OPTION II, Combining the nodes in the sub network 
## this will lead to avoid re-identification of nodes and to keep the properties
## also allow the calculation of the cluster's properties autoamtically
nhkbd_g
nhkbd_kwnymod
bbcbd_g
bbcbd_kwnymod

# do the mapping and test
# these keywords, convert them to the index of the subnetwork
# fore each mod, get the kw,
# for each kw in the mod get its index
sg_get_ibymod_indetail<-function(n,sg){
    #n<-names(ckw[1])
    sni<-which(names(V(sg))==n)#homogenizing graphs names
    output<-sni
    return(output)
}
sg_get_ibymod<-function(ckw,sg){
    #sg<-nhkbd_g
    #ckw<-nhkbd_kwnymod[1][[1]]
    ckw<-names(ckw)
    #lapply function
    # get all the neighbors of this keyword cluster
    pbymod<-unlist(lapply(ckw,sg_get_ibymod_indetail,sg))
    output<-pbymod
    return(output)
}
# to define the mapping of the vertex. 
# for each keyword community get the index in the sub network
# and that is the mapping to be developed.
sg<-nhkbd_g
nhkbd_ibymod_l<-lapply(nhkbd_kwnymod,sg_get_ibymod,sg)
sg<-bbcbd_g
bbcbd_ibymod_l<-lapply(bbcbd_kwnymod,sg_get_ibymod,sg)
# from all the keyword vertexs, get the first 85 or number of communities
# and make them the mapping number
# so get their index, and get the number of communities
# get the index corresponding to the number of communities and use them to map
# to map the documents their index are left as they are
do_kw_mapping<-function(mapi,ibymod){
    #mapi<-kwmapv[1]
    #ibymod<-ibymod_l[1][[1]]
    mval<-rep(mapi,length(ibymod))
    output<-cbind(map_value=mval,real_index=ibymod)
    return(output)
}
get_contract_mapping<-function(sg,ibymod_l){
    #ibymod_l<-nhkbd_ibymod_l
    #sg<-nhkbd_g#10441
    ivkw<-which(V(sg)$nt=="kw")#754
    ivp<-which(V(sg)$nt!="kw")#9689
    map<-vector("numeric",vcount(sg))
    map[1:length(ivp)]<-ivp
    kwmapv<-ivkw[1:length(ibymod_l)]
    #create the keyword mapping
    #mapply function
    # input kwmapv and ibymod_l
    # output two columns, real index and mapping value
    kwmapm<-do.call(rbind,mapply(do_kw_mapping,kwmapv,ibymod_l,SIMPLIFY=F))
    kwmapm_o<-kwmapm[order(kwmapm[,"real_index"]),]
    map[(length(ivp)+1):length(map)]<-kwmapm_o[,"map_value"]
    output<-map
    return(output)
}

nhk_map<-get_contract_mapping(nhkbd_g,nhkbd_ibymod_l)
bbc_map<-get_contract_mapping(bbcbd_g,bbcbd_ibymod_l)

sg<-nhkbd_g
nhkbdsgbykwcomm<-contract(sg,nhk_map,vertex.attr.comb=list("first"))
sg<-bbcbd_g
bbcbdsgbykwcomm<-contract(sg,bbc_map,vertex.attr.comb=list("first"))

kw2mod<-function(n2c,zg){
    #n2c<-n2c[1]
    output<-paste0(zg$mod[V(zg)$name==n2c],"m")
    
    return(output)
}
update_node_names2_comm<-function(g,zg){
    #take all the kw type nodes
    #zg<-nhk_bd_zg
    #zg_n<-names(V(zg))
    i2c<-which(V(g)$nt=="kw")
    n2c<-V(g)$name[i2c]
    #for each n2c get the modularity
    #lapply function
    
    nn<-unlist(lapply(n2c,kw2mod,zg))
    V(g)$name[i2c]<-nn
    output<-g
    return(g)
}
sgbycomm2gephy_mn<-function(g,m,zg){
    #update node names to the community modularity
    #g<-bbcbdsgbykwcomm
    #zg<-bbc_bd_zg
    g<-update_node_names2_comm(g,zg)
    np<-cbind(as.character(V(g)$name),#ID-1
              V(g)$name,# label-2
              V(g)$nt,# Type of node-3
              V(g)$py,# paper node's publication year-4
              V(g)$mv,# paper node's keyword network they belong to.-5
              V(g)$au,# paper node's authors-6
              V(g)$al,# keyword node connections to a (NHK)-7
              V(g)$bl,# keyword node connections to b (BBC)-8
              V(g)$cl,# keyword node connections to c (BD)-9
              V(g)$yla_c,#keyword node, label of py of the kw connecting a-c-10
              V(g)$yna_c,#keyword node, number of py of the kw connecting a-c-11
              V(g)$ylb_c,#keyword node, label of py of the kw connecting b-c-12
              V(g)$ynb_c#keyword node, number of py of the kw connecting b-c-13
    )
    colnames(np) <- c("Id",#-1
                      "Label",#-2
                      "N_type[z]",#-3
                      "py",#-4
                      "mv",#-5
                      "au",#-6
                      "al",#-7
                      "bl",#-8
                      "cl",#-9
                      "yla_c",#-10
                      "yna_c",#-11
                      "ylb_c",#-12
                      "ynb_c")#-13
    save(np,file=paste0("Multimode_Network/mm_contracted/np",m,".RData"))
    write.csv(np,file=paste0("Multimode_Network/mm_contracted/NOP_",m,".csv"))
    
    edge <- get.edgelist(g, names=TRUE)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them 
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    write.csv(edgefull,file=paste0("Multimode_Network/mm_contracted/EOP_",m,".csv"))
    return(g)
}

m<-"nhk_bd"
nhkbdsg_by_c<-sgbycomm2gephy_mn(nhkbdsgbykwcomm,m,nhk_bd_zg)
m<-"bbc_bd"
bbcbdsg_by_c<-sgbycomm2gephy_mn(bbcbdsgbykwcomm,m,bbc_bd_zg)


#### OPTION II finish
## MOVING ON TO getting the author and R&D profiles
## for each linking kw or kw community get the neighbors
get_comm_author_profile_indetail<-function(aul){
    #m<-"a"
    x<-aul
    auv<-trim(unlist(strsplit(x,"\\|")))
    aup<-sort(table(auv),decreasing = T)
    output<-aup
    return(output)
}
get_x_papers<-function(nl,m="a",sg_by_c){
    #m<-"a"
    x<-nl
    aps<-x[V(sg_by_c)$nt[x]==m]
    output<-V(sg_by_c)$au[aps]
    return(output)
}
get_comm_author_profile<-function(sg_by_c,m="a"){
    #sg_by_c<-sg_by_c
    #get the mods or kw communities
    mods<-V(sg_by_c)[(V(sg_by_c)$nt=="kw")]
    # get the list of neighbors
    nl<-adjacent_vertices(sg_by_c,mods)
    #lapply function
    
    #m<-"a"
    aul<-lapply(nl,get_x_papers,m,sg_by_c)
    
    aupl<-lapply(aul,get_comm_author_profile_indetail)
    output<-aupl
    return(output)
}
m<-"a"
nhkbd_aup<-get_comm_author_profile(nhkbdsg_by_c,m)
m<-"b"
bbcbd_aup<-get_comm_author_profile(bbcbdsg_by_c,m)
########################################################################## 
############################# ANALYSIS ###################################
########### CONNECTION OF LINKING KEYWORD CLUSTERS AND R&D PROFILES#######
##########################################################################

## From the zoom keywrod network get the cp, 
## Get from cp the clusters with more number of keywords
## Select those as representative linking groups.

## when you have defined linking groups do the author match
## for each cluster define the author profile DONE
## translate the author profile to R&D line
### to translate author to R&D line
### we need to:

#### match authors with their author coauthorship network'cluster. 
##### load the author np, to get their cluster info

# I need the r&D labels of each modularity.
load("NHK_fused/authors/nhk_rd_profiles.RData")
load("BBC_fused/authors/bbc_rd_profiles.RData")
# I need author's network np to get the cluster each belongs to
load("NHK_fused/authors/np_sall.RData")
nhk_aunp<-np_s
load("BBC_fused/authors/np_sall.RData")
bbc_aunp<-np_s

bbcbd_aup
# I need to simplify the author names to match the 
nhkbd_aup # to lower
nhkbd_aup_2l<-lapply(nhkbd_aup,
                     function(x){
                         names(x)<-tolower(names(x))
                     return(x)})
bbcbd_aup
bbcbd_aup_2l<-lapply(bbcbd_aup,
         function(x){
             names(x)<-tolower(names(x))
             return(x)})

#lapply function
get_aumod_indetail<-function(x,aunp){
    #x<-aup_2l[2]
    if(any(aunp[,"Author"]==x)){
        #ok get the mods
        mod<-aunp[aunp[,"Author"]==x,"Modularity ClassIL"]
        if(length(unique(mod)>1)){
            #two different authors, gotta check .. 
            output<-paste0(unique(mod),collapse = "|")
        }else{
            #ok
            output<-unique(mod)
        }
    }else{
        # the name will become a missing data "nf"
        output<-"nf"
    }
    
    return(output)
}
get_aumod<-function(aup_2l,aunp){
    #aup_2l<-nhkbd_aup_2l[2][[1]]
    aup_2lna<-names(aup_2l)
    #colnames(aunp)
    #aunp<-nhk_aunp
    #lapply function get the mod names
    mod_na<-unlist(lapply(aup_2lna,get_aumod_indetail,aunp))
    # update the modnames
    names(aup_2l)<-mod_na
    output<-aup_2l
    return(aup_2l)
}

aunp<-nhk_aunp
nhkbd_aum<-lapply(nhkbd_aup_2l,get_aumod,aunp)

aunp<-bbc_aunp
bbcbd_aum<-lapply(bbcbd_aup_2l,get_aumod,aunp)
#
#### Match the author modularities with their corresponding R&D profile
nhkbd_aum
nhk_rd_profiles#list, the element mod tell us to which author cluster it belongs to
# get the names of each list element,
#lapply function
get_RD_profiles_indetail<-function(modpn,rd_profiles){
    #modpe<-modp[1]
    #rd_profiles[1][[1]]
    #need a lapply function to get the data from the needed element 
    # of the rd profile list
    # if it is nf, leave
    # if it corresponds to a R&D profile, 
    if(modpn!="nf"){
        output<-unlist(lapply(rd_profiles,function(x,modpn){
            if(x[names(x)=="mod"]==modpn){
                return(x)
            }else{
                return(NULL)
            }
        },modpn))
    }else{
        buff<-1
        names(buff)<-modpn
        output<-buff
    }
    return(output)
}
sum_table<-function(x,aum){
    output<-sum(aum[names(aum)==x])
    names(output)<-x
    return(output)
}
get_RD_profiles<-function(aum,rd_profiles){
    #aum<-nhkbd_aum[4][[1]]
    #rd_profiles<-nhk_rd_profiles#list, the element mod tell us to which author cluster it belongs to
    # summirazing a composite table
    ## get the unique names
    un<-unique(names(aum))
    ## for each one, sum all the sub elements which contain that name
    modp<-unlist(lapply(un,sum_table,aum))

    #lapply function
    ## multiply the sum of the unique name for the values of the R&D profile
    rd_p_weights<-lapply(names(modp),get_RD_profiles_indetail,rd_profiles)
    ## for then decompose all the elements of each R&D profile with its multiplied components
    rd_p_stotal<-unlist(mapply(function(x,y){x*y},rd_p_weights,modp, SIMPLIFY = F))
    # unique names
    un<-unique(names(rd_p_stotal))
    ## and add everything together. 
    rd_p_total<-unlist(lapply(un,sum_table,rd_p_stotal))
    total<-sort(rd_p_total[names(rd_p_total)!="no_zero" & names(rd_p_total)!="cero" & names(rd_p_total)!="mod" ],decreasing = T)
    output<-list(total=total,modp=modp)
    return(output)
}

nhk_rd_link_kw_final_profiles<-lapply(nhkbd_aum,get_RD_profiles,nhk_rd_profiles)
nhk_rd_link_kw_final_profiles$`80m`

bbc_rd_link_kw_final_profiles<-lapply(bbcbd_aum,get_RD_profiles,bbc_rd_profiles)
bbc_rd_link_kw_final_profiles$`80m`
# get the uniques
# for each unique name sum the frequencies

########################################################################## 
############################# ANALYSIS ###################################
#################### WHICH CLUSTERS TO ANALYSE ###########################
##########################################################################


get_average_year_by_kwmod<-function(ibymod_l,sg,m){
    #sg<-nhkbd_g
    #ibymod_l<-nhkbd_ibymod_l[1][[1]]
    if(m=="a"){
        output<-mean(as.numeric(V(sg)$yna_c[ibymod_l]))
    }else{
        output<-mean(as.numeric(V(sg)$ynb_c[ibymod_l]))
    }
    
    return(output)
}
sg<-nhkbd_g
#nhkbd_ibymod_l
m<-"a"
nhk_comms_year_av<-lapply(nhkbd_ibymod_l,get_average_year_by_kwmod,sg,m)
nhk_comms_year_av_v<-unlist(nhk_comms_year_av)
nhk_selected_comms<-names(nhk_comms_year_av_v[nhk_comms_year_av_v>1])

sg<-bbcbd_g
m<-"b"
bbcbd_ibymod_l<-lapply(bbcbd_kwnymod,sg_get_ibymod,sg)
bbc_comms_year_av<-lapply(bbcbd_ibymod_l,get_average_year_by_kwmod,sg,m)
bbc_comms_year_av_v<-unlist(bbc_comms_year_av)
bbc_selected_comms<-names(bbc_comms_year_av_v[bbc_comms_year_av_v>1])

########################################################################## 
######################## ANALYSIS TOOLS###################################
######################## Selected communities ############################
####################### Linking keyword communities ######################
####################### Linking keyword communities author prof###########
####################### Linking keyword communities R&D prof #############
##########################################################################
nhk_selected_comms
write.csv(cbind(names(nhk_rd_link_kw_final_profiles$`65m`$total),nhk_rd_link_kw_final_profiles$`65m`$total),"65m.csv")

bbc_selected_comms
bbc_rd_link_kw_final_profiles$`27m`
write.csv(cbind(names(bbc_rd_link_kw_final_profiles$`22m`$total),bbc_rd_link_kw_final_profiles$`22m`$total),"22m.csv")

########################################################################## 
######################## ANALYSIS TOOLS###################################
############ Selected communities for paper level analysis ###############
############ Get papers from Broadcaster                   ###############
############ Get papers from big data                      ###############
############ Identify specific problems                    ###############
##########################################################################

#2 NHK communities
#2 BBC communities 

# Selecting the common communities and discommon communities. 
# gotta go for each comm keywords and check their interception
# see which ones share , which no
# among those who share and do not share, choose 2 comms for each broadcaster.

nhkbd_kwnymod
bbcbd_kwnymod
# sequencial lapply function
# start from bbc and for each bbc comm get their interception which 
# each nhk comm. 
get_kwcomm_interception_indetail<-function(kwnymod_b_l,kwnymod_a_l){
    #kwnymod_b<-kwnymod_b_l[1][[1]]
    int<-length(intersect(names(kwnymod_b_l),names(kwnymod_a_l)))
    output<-int
    return(output)
}
get_kwcomm_interception<-function(kwnymod_a_l,kwnymod_b_l){
    #kwnymod_a_l<-bbcbd_kwnymod[1][[1]]
    #kwnymod_b_l<-nhkbd_kwnymod
    #lapply
    comm_int<-unlist(lapply(kwnymod_b_l,get_kwcomm_interception_indetail,kwnymod_a_l))
    output<-comm_int
    return(output)
}
kwcomm_interception<-function(kwnymod_a_l,kwnymod_b_l){
    #nhkbd_kwnymod
    #bbcbd_kwnymod
    #kwnymod_a_l<-bbcbd_kwnymod
    #kwnymod_b_l<-nhkbd_kwnymod
    buff<-lapply(kwnymod_a_l,get_kwcomm_interception,kwnymod_b_l)
    output<-do.call(rbind,buff)
    return(output)
}
bbcnhk_comm_intersect<-kwcomm_interception(bbcbd_kwnymod,nhkbd_kwnymod)
bbcnhk_selected_comm_intersect<-bbcnhk_comm_intersect[bbc_selected_comms,nhk_selected_comms]

do_incidence_graph_nhkbbc<-function(inci_m){
    #inci_m<-bbcnhk_selected_comm_intersect
    colnames(inci_m)<-paste0(colnames(inci_m),"nhk")
    rownames(inci_m)<-paste0(rownames(inci_m),"bbc")
    #nrow(inci_m)
    #ncol(inci_m)
    
    g<-graph.incidence(inci_m, directed = F,weighted="c")
    nt<-vector("character",vcount(g))
    V(g)$nt<-nt
    V(g)$nt[1:nrow(inci_m)]<-"b"
    V(g)$nt[(nrow(inci_m)+1):vcount(g)]<-"a"
    
    save(g,file=paste0("Analysis/g",y,".RData"))#
    
    np<-cbind(as.character(V(g)$name),#ID-1
              V(g)$name,# label-2
              V(g)$nt# Type of node-3
    )
    
    colnames(np) <- c("Id",#-1
                      "Label",#-2
                      "N_type[z]")#-3
    
    save(np,file=paste0("Analysis/np",y,".RData"))
    write.csv(np,file=paste0("Analysis/NOP_kwl.csv"))
    
    
    edge <- get.edgelist(g, names=TRUE)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them 
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    write.csv(edgefull,file=paste0("Analysis/EOP_kwl.csv"))
    return(g)
}
do_incidence_graph_nhkbbc(bbcnhk_selected_comm_intersect)

# one common    - friendly [interfaces] - production[content search] - codecs
# one specific. - signal processing (NHK),  infra for data analysis (BBC)
#######################
### ANALYSIS RESULTS ##
#######################

#BBC common: 9bbc
#BBC single: 22bbc
#NHK common: 62NHK
#NHK single: 45NHK

###############################
### In detail paper analysis ##
###############################

#for the communities get the papers on the broadcaster's side 
# and identify problems
# GEt the papers from the broadcaster's side.
# Contracted network to communities of linking keywords.
nhkbdsg_by_c
bbcbdsg_by_c
V(bbcbdsg_by_c)[V(bbcbdsg_by_c)$nt=="kw"]
V(nhkbdsg_by_c)[V(nhkbdsg_by_c)$nt=="kw"]

m<-"a"#nhk
make_vertex_pairs<-function(side_a,side_b){
    #side_a<-thepapers[1]
    #side_b<-comm
    output<-c(side_a,side_b)
    return(output)
}
get_the_papersbycomm<-function(comm,sg_by_c,m){
    #comm<-comms[1]
    #E(comm)
    #E(sg_by_c)$c
    #V(sg_by_c)[comm]
    papers<-neighbors(sg_by_c,comm)
    thepapers<-papers[papers$nt==m]
    #thepapers
    #lapply function
   
    #pair_ind<-as.numeric(unlist(lapply(thepapers,make_vertex_pairs,comm)))
    #thepapers has the ending points of each comm.
    #eids<-get.edge.ids(sg_by_c, pair_ind)
    #E(sg_by_c)[E(sg_by_c)$c[eids]>1]
    # from each comms get its edges
    
    # get only those equal to thresold
    # from those edges get the paper they link to.
    
    output<-thepapers
    return(output)
}
get_the_papers2read<-function(sg_by_c,m){
    #sg_by_c<-bbcbdsg_by_c
    comms<-V(sg_by_c)[V(sg_by_c)$nt=="kw"]
    # for each comm get the papers
    #lapply function
    output<-lapply(comms,get_the_papersbycomm,sg_by_c,m)
    return(output)
}
m<-"b"#bbc
bbcbd_paper2read<-get_the_papers2read(bbcbdsg_by_c,m)

# Target communities for BBC 9 and 22
bbcbd_paper2read$`9m`
bbcbd_paper2read$`22m`

m<-"a"#bbc
nhkbd_paper2read<-get_the_papers2read(nhkbdsg_by_c,m)
# Target communities for BBC 62 and 45
nhkbd_paper2read$`62m`
nhkbd_paper2read$`45m`
# later read the big data side and find alternative solutions to those problems. 

m<-"c"#bbc
bdnhk_paper2read<-get_the_papers2read(nhkbdsg_by_c,m)
bdbbc_paper2read<-get_the_papers2read(bbcbdsg_by_c,m)
bdnhk_paper2read$`79m`


#####################################
### Get the paper's data  analysis ##
#####################################
# Get the node names and use them as index in the big mother matrix
colnames(nhk_accp_m)
a_m<-nhk_accp_m[,c("outputTitle","outputKw","outputAbs","comparKw","PY","mv")]
b_m<-bbc_accp_m[,c("outputTitle","outputKw","outputAbs","comparKw","PY","mv")]
c_m<-bd_accp_m[,c("outputTitle","outputKw","outputAbs","comparKw","PY","mv")]
mother_m<-rbind(a_m,b_m,c_m)

## BBC selected commmunity linked papers
p_ind<-unique(as.numeric(names(bbcbd_paper2read$`9m`)))
bbc9m_paperset<-mother_m[p_ind,]
write.csv(bbc9m_paperset,"Analysis/bbc/bbc9m_paperset.csv")

p_ind<-unique(as.numeric(names(bbcbd_paper2read$`22m`)))
bbc22m_paperset<-mother_m[p_ind,]
write.csv(bbc22m_paperset,"Analysis/bbc/bbc22m_paperset.csv")

p_ind<-unique(as.numeric(names(bbcbd_paper2read$`10m`)))
bbc10m_paperset<-mother_m[p_ind,]
write.csv(bbc10m_paperset,"Analysis/bbc/bbc10m_paperset.csv")

p_ind<-unique(as.numeric(names(bbcbd_paper2read$`11m`)))
bbc11m_paperset<-mother_m[p_ind,]
write.csv(bbc11m_paperset,"Analysis/bbc/bbc11m_paperset.csv")

p_ind<-unique(as.numeric(names(bbcbd_paper2read$`27m`)))
bbc27m_paperset<-mother_m[p_ind,]
write.csv(bbc27m_paperset,"Analysis/bbc/bbc27m_paperset.csv")

## NHK selected community linked papers
p_ind<-unique(as.numeric(names(nhkbd_paper2read$`62m`)))
nhk62m_paperset<-mother_m[p_ind,]
write.csv(nhk62m_paperset,"Analysis/nhk/nhk62m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`45m`)))
nhk45m_paperset<-mother_m[p_ind,]
write.csv(nhk45m_paperset,"Analysis/nhk/nhk45m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`59m`)))
nhk59m_paperset<-mother_m[p_ind,]
write.csv(nhk59m_paperset,"Analysis/nhk/nhk59m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`75m`)))
nhk75m_paperset<-mother_m[p_ind,]
write.csv(nhk75m_paperset,"Analysis/nhk/nhk75m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`20m`)))
nhk20m_paperset<-mother_m[p_ind,]
write.csv(nhk20m_paperset,"Analysis/nhk/nhk20m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`47m`)))
nhk47m_paperset<-mother_m[p_ind,]
write.csv(nhk47m_paperset,"Analysis/nhk/nhk47m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`57m`)))
nhk57m_paperset<-mother_m[p_ind,]
write.csv(nhk57m_paperset,"Analysis/nhk/nhk57m_paperset.csv")

p_ind<-unique(as.numeric(names(nhkbd_paper2read$`79m`)))
nhk79m_paperset<-mother_m[p_ind,]
write.csv(nhk79m_paperset,"Analysis/nhk/nhk79m_paperset.csv")


#######################
p_ind<-unique(as.numeric(names(bdnhk_paper2read$`79m`)))
bdnhk79m_paperset<-mother_m[p_ind,]
write.csv(bdnhk79m_paperset,"Analysis/bd/bdnhk79m_paperset.csv")

p_ind<-unique(as.numeric(names(bdnhk_paper2read$`57m`)))
bdnhk57m_paperset<-mother_m[p_ind,]
write.csv(bdnhk57m_paperset,"Analysis/bd/bdnhk57m_paperset.csv")

p_ind<-unique(as.numeric(names(bdbbc_paper2read$`9m`)))
bdbbc9m_paperset<-mother_m[p_ind,]
write.csv(bdbbc9m_paperset,"Analysis/bd/bdbbc9m_paperset.csv")

p_ind<-unique(as.numeric(names(bdbbc_paper2read$`11m`)))
bdbbc11m_paperset<-mother_m[p_ind,]
write.csv(bdbbc11m_paperset,"Analysis/bd/bdbbc11m_paperset.csv")

#to select the papers, a non ambigous keyword set of broadcasting data based applications 
# will be selected and from those, the big data papers 
# will be filtered and revised. 
# ISSUE: How to filter out the right papers to read from the connections between clusters
# the current issue is given because some clusters link to more than 1000 papers
# which makes is not possible to read,
# also some links are not related, like those generated by ambigous keywords like "children"
# there some keywords which are specific applications used by broadcasters, those are of interest
# some patterns we have detected is that these are not ambigous and normally 
# connect not so many big data papers.
# possible ways to in the future improve this matching are 
# 1)detect TFIDF of the documents belonging to the clusters in the broadcaster's
# and use those to match. 
# 2) do an ambiguosity test of the kewyords and separate those which are general
# 3) combination of 1) and 2).

# Getting the clusters keywords TFIDF
# Thigs to take into account for the TFIDF in this context
# the communities were found by kw, not by papers, meaning
# some papers will be in more than one comm
# it is expected that by the IDF charactersitic of the TFIDF
# these redundancies will not affect the final result.
# also it will be interesting to see the relationship 
# between centrality and TFIDF of the kw in the communities.
bbcbd_paper2read
nhkbd_paper2read
colnames(mother_m)
get_paper_kws<-function(ps,mm){
    #ps<-bbcbd_paper2read[1][[1]]
    #mm<-mother_m
    p_ind<-unique(as.numeric(names(ps)))
    pskw<-mother_m[p_ind,c("outputKw","comparKw")]
    pskw[,"comparKw"]<-as.character(pskw[,"comparKw"])
    output<-pskw
    #summary(output)
    return(output)
}
### get the paper kw for each paper in linked to the community
length(bbcbd_paper2read[1][[1]])
bbcbd_paperkw<-lapply(bbcbd_paper2read,get_paper_kws,mother_m)
nhkbd_paperkw<-lapply(nhkbd_paper2read,get_paper_kws,mother_m)

### put them in matrix form
bbcbd_paperkw_m<-do.call(rbind,bbcbd_paperkw)
nhkbd_paperkw_m<-do.call(rbind,nhkbd_paperkw)

### get from the compar kw the unique kws
bbcbd_ukws<-unique(trim(unlist(strsplit(bbcbd_paperkw_m[,"comparKw"],"|",fixed = TRUE))))
nhkbd_ukws<-unique(trim(unlist(strsplit(nhkbd_paperkw_m[,"comparKw"],"|",fixed = TRUE))))

## use these unique kws vector and the paper set by comm and get the 
## by comm doc vs kw.
docVskw_from_list<-function(paperkw_v,ukws){
    #paper level
    #paperkw_v<-paperkw_m[1,"comparKw"]
    pkw<-trim(unlist(strsplit(paperkw_v,"|",fixed = TRUE)))
    #lapply function
    #keyword level
    pkw_ind<-which(ukws%in%pkw)
    buff<-vector("numeric",length(ukws))
    names(buff)<-ukws
    buff[pkw_ind]<-1
    output<-buff
    return(buff)
}
## lapply for each comm
get_dovVskw_from_list<-function(paperkw_m,ukws){
    #ukws<-bbcbd_ukws
    # community level
    #paperkw_m<-bbcbd_paperkw[1][[1]]
    paperkw_comparkw<-paperkw_m[,"comparKw"]
    # lapply function
    # for each paper get the kw and find their position in the ukws vector
    comm_docVskw_list<-lapply(paperkw_comparkw,docVskw_from_list,ukws)
    # return that vector
    comm_docVskw_m<-do.call(rbind,comm_docVskw_list)
    # with that one, colsum it and you get the comm doc vs kw
    output<-colSums(comm_docVskw_m)
    return(output)
}

bbcbd_docVskw_by_comm<-lapply(bbcbd_paperkw,get_dovVskw_from_list,bbcbd_ukws)
nhkbd_docVskw_by_comm<-lapply(nhkbd_paperkw,get_dovVskw_from_list,nhkbd_ukws)

bbcbd_docVskw_by_comm_m<-do.call(rbind,bbcbd_docVskw_by_comm)
nhkbd_docVskw_by_comm_m<-do.call(rbind,nhkbd_docVskw_by_comm)

## these will be the rows of the final doc vs kw matrix used to calculate the 
## TFIDF
get_idf<-function(tfm){
    dtpm<-matrix(0,nrow(tfm),ncol(tfm))
    attributes(dtpm)<-attributes(tfm)
    dtpm[tfm[,]>0]<-1
    idf <- log2(nrow(tfm)/colSums(dtpm))
    output<-idf
    return(output)
}#based on log2, it gives you a better resolution on the small document collections. 
get_tfidf<-function(dtm){
    #dtm<-bbcbd_docVskw_by_comm_m
    tfm <- as.matrix(dtm)
    idf <- get_idf(tfm)
    tfidf <- tfm
    
    for(word in names(idf)){
        tfidf[,word] <- tfm[,word] * idf[word]
    }
    output<-tfidf
    return(output)
}

bbcbd_tfidf_by_comm_m<-get_tfidf(bbcbd_docVskw_by_comm_m)
nhkbd_tfidf_by_comm_m<-get_tfidf(nhkbd_docVskw_by_comm_m)

## now i need for each community to see their scores so 
## to see only words which are have values bigger than one.

bbcbd_tfidf_by_comm_l<-apply(bbcbd_tfidf_by_comm_m,1,function(x){sort(x[x>1],decreasing=T)})
nhkbd_tfidf_by_comm_l<-apply(nhkbd_tfidf_by_comm_m,1,function(x){sort(x[x>1],decreasing=T)})

## report tfidf
## mapply
report_tfidf<-function(tfidf_by_comm_l,comm_name,m){
    #tfidf_by_comm_l<-bbcbd_tfidf_by_comm_l[1][[1]]
    #comm_name<-names(bbcbd_tfidf_by_comm_l)[1]
    write.csv(tfidf_by_comm_l,file=paste0("Analysis/",m,"/tfidf/",comm_name,".csv"))
    return(NULL)
}

m<-"bbc"
mapply(report_tfidf,bbcbd_tfidf_by_comm_l,names(bbcbd_tfidf_by_comm_l),m)
m<-"nhk"
mapply(report_tfidf,nhkbd_tfidf_by_comm_l,names(nhkbd_tfidf_by_comm_l),m)



