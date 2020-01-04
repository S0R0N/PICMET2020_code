library(igraph)
library(lsa)
library('Matrix')
# Keywords dinamic exploration
# Import all nps and orgnize them in a full table with their clusters
# Create a data frame to save the np and cv data only. 

# Something is not right, check the raw data base of different years and follow one word 
# when detecting the dinamics, maybe the indexing is failing.
# the destiny detection has problems.
# SOLVED: ordered the vectors from each year so their communities corresponding in origin and destination tables

#This code analyzes centrality and degree of a keyword inside  the community they belong to in a given year

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rmSpace<-function(x){gsub("\\s", "", x)}
rmPunct<-function(x){gsub("[[:punct:]]","",x)}
prePmix<-function(x){rmPunct(rmSpace(trim(tolower(x))))}

fieldMerger<-function(x){
    output<-paste(x,collapse = " | ")
    return(output)
}

# you need the cluster tag identify real change. To combine with the community relative measures
load_np<-function(x){
    #load(file="2008 data/np2008.RData") np_s_2008.RData
    load(file=paste0("Convergence/",x,"/np_s_",x,".RData"))#getting the stored data
    #buff[paste0("2008")]<-list(node_name=np[,"Keyword"],communities=np[,"Modularity ClassIL"],year=2008)
    y<-np_s[,c("Keyword","Modularity ClassIL")]
    z<-y[order(y[,"Keyword"]),]
    #output<-list(node_name=z[,"Keyword"],communities=z[,"Modularity ClassIL"])#saving it        
    output<-z
    return(output)
}# load the np to create the dinamics

load_general_np<-function(x,p){# this general brigns the values of centrality for all the network. 
    load(file=paste0(p,"/np_s",x,".RData"))#getting the stored data
    y<-np_s
    z<-y[order(y[,"kwValue"]),]
    #output<-list(node_name=z[,"Keyword"],communities=z[,"Modularity ClassIL"])#saving it        
    output<-z
    return(output)
}# load the np to create the dinamics
load("contest_data_m.rdata")
py <- sort(unique(contest_kwdata[,"PY"]),decreasing = F)
#Implementation


## community relative values ##
# np_year<-lapply(y,load_np) #Load the data on the np variables for those years

##  General network values  ##mode
mode <- "contest_fused"
np_year_general<-lapply(py,load_general_np,mode)

##--------------------------##
## formating the general network 
get_allkw_general <- function(x){
    output <- x[,"kwValue"]
    return(output)
}
allKw_general <- unlist(lapply(np_year_general,get_allkw_general))#

## Getting unique keywords
allKw_general_u<-unique(allKw_general)#

## creating the matrix for the factor analysis
#mapply, give it the index of row and at the end of each iteration create a vector
# the output will be a two coloumn vector of i and j
# that output will be rbind for all years and you get the indexes. 

get_general_matrix_index <- function(x,np,allKw){
    j <- which(allKw%in%np[,"kwValue"])
    i <- vector("numeric",length(j))
    i[] <- x
    output <- cbind(i,j)
    return(output)
}
yn <- 1:length(py)
gmi<- mapply(get_general_matrix_index,yn,np_year_general,MoreArgs=(list(allKw_general_u)))
gmi_m <- do.call(rbind,gmi)

#get the values for each matrix
np_year_general_m<- do.call(rbind,np_year_general)
save(np_year_general_m,file="contest_np_year_general_m.RData")


f_input_general_d <- sparseMatrix(i=gmi_m[,"i"]
                                   ,j=gmi_m[,"j"]
                                   ,x=as.numeric(np_year_general_m[,"DegreeI"])
                                   ,dims=c(length(np_year_general),length(allKw_general_u))
                                   ,dimnames = list(py,allKw_general_u))
f_input_general_c <- sparseMatrix(i=gmi_m[,"i"]
                                  ,j=gmi_m[,"j"]
                                  ,x=as.numeric(np_year_general_m[,"Betweenness CentralityI"])
                                  ,dims=c(length(np_year_general),length(allKw_general_u))
                                  ,dimnames = list(py,allKw_general_u))
f_input_general_egc <- sparseMatrix(i=gmi_m[,"i"]
                                  ,j=gmi_m[,"j"]
                                  ,x=as.numeric(np_year_general_m[,"Eigenvector Centrality"])
                                  ,dims=c(length(np_year_general),length(allKw_general_u))
                                  ,dimnames = list(py,allKw_general_u))
f_input_general_lmod <- sparseMatrix(i=gmi_m[,"i"]
                                    ,j=gmi_m[,"j"]
                                    ,x=as.numeric(np_year_general_m[,"Modularity ClassIL"])
                                    ,dims=c(length(np_year_general),length(allKw_general_u))
                                    ,dimnames = list(py,allKw_general_u))
f_input_general_kwp <- sparseMatrix(i=gmi_m[,"i"]
                                     ,j=gmi_m[,"j"]
                                     ,x=1
                                     ,dims=c(length(np_year_general),length(allKw_general_u))
                                     ,dimnames = list(py,allKw_general_u))




anyNA(f_input_general_c) #the betweenes centrality problem for 2016 and 2017
anyNA(f_input_general_d)
anyNA(f_input_general_egc)
anyNA(f_input_general_kwp)


##########################################################################################
###---------------------------------------TIME Presence CHECK------------------------#####
##########################################################################################
kwp<-f_input_general_kwp
class(kwp)
nrow(kwp)
ncol(kwp)
kwp[,1:4]
# 09/14/2016
# operationalizing the presence in time verification
# KWTP is operationalized as SUM until n-1 of (ai*ai+1) n being the items in the vector
do_kwtp<-function(x){
    #x<-kwp[,1]
    buff<-vector("numeric",length(x)-1)
    for(i in 1:length(buff)){
        buff[i]<-x[i]*x[i+1]
    }
    output<-sum(buff)
    return(output)
}

# get the positions of the words that are over a set threshold(th) of kwtp right now 2.
get_kwtp<-function(kwp,th=2){
    #th<-2
    buff<-apply(kwp,2,do_kwtp)#select the columns that go with the threshold
    output<-which(buff>th)
    return(output)
}
get_kwtp_score<-function(kwp,th=2){
    #th<-2
    buff<-apply(kwp,2,do_kwtp)#select the columns that go with the threshold
    output<-buff
    return(output)
}

# Get the indexes of the keywords that commply with the precenece thingy. 
# Column indexes with that indicate which keywords should we analyze for the centrality thingy
#rm(get_kwtp_output)
kwtp_output <- get_kwtp(kwp,1)
kwtp_output2 <- get_kwtp(kwp,2)
length(kwtp_output)# words passed the kwpt test
length(kwtp_output2)

# to be able to track this step.
kwtp_output_score<-get_kwtp_score(kwp)
write.csv(kwtp_output_score,"contest_kwtp_output_score.csv")
length(kwtp_output_score)
# to be able to track this step.

# connecting time verification results with centrality verification
# ISSUE: not all the words have the eigenvalue .. have to intercept these groups.
# SOLUTION: apply the centrality issue solution after this filter 

# first filter the words that complied with kwtp and then fix the centrality issue. 
f_input_kwtp_c<-f_input_general_c[,kwtp_output]
f_input_kwtp_d<-f_input_general_d[,kwtp_output]
f_input_kwtp_egc<-f_input_general_egc[,kwtp_output]

f_input_kwtp_c2<-f_input_general_c[,kwtp_output2]
f_input_kwtp_d2<-f_input_general_d[,kwtp_output2]
f_input_kwtp_egc2<-f_input_general_egc[,kwtp_output2]

######################################################################
#####--------------------CENTRALITY CHECK------------------------#####
######################################################################
# Getting a vector of average centrality for each year to do the centrality check
egc_means<-sapply(np_year_general,function(x){mean(as.numeric(x[,"Eigenvector Centrality"]))})

# 10/04/2016
# 04-30-2019 $ added an additional variable to check what happens if the threshold of KWTP is lowered
# Formating the matrix to list to do the centrality check.
f_input_kwtp_egc
rowN<-rownames(f_input_kwtp_egc)
colN<-colnames(f_input_kwtp_egc)
l_input_kwp_egc<-lapply(1:nrow(f_input_kwtp_egc),function(x,f_input_kwtp_egc){f_input_kwtp_egc[x,]},f_input_kwtp_egc)
attributes(l_input_kwp_egc)$names<-rowN

f_input_kwtp_egc2
rowN2<-rownames(f_input_kwtp_egc2)
colN2<-colnames(f_input_kwtp_egc2)
l_input_kwp_egc2<-lapply(1:nrow(f_input_kwtp_egc2),function(x,f_input_kwtp_egc2){f_input_kwtp_egc2[x,]},f_input_kwtp_egc2)
attributes(l_input_kwp_egc2)$names<-rowN2


# implement the whole list comparison with a mapply. 
# I can get the index of those who are bigger. 
l_centrality_check_indexes<-mapply(function(l_input_kwp_egc,egc_means){which(l_input_kwp_egc>egc_means)},l_input_kwp_egc,egc_means)
length(l_centrality_check_indexes) # 13

l_centrality_check_indexes2<-mapply(function(l_input_kwp_egc2,egc_means){which(l_input_kwp_egc2>egc_means)},l_input_kwp_egc2,egc_means)
length(l_centrality_check_indexes2) # 13
# create a clone of the matrix of centralities
# with attributes and everything
#rowN<-attributes(f_input_kwtp_egc )$dimnames[[1]]
#colN<-attributes(f_input_kwtp_egc )$dimnames[[2]]
get_centrality_check_index <- function(x,n){
    y <- vector("numeric",length(x))
    y[] <- n
    output <- cbind(i=y,j=x)
    return(output)
}
#yn
centrality_check_index_l<- mapply(get_centrality_check_index,l_centrality_check_indexes,yn)
centrality_check_index_l2<- mapply(get_centrality_check_index,l_centrality_check_indexes2,yn)
centrality_check_index_m <- do.call(rbind,centrality_check_index_l)
centrality_check_index_m2 <- do.call(rbind,centrality_check_index_l2)

m_centrality_check <- sparseMatrix(i=centrality_check_index_m[,"i"]
                                   ,j=centrality_check_index_m[,"j"]
                                   ,x=1
                                   ,dims=c(length(rowN),length(colN))
                                   ,dimnames=list(rowN,colN))

m_centrality_check2 <- sparseMatrix(i=centrality_check_index_m2[,"i"]
                                   ,j=centrality_check_index_m2[,"j"]
                                   ,x=1
                                   ,dims=c(length(rowN2),length(colN2))
                                   ,dimnames=list(rowN2,colN2))

##-------------------- END CENTRALITY CHECK---------------------------------------

##-------------------- ANALYSIS --------------------------------------------------
### words that have centrality surge last year and those who do not. 

x<-which(colSums(m_centrality_check)>1)# for all years, which had more than one surge
y<-which(m_centrality_check[length(rowN),]!=0)#all keywords which had surge in the last year

x2<-which(colSums(m_centrality_check2)>1)# for all years, which had more than one surge
y2<-which(m_centrality_check2[length(rowN2),]!=0)#all keywords which had surge in the last year

m_both<-m_centrality_check[,intersect(x,y)] #2621-1485 Common words between both conditions
m_both2<-m_centrality_check2[,intersect(x2,y2)] #2621-1485 Common words between both conditions
m_before<-m_centrality_check[,setdiff(x,y)] # Words that that more than 2 but did not have in the last year
m_before2<-m_centrality_check2[,setdiff(x2,y2)] # Words that that more than 2 but did not have in the last year
m_last_year<-m_centrality_check[,setdiff(y,x)] #178 Words that had the increase last year
m_last_year2<-m_centrality_check2[,setdiff(y2,x2)] #178 Words that had the increase last year

m_union<-m_centrality_check[,union(x, y)] #9428 words that had more than 2 increases or those with increase in the last year.
m_union2<-m_centrality_check2[,union(x2, y2)] #9428 words that had more than 2 increases or those with increase in the last year. 

np_year_general_m
#all_kw_kwValues<-cbind(kwValue=all_KwValues,Keyword=all_keywords)
all_kw_kwValues <- np_year_general_m[,c("kwValue","Keyword","Modularity ClassIL")]
all_kw_kwValues[,"Keyword"]

# helper function to get the keywords names. 
get_kword_names<-function(all_kw_kwValues,v_target){
    kw_index<-which(all_kw_kwValues[,"kwValue"]%in%v_target)
    m_kwords_names<-all_kw_kwValues[kw_index,]
    m_kwords_names[order(m_kwords_names[,"kwValue"]),]
    v_kwords_uniquenames<-unique(m_kwords_names[,"Keyword"])
    output<-v_kwords_uniquenames[order(v_kwords_uniquenames)]
    return(output)
}

contest_union_ckw<-colnames(m_union)# all the converging keywords
contest_union_ckw2<-colnames(m_union2)# all the converging keywords
length(colnames(m_union))#9428
length(colnames(m_union2))#9428

# v_target<-colnames(m_both)
# length(colnames(m_both))#334
# 
contest_before_ckw<-colnames(m_before)
contest_before_ckw2<-colnames(m_before2)
# length(colnames(m_before))#278
contest_last_year_ckw<-colnames(m_before)
contest_last_year_ckw2<-colnames(m_last_year2)
# length(colnames(m_last_year))#30

#v_order_kwords_uniquenames<-get_kword_names(all_kw_kwValues,v_target)
v_order_kwords_uniquenames<-get_kword_names(all_kw_kwValues,contest_union_ckw)

#################################################################
####                    SAVING TO Rdata                   #######
#################################################################
#ai_all_ckeywords<-get_kword_names(all_kw_kwValues,v_target)
contest_all_ckeywords<-get_kword_names(all_kw_kwValues,sh_union_ckw)
save(contest_all_ckeywords,file = "contest_all_ckeywords.rdata")

contest_all_ckeywords2<-get_kword_names(all_kw_kwValues,contest_union_ckw2)
save(contest_all_ckeywords2,file = "contest_all_ckeywords2.rdata")

#################################################################
####                    SAVING TO CSV                     #######
#################################################################

contest_union_ckeywords<-get_kword_names(all_kw_kwValues,contest_union_ckw)
write.csv(contest_union_ckeywords,"contest_union_names_conv_kw_set.csv")
write.csv(as.matrix(m_union),"contest_union_conv_kw_set.csv")

contest_union_ckeywords2<-get_kword_names(all_kw_kwValues,contest_union_ckw2)
write.csv(contest_union_ckeywords2,"contest_union_names_conv_kw_set2.csv")
write.csv(as.matrix(m_union2),"contest_union_conv_kw_set2.csv")

contest_before_ckeywords<-get_kword_names(all_kw_kwValues,contest_before_ckw)
write.csv(contest_before_ckeywords,"contest_before_conv_kw_set.csv")
write.csv(as.matrix(m_before),"contest_before_conv_kw_set.csv")

contest_before_ckeywords2<-get_kword_names(all_kw_kwValues,contest_before_ckw2)
write.csv(contest_before_ckeywords2,"contest_before_conv_kw_set2.csv")
write.csv(as.matrix(m_before2),"contest_before_conv_kw_set2.csv")

contest_last_year_ckeywords<-get_kword_names(all_kw_kwValues,contest_last_year_ckw)
write.csv(contest_last_year_ckeywords,"contest_last_year_conv_kw_set.csv")
write.csv(as.matrix(m_last_year),"contest_last_year_conv_kw_set.csv")

contest_last_year_ckeywords2<-get_kword_names(all_kw_kwValues,contest_last_year_ckw2)
write.csv(contest_last_year_ckeywords2,"contest_last_year_conv_kw_set2.csv")
write.csv(as.matrix(m_last_year2),"contest_last_year_conv_kw_set2.csv")

# 10/05/2016

# Take the union all of them and then create the size of knowledge classification.
# Wikipedia classification in progress

# 10/06/2016
# so there are different types of concepts that words describe
# there are research fields, services, disciplines, sciences and so on.. 
# yet some of them are related to the development of the big data technical characteristics
# others are receptors of the benefits generated by big data
# so to identify them we proposed the data value chain concept
# those concepts that are outside and are disciplines or object of study should be 
# receptors of big data services or guest to this field so to say. 

# summary: words not in the data value chain are our potential application convergence targets

# TO DO:
# Assign to the selected words the label of the cluster they belong to



# I updated cp to get words according to eigen vector centrality
# calculated all the cps. 2008-2015

# 10/07/2016
# TO DO: load cp, get for all the keywords, the label of the communities they 
# belonged to by year. 
# # like a matrix of column keyword vs year row and content label. 
# f_input_general_lmod
# colnames(f_input_general_clabel)
# # there i will be able to see the meaning of the word to disambiguate. 
# 
# # search for the colnames of the target keywords, get them and see how they fared on the years
# # compare the target vector agains tht column names
# # get the index to the columns 
# # extract the columns of the matrix that are of interest to us. 
# v_target<-colnames(m_union)
# v_target2<-colnames(m_union2)
# # v_target<-colnames(m_before)
# # v_target<-colnames(m_last_year)
# 
# get_kword_clabel<-function(m_Clabel,v_target){
#     col_index<-which(colnames(m_Clabel)%in%v_target)
#     output<-m_Clabel[,col_index]
#     return(output)
# }
# m_clabel_taget_keywords<-get_kword_clabel(f_input_general_clabel,v_target)
# m_clabel_taget_keywords2<-get_kword_clabel(f_input_general_clabel,v_target2)
# 
# write.csv(m_clabel_taget_keywords,"Convergence/output/output_clabel_union_kw_set.csv")
# write.csv(m_clabel_taget_keywords,"Convergence/output/output_clabel_before_kw_set.csv")
# write.csv(m_clabel_taget_keywords,"Convergence/output/output_clabel_last_year_kw_set.csv")
# # in time to check their meaning and use it on the disambiguation process. 
# 
