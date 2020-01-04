library(igraph)
library(lsa)
library('Matrix')
#Word preprocessing functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rmSpace<-function(x){gsub("\\s", "", x)}
rmPunct<-function(x){gsub("[[:punct:]]","",x)}
prePmix<-function(x){rmPunct(rmSpace(trim(tolower(x))))}
all_duplicated<-function(vec){
    duplicated(vec) | duplicated(vec, fromLast=TRUE)
}
# -----------------Classifying convergent words----------------------------------

# 1) WORD KNOWLEDGE SIZE CLASSIFICATION
# FIRST CHECK for all
# to this dbpedia properties were needed
# it was possbile to characterize the words into disciplines, industry or service/product. 

# first face each word against wikipedia, find out the different states that wikipedia offers
# so far found: ambigous, ok, redirect, unkwnown, does not exists.
# get the properties of the wikipedia object, so far categories is the most general one.
# CATEGORIES

# for successful cases get the url name, so you can try it on dbpedia
# get the properties of the dbpedia object.
# basic are
# SERVICE, INDUSTRY, DISCIPLINE
#trying wikipedia with curl..
source("wiki_logic_04-30-2019.R")


# have to create an external list in a file or an internal vector 
# to update the state of each word, as the execution goes. 
# everyone starting in do not know and then changing states. 
#input
#w:  original word
#ww: wikipedia corrected word
#gw: google to wikipedia suggested word
load("ai_all_ckeywords.rdata")
load("ai_fb_ckeywords.rdata")

#ISSUE: the keyword field got lost when creating the networks
#SOLUTION: recover them using the acck

# load al accsk
get_okw_ref <- function(y,p){
    load(paste0(p,"/acck",y,".RData"))
    output <- accsk[,"outputKw"]
    return(output)
}
mode <- "ai_fused"
y_fb <- 1990:2013
y_all <- 1990:2017
okw_ref_l_fb<- mapply(get_okw_ref,y_fb,MoreArgs=list(mode),SIMPLIFY = F)
okw_ref_l_all<- mapply(get_okw_ref,y_all,MoreArgs=list(mode),SIMPLIFY = F)
# build the index from compar kw to original
## get the originals, break them to a vector

okw_ref_v_fb <- tolower(trim(unlist(strsplit(unlist(okw_ref_l_fb),"|",fixed = T))))
okw_ref_v_all <- tolower(trim(unlist(strsplit(unlist(okw_ref_l_all),"|",fixed = T))))
## get all unique
okw_ref_v_fb_u <- unique(okw_ref_v_fb)
okw_ref_v_all_u <- unique(okw_ref_v_all)
## apply the premix
ref_v_fb <- prePmix(okw_ref_v_fb_u)
ref_v_all <- prePmix(okw_ref_v_all_u)
## build the index
ref_index <- cbind(ow=okw_ref_v_fb_u,kwv=ref_v_fb)
ref_index_all <- cbind(ow=okw_ref_v_all_u,kwv=ref_v_all)

## find.
get_ow <- function(rkw,ri){
    #ri  <- ref_index
    #rkw <- ai_all_ckeywords[1:3]
    output  <- unique(ri[which(ri[,"kwv"]%in%rkw),"ow"])
    return(output)
}
ai_fb_ckeywords_ow<- unlist(lapply(ai_fb_ckeywords,get_ow,ref_index))#7365
ai_all_ckeywords_ow<- unlist(lapply(ai_all_ckeywords,get_ow,ref_index_all))#13084
save(ai_fb_ckeywords_ow,file="ai_fb_ckeywords_ow.RData")
save(ai_all_ckeywords_ow,file="ai_all_ckeywords_ow.RData")
# use the target keywords to get all the ow from the accsk
# use the original keywords as input.

###########################################
##       Identifying Wiki entities       ##
###########################################
load("ws_all.rdata")
load("ai_fb_ckeywords_ow.RData")
kckw_fb<-gsub("\"","",ai_fb_ckeywords_ow)
kckw_all<-gsub("\"","",ai_all_ckeywords_ow)

ws_fb<-lapply(kckw_fb,get_wiki_entities)
ws_all<-lapply(kckw_all,get_wiki_entities)

ws_all_m <- apply(do.call(rbind,ws_all),2,unlist)

# recovering the wikipedia links from the complete set.
# x vector of words to find
# y reference data set.
get_ws_from_ws_all <- function(x,y){
    output <- y[which(y[,"ow"]%in%x),]
    return(output)
}

ws_fb<- lapply(kckw_fb,get_ws_from_ws_all,ws_all_m)
ws_fb_m <- do.call(rbind,ws_fb)

save(ws_all,file="ws_all.RData")
save(ws_all_m,file="ws_all_m.RData")
save(ws_fb,file="ws_fb.RData")
save(ws_fb_m,file="ws_fb_m.RData")
write.csv(ws_all_m,file = "ws_all_m.csv")
load("ws_all.RData")

ws_all_m#13084
ws_fb_m #7365
# am: Ambigous -> manual check
am_all<-ws_all_m[ws_all_m[,"ws"]=="am",]
am_fb<-ws_fb_m[ws_fb_m[,"ws"]=="am",]
nrow(am_all)#4500
nrow(am_fb) #2598
# ne: Not exists -> next step
ne_all<-ws_all_m[ws_all_m[,"ws"]=="ne",]
nrow(ne_all)#257
ne_fb<-ws_fb_m[ws_fb_m[,"ws"]=="ne",]
nrow(ne_fb)#107
# ok: ok-> Proceed to get the data 
ok<-m_status[m_status[,"ws"]=="ok",]
nrow(ok)#405/306

save(m_status,file="status.rdata")
save(am,file="am.rdata")
save(ne,file="ne.rdata")
save(ok,file="ok.rdata")

########################################
# little manual confirmation         ###
########################################
# confirming manual check with papers related to the term
load("np_year_general_m.Rdata")
y_all <- 1990:2017
p <- "ai_fused"
get_accsk_ref <- function(y,p){
    load(paste0(p,"/acck",y,".RData"))
    output <- accsk[,c("outputKw","outputTitle")]
    return(output)
}
mode <- "ai_fused"
y_fb <- 1990:2013
y_all <- 1990:2017
accsk_l_all<- mapply(get_accsk_ref,y_all,MoreArgs=list(mode),SIMPLIFY = F)
accsk_l_all[1]
accsk_l_all_m <- do.call(rbind,accsk_l_all)
colnames(accsk_l_all_m)
save(accsk_l_all_m,file="accsk_l_all_m.Rdata")
w<-"abs"
target<-grep(paste0("\\b",w,"\\b"),accsk_l_all_m[,"outputKw"],ignore.case = T)
accsk_l_all_m[target,"outputKw"]

update_status_am<-function(m,status){
    
    #m<-t1_am_ok_m
    #status<-m_status
    # update m status
    m[,"ws"]<-"ok"
    # compare the word link in
    # in this case is ow
    x<-status[,"ow"]
    y<-m[,"ow"]
    dim(m)
    dim(status[which(x%in%y),])
    status[which(x%in%y),]<-m
    output<-status
    return(output)
}
update_status_ne<-function(m,status){
    #m<-t1_am_ok_m
    # update m status
    m[,"ws"]<-"ok"
    # compare the word link in
    # in this case is ow
    x<-status[,"ow"]
    y<-m[,"nw"]
    status[which(x%in%y),]<-m
    output<-status
    return(output)
}
###################################################
##             Get those not error               ##
###################################################
ws_all_m#13084
ws_fb_m #7365

ws_all_m_none <- ws_all_m[ws_all_m[,"ws"]!="ne",]
ws_fb_m_none  <- ws_fb_m[ws_fb_m[,"ws"]!="ne",]



save(ws_all_m_none,file="ws_all_m_none.Rdata")
save(ws_fb_m_none,file="ws_fb_m_none.Rdata")
load("ws_fb_m_none.Rdata")
nrow(ws_fb_m_none)


##************************************************##
##        END google and wikipedia tests          ##
##               Part of google TEST              ##
##                                                ##
##************************************************##
# get the properties from wikipedia and dbpedia


load("ws_all_m_none.Rdata")

#ask the web site from dbpedia
# get the date from the url which is the disambiguated source
dbpedia_test_properties<-function(ur){
    w<-gsub("https://en.wikipedia.org/wiki/","",ur)
    
    uri_base<-"http://dbpedia.org/page/"
    uri_go<-paste0(uri_base,w)
    # The page exists?
    x<-tryCatch({
        wiki_test <- read_html(uri_go)
    }, warning = function(war){
        print(paste("MY_WARNING:  ",war))
        return(war)
    }, error = function(err) {
        print(paste("MY_ERROR:  ",err))
        return(err)
    }, finally = {
        print(paste("url called:  ",uri_go))
        closeAllConnections()
    })
    if(class(x)[2]=="error"){
        # insert in the field wikipedia test failed
        print("test failed, url do not exists")
        output<-"error"
    }else{# get the properties
        # check if the object has properties
        test<-wiki_test %>% 
            html_nodes(".property") %>%
            html_text()
        if(!is.na(test[1])){
            # it has properties
            # Get the properties
            p<-test
            output<-p
        }else{
            p<-"no properties"
            output<-p
        }
    }
    return(output)
}
# this will output for each word all its properties.
ws_all_m_none[,"fl"]
#ok_dbp_properties<-lapply(ws_all_m_none[1:2,"fl"],dbpedia_test_properties)

ok_dbp_properties<-lapply(ws_all_m_none[,"fl"],dbpedia_test_properties)

save(ok_dbp_properties,file="ok_dbp_properties.rdata")
load("ok_dbp_properties.rdata")

get_error_p<-function(x){
    if(x=="error"){
        output<-"e"
    }else{
        output<-"ne"
    }
    return(output)
}
get_error_index<-function(x,i){
    if(x=="error"){
        output<-i
    }else{
        output<-""
    }
    return(output)
}

e<-lapply(ok_dbp_properties,get_error_p)
warnings()
e_l<-do.call(rbind,e)#23
length(e_l[e_l=="e"])#442
e_i<-mapply(get_error_index,ok_dbp_properties,1:length(e))
e_i_v<-as.numeric(e_i[e_i!=""])

ok_t3[e_i_v,]
nrow(ws_all_m_none[e_i_v,])

# subset the dbpedia properties list into those which had
# and those which had errors. 

###################
ok_dbp_properties# dbpedia properties for those words which had..
# some of them did not have
# 445/8327 not found
# 7885/8327    found
##################
ws_all_m_none[,"ww"]
wikipedia_test_categories<-function(ur){
    #ur<-ok_t3[1,"fl"]
    w<-gsub("https://en.wikipedia.org/wiki/","",ur)
    uri_base<-"https://en.wikipedia.org/wiki/"
    uri_go<-paste0(uri_base,w)
    # The page exists?
    x<-tryCatch({
        wiki_test <- read_html(uri_go)
    }, warning = function(war){
        print(paste("MY_WARNING:  ",war))
        return(war)
    }, error = function(err) {
        print(paste("MY_ERROR:  ",err))
        return(err)
    }, finally = {
        print(paste("url called:  ",uri_go))
        closeAllConnections()
    })
    if(class(x)[2]=="error"){
        # insert in the field wikipedia test failed
        print("test failed, url do not exists")
        output<-"error"
    }else{# get the properties
        # check if the object has properties
        test<-wiki_test %>% 
            html_node("#catlinks")%>%
            html_node("ul")%>%
            html_nodes("li")%>%
            html_text()
        if(!is.na(test)[1]){
            # it has properties
            # Get the properties
            p<-test
            output<-p
        }else{
            p<-"no properties"
            output<-p
        }
    }
    return(output)
}


#ok_wik_properties<-lapply(ws_all_m_none[,"ww"],get_categories)

ok_wik_properties<-lapply(ws_all_m_none[,"fl"],wikipedia_test_categories)
save(ok_wik_properties,file="ok_wik_properties.rdata")
load("ok_wik_properties.rdata")

###################
# Preparing ok_t3 #
###################
ok_t3[,"ws"]<-"ok"
i<-which(ok_t3[,"nw"]!="")
ok_t3[i,"ow"]<-ok_t3[i,"nw"]
ok_t3[i,"ww"]
save(ok_t3,file="ok_t3.rdata")
###############
nrow(ok_t3)#672/649
ok_dbp_properties#12827 ok
ok_wik_properties#12827 ok
#################

# detect from the dbpedia properties which ones are 
# discipline, service or industry.
# tag them
# for each element of the list, check for the properties you want. 
# check 1) Discipline, 2) Industry, 3) Service
# Get only de disciplines
get_dp_p<-function(x){
    #x<-ok_dbp_properties[1][[1]]
    if(x=="error"){
        output<-"e"
    }else{
        # check for the properties and then fuse them together
        label<-""
        
        service_patters<-paste("product",
                               "service",
                               "products",
                               "services", sep = "|")
        
        industry_patters<-paste("industry",
                                "industry", sep = "|")
        
        discipline_patters<-paste("discipline",
                                  "academicDiscipline",
                                  "field",
                                  "field",
                                  "fields", sep = "|")
        sp_check<-grep(service_patters,x,value = T)
        if(length(sp_check)>0){
            sp_check<-"service/product"
        }
        ind_check<-grep(industry_patters,x,value = T)
        if(length(ind_check)>0){
            ind_check<-"industry"
        }
        dis_check<-grep(discipline_patters,x,value = T)
        if(length(dis_check)>0){
            dis_check<-"discipline"
        }
        
        label<-paste(sp_check,ind_check,dis_check,sep = "|")
        if(length(label)==0){
            label<-"e"
        }
        output<-label
    }
    return(output)
}
class_dbp_properties<-lapply(ok_dbp_properties,get_dp_p)
# use get index modified to get the index of those words which
# have discipline classificaiton.
# to let them ready to be insert to the main table
# these are the keywords which have the discipline properties

get_dp_index<-function(x,i){
    #x<-ok_dbp_properties[1][[1]]
    if(x=="e"){
        output<-""
    }else{
        # get the index of the words with discipline class
        if(grepl("discipline",x)){
            output<-i
        }else{
            output<-""
        }
    }
    return(output)
}
i<-1:length(class_dbp_properties)
dbp_discipline_index<-mapply(get_dp_index,class_dbp_properties,i,SIMPLIFY = T)
dbp_dsc_i<-as.numeric(dbp_discipline_index[dbp_discipline_index!=""])

#################################
# FIRST DISCIPLINE CHECK OUTPUT #
#################################

ok_t3[dbp_dsc_i,"ow"]
length(ok_t3[dbp_dsc_i,"ow"])#215

ws_all_m_none[dbp_dsc_i,"ow"]
length(ws_all_m_none[dbp_dsc_i,"ow"])#2154
# pending to link with the original bd
#################################
# FIRST DISCIPLINE CHECK OUTPUT #
#################################

# SECOND CHECK for the discplines
#################################
# SECOND DISCIPLINE CHECK       #
#################################
# we used a list of academic displines organized by wikipedia
# use ndivuwho list can help to identify the disciplines, invovled in big data 
# https://en.m.wikipedia.org/wiki/Outline_of_academic_disciplines
# peding to automatically do
# [work for today]
v_target<-colnames(m_union)
v_target<-ai_all_ckeywords
#load the disciplines word list.
file<-"list_disciplines.csv"
dt_disciplines<-read.csv(file, header = T,stringsAsFactors =F)
# compare which of the long list are in the smaller list. 
# is v_target on dt_disciplines
# pre process the disciplines vector
dt_disciplines[,1]<-prePmix(dt_disciplines[,1])
dt_disciplines[,1]# 1257
v_unique_disciplines<-unique(dt_disciplines[,1])#1164

V_discipline_check<-v_target[v_target%in%v_unique_disciplines] # result of interest
# ok DONE
#V_discipline_check has the values of the keywords which had disciplines
#with respect to the premixed words
# have to translate them to the 716 assing them the value 
# then complement with the dbpedia obtained ones. 
######################################
# SECOND DISCIPLINE CHECK CONVERTING #
######################################
length(V_discipline_check)#147
V_discipline_check_ow<-get_kword_names(all_kw_kwValues,V_discipline_check)

x<-ok_t3[dbp_dsc_i,"ow"]
y<-V_discipline_check_ow
z<-intersect(x,y)#32

# this one
x<-prePmix(ws_all_m_none[dbp_dsc_i,"ow"])
y<-V_discipline_check
z<-intersect(x,y)#32


######################################
# SECOND DISCIPLINE CHECK OUTPUT #
######################################
V_discipline_check_ow
length(V_discipline_check_ow)#38
V_discipline_check
length(V_discipline_check)#147
######################################
# SECOND DISCIPLINE CHECK OUTPUT #
######################################

###########################################
# AGERGGATING THE DATA from wiki and list #
###########################################

##********************##
## Preparing m_status ##
##********************##
i<-which(m_status_t3[,"nw"]!="")
m_status_t3[i,"ow"]<-m_status_t3[i,"nw"]


# create the two columns to add, wikipedia and list
wk<-vector("character",nrow(ws_all_m_none))
lt<-vector("character",nrow(ws_all_m_none))
rm(ws_all_m_none_f)
ws_all_m_none_f<-cbind(ws_all_m_none,wk,lt)

#lt_index<-which(m_status_f[,"ow"]%in%V_discipline_check_ow)
#m_status_f[lt_index,"lt"]<-"1"

lt_index<-which(prePmix(ws_all_m_none_f[,"ow"])%in%V_discipline_check)
ws_all_m_none_f[lt_index,"lt"]<-"1"

#wk_index<-which(m_status_f[,"ow"]%in%ok_t3[dbp_dsc_i,"ow"])
#m_status_f[wk_index,"wk"]<-"1"

wk_index<-which(ws_all_m_none_f[,"ow"]%in%ws_all_m_none_f[dbp_dsc_i,"ow"])
ws_all_m_none_f[wk_index,"wk"]<-"1"

## -------------------------------------------------------
## ISSUE of repeated ow
## to get a proper converging kewyords index need to fix it here too
any(duplicated(ws_all_m_none_f[,"ow"]))


ws_all_m_none_f_u <- ws_all_m_none_f[!duplicated(ws_all_m_none_f[,"ow"]),]
ws_fb_m_none_u <- ws_fb_m_none[!duplicated(ws_fb_m_none[,"ow"]),]


nrow(ws_fb_m_none)  #7234
nrow(ws_fb_m_none_u)#7172
ws_all_m_none_f
nrow(ws_all_m_none_f)  #12827
nrow(ws_all_m_none_f_u)#12801

## ---------------------------------------------------------
# given the issue, you have re obtain the lt and wk indexes
ws_all_m_none_f_u
lt_index_issue <- which(ws_all_m_none_f_u[,"lt"]=="1")
wk_index_issue <- which(ws_all_m_none_f_u[,"wk"]=="1")

#x<-lt_index
#y<-wk_index
x<-lt_index_issue
y<-wk_index_issue
z<-union(x,y)
d<-setdiff(x,y)#these disciplines were not covered by wikipedia properties

length(x)#195 by list
length(y)#2146 by dbpedia categories
length(d)#42 disciplines covered by list not by dbpedia cats

#m_status_f[d,]
#ws_all_m_none_f[z,]
ws_all_m_none_f_u[z,]

index_2_all_converging_disciplines<-z
length(z)

##**************************************##
## FINAL SET OF DISICIPLINES AGGREGATED ##
##**************************************##
ws_all_m_none_f_u[index_2_all_converging_disciplines,"ww"]
save(ws_all_m_none_f,file = "ws_all_m_none_f.RData")
save(ws_all_m_none_f_u,file = "ws_all_m_none_f_u.RData")
save(z,file = "index_2_all_converging_disciplines.rdata")
write.csv(ws_all_m_none_f,"ws_all_m_none_f.csv")
write.csv(ws_all_m_none_f_u,"ws_all_m_none_f_u.csv")
##**************************************##
## FINAL SET OF DISICIPLINES AGGREGATED ##
##**************************************##

load("m_status_f.rdata")
load("index_2_all_converging_disciplines.rdata")
index_2_all_converging_disciplines<-z

#m_status_f[index_2_all_converging_disciplines,]
## all these disciplines , i can add them their behaviour in time
## consulting each vector in time to see which were present 
## for which year and making a column out of that. 
#lapply function
#v<-m_status_f[,"ow"]
#l<-np_year_general
#get_years_from_kw<-function(l,v){
    #l[1][[1]][,"Keyword"]
#     output<-which(v%in%l[,"Keyword"])
#     return(output)
# }
#l_index<-lapply(l,get_years_from_kw,v)
#input, list of index and status matrix

#updating_status_years<-function(l,status)
    
    #NOW GET THE INFO FROM THE OTHER FIELDS AND START TO GIVE INTERESTING RESULTS
    
    # SO WHAT KIND OF THINGS WE CAN DO WITH THE DISCPLINES?
    # this disciplines againts the WOS disciplines...
    ## for this one I have to bring to this table the np property of 
    ## subject
    
    # which wikipedia words summarize these discplines
    ## for this just group repeated fields, table on ww or subset using ww
    ## just show a few interesting ones.

# SEE HOW THEY GROUP TOGETHER.. this is useful for 
# analysis and to eliminate redundancy of words before or after 
# word analysis
# FILTERING ARCHITECTURE, THE ONLY DISCIPLINE WHICH COULD NOT BE 
# DISAMBIGUATED

# Connection between disciplines by wikipedia categories
## 
#find the words and how they connect with each other throug ww properties
# final analyzis over converging disciplines
###### ######################################################
### setting the wikipedia properties to the final matrix ####
###############################################################
#w<-ok_t3[,"ow"]
#which(duplicated(ok_t3[,"ow"]))
#status<-m_status_f
#i<-1:nrow(ok_t3)
#p<-ok_wik_properties
ws_all_m_none_f
ws_all_m_none_f_u

any(duplicated(ws_all_m_none_f[,"ow"]))
any(duplicated(ws_all_m_none_f_u[,"ow"]))
#w<-ws_all_m_none_f[,"ow"]
w<-ws_all_m_none_f_u[,"ow"]
#which(duplicated(ok_t3[,"ow"]))
#status<-ws_all_m_none_f
status<-ws_all_m_none_f_u
i<-1:nrow(ws_all_m_none_f_u)
p<-ok_wik_properties

# in the context of having two matrices, one with the
# elements with wiki properties
# other with all the elements regardless
# then these functions match the wiki properties to those elements in the 
# all matrix.

get_index_wiki_prop_2_status<-function(i,w,p,status){
    #status<-m_status_f
    p_i<-ok_wik_properties[i][[1]]
    ix<-which(status[,"ow"]==w[i])[1]
    wp<-paste0(p_i,collapse = " | ")
    output<-cbind(ix,wp)
    return(output)
}
wiki_prop_index<-do.call(rbind,lapply(i,get_index_wiki_prop_2_status,w,p,status))

ix<-as.numeric(wiki_prop_index[,"ix"])
wp<-wiki_prop_index[,"wp"]
#status<-ws_all_m_none_f
status<-ws_all_m_none_f_u

format_wiki_prop_2_status<-function(ix,wp,status){
    wkp<-vector("character",nrow(status))
    status<-cbind(status,wkp)
    status[ix,"wkp"]<-wp
    output<-status
    return(output)
}
m_status_f_1<-format_wiki_prop_2_status(ix,wp,status)
colnames(m_status_f_1)
head(m_status_f_1)
######################################################
### these are the unique wikipedia categories. 
######################################################
## GENERATE THE NETWORK ow - ww network
######################################################
## mstatus_f_1 has the wikipedia properties of those words which had 
## properties, those which did not had or reported error are in 
## other matrix, ws_all_m
##-------------------------------------------------------------
## finding out what is going on here
## you get all convering disiciplines or keywords related to knowledge.
## ISSUE: there is an issue, I forgot to update the status 
## as we have a lot of ambigous which will be passed as okay
## you have to force this change in status
##----------------------------------------------
##- FOrcing change in status
##----------------------------------------------
#Get the m_status_f_1 for the first boom!
##m_status_f_1 has all the info for the converging into AI keywords
## is possible to obtain the set for the first boom by subdiviging the set. 
load("ws_all_m_none_f_u.RData")

which(ws_fb_m_none_u[,"ow"]%in%m_status_f_1[,"ow"])
m_status_fb<- m_status_f_1[which(m_status_f_1[,"ow"]%in%ws_fb_m_none_u[,"ow"]),]

m_status_fb[,"ws"] <- "ok"
m_status_f_1[,"ws"] <- "ok"

ws_fb_m_none
#----------------------------------------------------------
## Recalculating the index of converging keywords for the first boom!
##------------------------------------------------------------------
lt_index_issue_fb <- which(m_status_fb[,"lt"]=="1")
wk_index_issue_fb <- which(m_status_fb[,"wk"]=="1")

#x<-lt_index
#y<-wk_index
x<-lt_index_issue_fb
y<-wk_index_issue_fb
z<-union(x,y)
d<-setdiff(x,y)#these disciplines were not covered by wikipedia properties

length(x)#195 by list
length(y)#2146 by dbpedia categories
length(d)#42 disciplines covered by list not by dbpedia cats

#m_status_f[d,]
#ws_all_m_none_f[z,]
#ws_all_m_none_f_u[z,]

index_2_all_converging_disciplines_fb<-z
length(z)
save(index_2_all_converging_disciplines_fb,file = "index_2_all_converging_disciplines_fb.RData")

#----------------------------------------------------------
## Recalculating the index of converging keywords for the first boom!
##------------------------------------------------------------------
dp<-m_status_f_1[index_2_all_converging_disciplines,]
nrow(dp)

dp_fb<-m_status_fb[index_2_all_converging_disciplines_fb,]
nrow(dp_fb)

## you get all the converging disciplines which have status oka
dp_ok<-dp[dp[,"ws"]=="ok",]
nrow(dp_ok)

dp_ok_fb<-dp_fb[dp_fb[,"ws"]=="ok",]
nrow(dp_ok_fb)
## you create a table of the oka disciplines
u_bd_disciplines<-table(dp_ok[,"ww"])
u_bd_disciplines_fb<-table(dp_ok_fb[,"ww"])

# you get all th unique disicplines.
u_bd_d_all<-names(u_bd_disciplines)
u_bd_d_all_fb<-names(u_bd_disciplines_fb)
# for each of these Wikipedia words get the real words

#x<-u_bd_d_all
#m<-m_status_f
#y<-unique(m[,"ow"])
#ws_all_m
#write.csv(m[order(m[,"ow"]),],"test_m.csv")
#write.csv(ws_all_m[order(ws_all_m[,"ow"]),],"ws_test_m.csv")
#---------------------------------------------------
# issue number two 
# somehow i have duplicated ows in the m_status+_f1 which should not happen. 
# check them , ideed i have the problem since the first matrix.
# the ow comes repeated and should have been cleaded before
# SOLUTION: forcing clean

#----------------------------------------------------
# issue number three
# so i was comparing only the disciplines against all the matrix
# is we compare disciplines against all the matrix m_status_f_1_u
# then we will have orphan ow, because not all the ows are disciplines.
# i would suggest to subset the full matrix m_status_f_1_u to 
# a matrix which have the unique ows correspongin to the disciplines.
# like that you can create the adjacency matrix.
# SOLUTION: subset m_status_f_1_u to a m_status_only_disciplines
# fixed since the detection of the knowledge related keywords

#m_status_f_1_u <- m_status_f_1[!duplicated(m_status_f_1[,"ow"]),]

m_status_only_discplines <- dp_ok
m_status_only_discplines_fb <- dp_ok_fb

x<-u_bd_d_all       #849
m<-m_status_only_discplines
nrow(m)             #2188
y<-unique(m[,"ow"])
#m[duplicated(m[,"ow"]),"ow"]
length(y)           #2188

x<-u_bd_d_all_fb       #849
m<-m_status_only_discplines_fb
nrow(m)             #2188
y<-unique(m[,"ow"])
#m[duplicated(m[,"ow"]),"ow"]
length(y)           #2188

ww_grouping<-function(x,m){
    # for each word, get all ow that are linked by them
    output<-m[m[,"ww"]==x,]
    return(output)
}
ww_edge_list_generator<-function(x,m){
    # for each word, get all ow that are linked by them
    #x<-u_bd_d_all[1]
    buff<-m[m[,"ww"]==x,]
    if(is.matrix(buff)){
        len<-nrow(buff)
        s<-vector("character",len)
        t<-vector("character",len)
        ty<-vector("character",len)
        s[]<-x
        t[]<-buff[,"ow"]
        ty[]<-"undirected"
    }else{
        len<-1
        s<-vector("character",len)
        t<-vector("character",len)
        ty<-vector("character",len)
        s[]<-x
        t[]<-buff["ow"]
        ty[]<-"undirected"
    }
    e_l<-cbind(s,t,ty)# joining them 
    colnames(e_l)<-c("Source","Target","Type")# preparing the file
    return(e_l)
}

## you get the index of the disciplines from the matrix you pass
## in this case is the same m_status_f_1 so you are getting their same indexes
## or in other words you get all the oriiginal words that match the disciplines.
ww_adjm_j_index_generator<-function(x,m,y){
    # for each wikipedia property, gets the position of the matching keyword
    # compare those ow with the unique ow bgi vector 
    # get the idnex of the ow que corresponden a las ww
    ## solo las ow unicas
    buff<-m[,"ww"]==x
    target<-m[buff,"ow"]
    # get their positions and output
    output<-which(y%in%target)
    return(output)
}
## this dude, generates the corresponding i dimension of a sparce matrix
ww_adjm_i_index_generator<-function(i,x){
    # for each item, get the size
    j_ix<-x
    len<-length(j_ix)
    # create an i vector
    i_ix<-vector("numeric",len)
    i_ix[]<-i
    output<-cbind(i_ix,j_ix)
    return(output)
}
get_ww_ow_sparse<-function(i,j,rn,cn){
    buff<-sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(rn,cn))
    output<-buff
    return(output)
}
##***********************************##
##      First approach edge list     ##
##***********************************##

#l_edg_ww_grouping<-lapply(x,ww_edge_list_generator,m)
#m_edg_ww_grouping<-do.call(rbind,l_edg_ww_grouping)


##***********************************##
## Second approach adjancency matrix ##
##***********************************##
#sparse matrix of wikipedia words vs ow
# this gets you the j indexes
s_ww_adjm_j_index<-lapply(x,ww_adjm_j_index_generator,m,y)
#-------------------------------------
# this generates the correspoding i index
x<-s_ww_adjm_j_index
i<-1:length(x)
# mapply function
l_adj_index<-mapply(ww_adjm_i_index_generator,i,x,SIMPLIFY = F)

m_adjm_index_ww_ow<-do.call(rbind,l_adj_index)
rn<-u_bd_d_all_fb# row names
length(rn)#849
cn<-unique(m[,"ow"])# colnames
length(cn)#2188
i<-m_adjm_index_ww_ow[,"i_ix"]
j<-m_adjm_index_ww_ow[,"j_ix"]
#any(m_adjm_index_ww_ow[,"j_ix"]>2188)

#sparseMatrix(i,j,x=1,use.last.ij = T)
#sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(rn,cn))
adjm_ww_ow<-get_ww_ow_sparse(i,j,rn,cn)# 162X715 [architecture,weka]
adjm_ww_ow_fb <- adjm_ww_ow

##---------------------------------------------------
## Success after correcting the ISSUES 1,2 and 3
## the matrix was succesfully created.
## learnings, ISSUE one, if you are going to jump over the rules, remember to update the status
## ISSUE two: elminate repeated
## ISSUE three: the network was being generated using the general matrix
## this led to orphan nodes, so subsetting to those nodes which will really matter is better.
######################################################
## GENERATE THE NETWORKS ww - wkp network
######################################################
# Which kvalues words summarize these discplines
## same as before
# the only discipline without properties is architecture which 
# was an unsolved ambiguity is not taken into account for analysis
# filtering by the ws==ok

uwkp<-unique(dp_ok[,"wkp"])#1278 unique set of categories
uwkp<-unique(dp_ok_fb[,"wkp"])#1278 unique set of categories
# first expand and then compress
uwkp_e<-unique(trim(unlist(strsplit(uwkp,"|",fixed = TRUE)))) #2152
# rows - wikipedia words
uww<-u_bd_d_all_fb#849
uww<-u_bd_d_all#849

w<-uww
x<-uww
m<-dp_ok
m<-dp_ok_fb
y<-uwkp_e
wkp_edge_list_generator<-function(w,m){
    #for each unique ww, get the properties
    p<-m[m[,"ww"]==w,"wkp"]
    pp<-trim(unlist(strsplit(p[1],"|",fixed = TRUE)))
    len<-length(pp)
    s<-vector("character",len)
    t<-vector("character",len)
    ty<-vector("character",len)
    s[]<-w
    t[]<-pp
    ty[]<-"undirected"
    e_l<-cbind(s,t,ty)# joining them 
    colnames(e_l)<-c("Source","Target","Type")# preparing the file
    return(e_l)
    #and you are done
}
#lapply function
wkp_adjm_j_index_generator<-function(x,m,y){
    # for each word, compare to which ow it matches
    # compare those ow with the unique ow bgi vector 
    #x<-x[2]
    p<-m[m[,"ww"]==x,"wkp"]
    target<-trim(unlist(strsplit(p[1],"|",fixed = TRUE)))
    # get their positions and output
    output<-which(y%in%target)
    return(output)
}
#mapply function
wkp_adjm_i_index_generator<-function(i,x){
    # for each item, get the size
    j_ix<-x
    len<-length(j_ix)
    # create an i vector
    i_ix<-vector("numeric",len)
    i_ix[]<-i
    output<-cbind(i_ix,j_ix)
    return(output)
}
#plain function
get_ww_wkp_sparse<-function(i,j,rn,cn){
    buff<-sparseMatrix(i,j,x=1,use.last.ij = T,dimnames=list(rn,cn))
    output<-buff
    return(output)
}

# Generate the edgelist of these relationships
#el_wkp<-lapply(w,wkp_edge_list_generator,m)
#el_wkp_m<-do.call(rbind,el_wkp)

wkp_j_index<-lapply(x,wkp_adjm_j_index_generator,m,y)
x<-wkp_j_index
i<-1:length(x)
wkp_i_j_index<-mapply(wkp_adjm_i_index_generator,i,x)

m_adjm_index_wkp_ww<-do.call(rbind,wkp_i_j_index)

rn<-uww# row names
cn<-uwkp_e# colnames
i<-m_adjm_index_wkp_ww[,"i_ix"]
j<-m_adjm_index_wkp_ww[,"j_ix"]
adjm_wkp_ww<-get_ww_wkp_sparse(i,j,rn,cn)# 162X715 [architecture,weka]
adjm_wkp_ww_fb<-get_ww_wkp_sparse(i,j,rn,cn)# 162X715 [architecture,weka]


##**************************************##
##       FINAL ADJM for ow VS ww        ##
##**************************************##
adjm_ww_ow
save(adjm_ww_ow,file="adjm_ww_ow.rdata")
save(adjm_wkp_ww_fb,file="adjm_ww_ow_fb.rdata")
load("adjm_ww_ow.rdata")
##**************************************##
##       FINAL ADJM for wpk VS ww       ##
##**************************************##
adjm_wkp_ww
save(adjm_wkp_ww,file="adjm_wkp_ww.rdata")
save(adjm_wkp_ww_fb,file="adjm_wkp_ww_fb.rdata")
load("adjm_wkp_ww.rdata")

##**************************************##
##  wpk and ww ow network optimization  ##
##**************************************##
length(which(colSums(adjm_ww_ow)>1))# cannot be optmized as the 
# whole objective of this network is to show one type connections
##**************************************##
##  wpk  network optimization           ##
##**************************************##
adjm_wkp_ww_o<-adjm_wkp_ww[,colSums(adjm_wkp_ww)>1]
adjm_wkp_ww_o_fb<-adjm_wkp_ww_fb[,colSums(adjm_wkp_ww_fb)>1]
#adjm_wkp_ww_o<-adjm_wkp_ww[,colSums(adjm_wkp_ww)>0]# gives the error of tribology

rownames(adjm_wkp_ww_o)<-paste0(rownames(adjm_wkp_ww_o),"_w")
rownames(adjm_ww_ow)<-paste0(rownames(adjm_ww_ow),"_w")

rownames(adjm_wkp_ww_o_fb)<-paste0(rownames(adjm_wkp_ww_o_fb),"_w")
rownames(adjm_ww_ow_fb)<-paste0(rownames(adjm_ww_ow_fb),"_w")

colnames(adjm_wkp_ww_o)<-paste0(colnames(adjm_wkp_ww_o),"_wpk")
colnames(adjm_ww_ow)<-paste0(colnames(adjm_ww_ow),"_ow")

colnames(adjm_wkp_ww_o_fb)<-paste0(colnames(adjm_wkp_ww_o_fb),"_wpk")
colnames(adjm_ww_ow_fb)<-paste0(colnames(adjm_ww_ow_fb),"_ow")

##**************************************##
##  wpk  network optimization outputs   ##
##**************************************##
adjm_wkp_ww_o# optimized - 849 x 441
adjm_ww_ow# no need to optimize 162 x 715
#1-104 wkp nodes
ncol(adjm_wkp_ww_o)#104
#105-819 ww_ow nodes

##**************************************##
##  Create networks and export them     ##
##**************************************##

# Operations to do to the networks
# Calculate eigen vector centrality
# Format and export
adjm_2_network_wpk<-function(adjm){
    # Create graph
    #adjm<-adjm_wkp_ww_o
    g<-graph.incidence(adjm, directed = F,weighted="c")
    
    # mark the type of nodes
    nt_ww<-vector("character",nrow(adjm))
    nt_ww[]<-"ww"
    
    nt_wkp<-vector("character",ncol(adjm))
    nt_wkp[]<-"wpk"
    
    V(g)$nt[1:length(names(V(g)))]<-""
    V(g)$nt[1:nrow(adjm)]<-nt_ww
    V(g)$nt[(nrow(adjm)+1):length(names(V(g)))]<-nt_wkp
    
    # create do calculations
    V(g)$ceg<-centr_eigen(g,
                          directed = F,
                          scale = T,
                          normalized = T)$vector
    
    clouv<-cluster_louvain(g)
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    # create the node list
    npexp<-cbind(as.character(names(V(g))),
                 as.character(names(V(g))),
                 V(g)$ceg,
                 npcmlv,
                 V(g)$nt
    )
    
    colnames(npexp) <- c("Id",
                         "Label",
                         "Eigenvector Centrality",
                         "MOD_Louv",
                         "N_type[z]")
    # create the edge list
    edge <- get.edgelist(g, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    #output,
    #the graph,
    #the formated edge list
    #and the formated node list
    output<-list(g=g,nl=npexp,el=edgefull)
    return(output)
}
adjm_2_network_ow<-function(adjm){
    # Create graph
    #adjm<-adjm_ww_ow
    g<-graph.incidence(adjm, directed = F,weighted="c")
    
    # mark the type of nodes
    nt_ww<-vector("character",nrow(adjm))
    nt_ww[]<-"ww"
    
    nt_wkp<-vector("character",ncol(adjm))
    nt_wkp[]<-"ow"
    
    V(g)$nt[1:length(names(V(g)))]<-""
    V(g)$nt[1:nrow(adjm)]<-nt_ww
    V(g)$nt[(nrow(adjm)+1):length(names(V(g)))]<-nt_wkp
    
    # create do calculations
    V(g)$ceg<-centr_eigen(g,
                          directed = F,
                          scale = T,
                          normalized = T)$vector
    clouv<-cluster_louvain(g)
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    # create the node list
    npexp<-cbind(as.character(names(V(g))),
                 as.character(names(V(g))),
                 V(g)$ceg,
                 npcmlv,
                 V(g)$nt)
    
    colnames(npexp) <- c("Id",
                         "Label",
                         "Eigenvector Centrality",
                         "MOD_Louv",
                         "N_type[z]")
    # create the edge list
    edge <- get.edgelist(g, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    #output,
    #the graph,
    #the formated edge list
    #and the formated node list
    output<-list(g=g,nl=npexp,el=edgefull)
    return(output)
}
adjm_2_multi_network<-function(adjm_1,adjm_2){
    # Create graph
    #adjm_1<-adjm_wkp_ww_o
    #adjm_2<-adjm_ww_ow
    adjm<-cbind(adjm_1,adjm_2)#162 x 819
    g<-graph.incidence(adjm, directed = F,weighted="c")
    
    # mark the type of nodes
    nt_ww<-vector("character",nrow(adjm))
    nt_ww[]<-"ww"
    
    nt_wkp<-vector("character",ncol(adjm_1))
    nt_wkp[]<-"wpk"
    
    nt_ow<-vector("character",ncol(adjm_2))
    nt_ow[]<-"ow"

    #V(g)[1:nrow(adjm)]
    
    V(g)$nt[1:length(V(g))]<-""
    V(g)$nt[1:nrow(adjm)]<-nt_ww
    V(g)$nt[(nrow(adjm)+1):(nrow(adjm)+ncol(adjm_1))]<-nt_wkp
    V(g)$nt[(nrow(adjm)+ncol(adjm_1)+1):length(V(g))]<-nt_ow
    
    # create do calculations
    V(g)$ceg<-centr_eigen(g,
                          directed = F,
                          scale = T,
                          normalized = T)$vector
    clouv<-cluster_louvain(g)
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    # create the node list
    npexp<-cbind(as.character(names(V(g))),
                 as.character(names(V(g))),
                 V(g)$ceg,
                 npcmlv,
                 V(g)$nt)
    
    colnames(npexp) <- c("Id",
                         "Label",
                         "Eigenvector Centrality",
                         "MOD_Louv",
                         "N_type[z]")
    # create the edge list
    edge <- get.edgelist(g, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    #output,
    #the graph,
    #the formated edge list
    #and the formated node list
    output<-list(g=g,nl=npexp,el=edgefull)
    return(output)
}
adjm_ww_vs_ww_by_wpk<-function(adjm){
    # multiply for the transpose
    adjm_s<-(adjm)%*%t(adjm)
    # generate the graph
    g<- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)
    # do the thingies to take to gephi
    # create do calculations
    V(g)$nt[1:length(V(g))]<-"ww"
    V(g)$ceg<-centr_eigen(g,
                          directed = F,
                          scale = T,
                          normalized = T)$vector
    clouv<-cluster_louvain(g)
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    # create the node list
    npexp<-cbind(as.character(names(V(g))),
                 as.character(names(V(g))),
                 V(g)$ceg,
                 npcmlv,
                 V(g)$nt)
    
    colnames(npexp) <- c("Id",
                         "Label",
                         "Eigenvector Centrality",
                         "MOD_Louv",
                         "N_type[z]")
    # create the edge list
    edge <- get.edgelist(g, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    #output,
    #the graph,
    #the formated edge list
    #and the formated node list
    output<-list(g=g,nl=npexp,el=edgefull,c=clouv)
    return(output)
}
adjm_wpk_vs_wpk_by_ww<-function(adjm){
    # multiply for the transpose
    adjm_s<-t(adjm)%*%(adjm)
    # generate the graph
    g<- graph.adjacency(adjm_s, mode="undirected",weighted="c", diag=FALSE)
    # do the thingies to take to gephi
    # create do calculations
    V(g)$nt[1:length(V(g))]<-"wpk"
    V(g)$ceg<-centr_eigen(g,
                          directed = F,
                          scale = T,
                          normalized = T)$vector
    clouv<-cluster_louvain(g)
    npcml<-membership(clouv)
    npcmlv<-as.numeric(npcml)
    # create the node list
    npexp<-cbind(as.character(names(V(g))),
                 as.character(names(V(g))),
                 V(g)$ceg,
                 npcmlv,
                 V(g)$nt)
    
    colnames(npexp) <- c("Id",
                         "Label",
                         "Eigenvector Centrality",
                         "MOD_Louv",
                         "N_type[z]")
    # create the edge list
    edge <- get.edgelist(g, names=T)# getting the Edgelist
    edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
    eT<-vector(mode = "character",length=length(edgeW))
    eT[]<-"Undirected"
    
    edgefull= cbind(edge,edgeW,eT)# joining them
    dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
    
    #output,
    #the graph,
    #the formated edge list
    #and the formated node list
    output<-list(g=g,nl=npexp,el=edgefull,c=clouv)
    return(output)
}# this is the one

multi_network_output<-adjm_2_multi_network(adjm_wkp_ww_o,adjm_ww_ow)
ow_network<-adjm_2_network_ow(adjm_ww_ow)
wpk_network<-adjm_2_network_wpk(adjm_wkp_ww_o)

multi_network_output_fb<-adjm_2_multi_network(adjm_wkp_ww_o_fb,adjm_ww_ow_fb)
ow_network_fb<-adjm_2_network_ow(adjm_ww_ow_fb)
wpk_network_fb<-adjm_2_network_wpk(adjm_wkp_ww_o_fb)

ww_vs_ww_network<-adjm_ww_vs_ww_by_wpk(adjm_wkp_ww_o)
wpk_vs_wpk_network<-adjm_wpk_vs_wpk_by_ww(adjm_wkp_ww_o)

ww_vs_ww_network_fb<-adjm_ww_vs_ww_by_wpk(adjm_wkp_ww_o_fb)
wpk_vs_wpk_network_fb<-adjm_wpk_vs_wpk_by_ww(adjm_wkp_ww_o_fb)

giant.component <- function(g) { 
    #g<-ww_vs_ww_network$g
    cl <- clusters(g) 
    gc<-induced.subgraph(g, which(cl$membership == which.max(cl$csize)))
    return(gc)
}
# get the big component
# do the communities
ww_vs_ww_network$gc<-giant.component(ww_vs_ww_network$g)
ww_vs_ww_network_fb$gc<-giant.component(ww_vs_ww_network_fb$g)

get_nl <- function(g){
  V(g)$ceg<-centr_eigen(g,
                        directed = F,
                        scale = T,
                        normalized = T)$vector
  clouv<-cluster_louvain(g)
  npcml<-membership(clouv)
  npcmlv<-as.numeric(npcml)
  npexp<-cbind(as.character(names(V(g))),
               as.character(names(V(g))),
               V(g)$ceg,
               npcmlv,
               V(g)$nt)
  
  colnames(npexp) <- c("Id",
                       "Label",
                       "Eigenvector Centrality",
                       "MOD_Louv",
                       "N_type[z]")
  output <- npexp
  return(output)
}
get_el <- function(g){
  edge <- get.edgelist(g, names=T)# getting the Edgelist
  edgeW <- get.edge.attribute(g,"c", index=E(g))# getting the weights of each link
  eT<-vector(mode = "character",length=length(edgeW))
  eT[]<-"Undirected"
  
  edgefull= cbind(edge,edgeW,eT)# joining them
  dimnames(edgefull) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
  output <- edgefull
  return(output)
}

ww_vs_ww_network_fb$gc$nl <- get_nl(ww_vs_ww_network_fb$gc)
ww_vs_ww_network$gc$nl <- get_nl(ww_vs_ww_network$gc)

ww_vs_ww_network_fb$gc$el <- get_el(ww_vs_ww_network_fb$gc)
ww_vs_ww_network$gc$el <- get_el(ww_vs_ww_network$gc)

#ww_vs_ww_network_fb$c
#ww_vs_ww_network$g

clouv<-cluster_louvain(ww_vs_ww_network$gc) # 17 clusters
clouv_fb<-cluster_louvain(ww_vs_ww_network_fb$gc)# 12 clusters

gc_comms<-igraph::groups(clouv)# goups is the list with all the words per commu
gc_comms_fb<-igraph::groups(clouv_fb)# goups is the list with all the words per commu
# from those words get the wkp related to them 
# first clean _w

reverting_node_tag<-function(x){
  output<-gsub("_w","",x)
  return(output)
}
#gc_comms_c<-lapply(gc_comms,reverting_node_tag)
# get the wpk for each cluster
# from the multi mode network i get which wpk are incident to the kws
get_wpks_from_ww<-function(x,wpk_network){
    #x<-gc_comms[1][[1]]# cluster ww names
    y<-adjacent_vertices(wpk_network$g, x)
    t<-lapply(y,names)
    z<-sort(table(do.call(c,t)), decreasing = T)# wpk names
    output<-list(ww=x,wpk=z)
    return(output)
}

#getting the internal centrality to rank the keywords.
#x gcomms, the nodes of the communities
#g: principal graph which will be subdivided
get_comm_Ne<-function(x,comm){
    buff<-x#getting the keywords of say cluster
    
    #buff<-gc_comms[1][[1]]
    #comm<-ww_vs_ww_network
    
    cIndex<-which(reverting_node_tag(V(comm$g)$name) %in% buff)# getting the index of the sub graph
    cIndex2<-which(reverting_node_tag(comm$nl[,"Label"]) %in% buff)
    
    gegc<-comm$nl[cIndex2,"Eigenvector Centrality"]
    gc<-induced_subgraph(comm$g,cIndex)
    egc<-centr_eigen(gc, directed = F,scale = T,normalized = T)$vector
    bc<-betweenness(gc,directed = FALSE, normalized = TRUE)
    ebc<-egc*bc
    #gc<-get_comm_egc(gc)
    gc$c<-cbind(names(V(gc)),egc,bc,ebc,gegc)
    gc$c<-gc$c[order(as.numeric(gc$c[,"ebc"])),decreasing =T]
    output<-gc
    return(output)
}

#ww_vs_ww_network$g
#gc_comms
# get the centralities for the little ones
comm_g<-lapply(gc_comms,get_comm_Ne,ww_vs_ww_network)
comm_g_fb<-lapply(gc_comms_fb,get_comm_Ne,ww_vs_ww_network_fb)

#comm_g[2][[1]]$c
# report to filter the keywords
ix<-1:length(comm_g)
ix_fb<-1:length(comm_g_fb)
# mapply function
report_comm_c<-function(ix,comm){
    #comm$c
    #comm_g[1][[1]]$c
    #ix<-1
    write.csv(comm$c,paste0("Comm_labels/ww_c",ix,".csv"))
    #write.csv(comm_g[1][[1]]$c,paste0("Comm_labels/ww_c",ix,".csv"))
    
}
mapply(report_comm_c,ix,comm_g)
mapply(report_comm_c,ix_fb,comm_g_fb)

ww_wpk_by_comm<-lapply(gc_comms,get_wpks_from_ww,wpk_network)
ww_wpk_by_comm_fb<-lapply(gc_comms_fb,get_wpks_from_ww,wpk_network_fb)

ww_wpk_by_comm[11]

# from each of them get the wpk they have connections to.

ww_vs_ww_network$c





wpk_vs_wpk_network$c


write.csv(wpk_vs_wpk_network$nl,"knowledge_size/wpk_vs_wpk_NL.csv")
write.csv(wpk_vs_wpk_network$el,"knowledge_size/wpk_vs_wpk_EL.csv")

write.csv(wpk_vs_wpk_network_fb$nl,"knowledge_size/wpk_vs_wpk_NL_fb.csv")
write.csv(wpk_vs_wpk_network_fb$el,"knowledge_size/wpk_vs_wpk_EL_fb.csv")
save(wpk_vs_wpk_network,file = "wpk_vs_wpk_network.rdata")
save(wpk_vs_wpk_network_fb,file = "wpk_vs_wpk_network_fb.rdata")


write.csv(ww_vs_ww_network_fb$nl,"knowledge_size/ww_NL_fb.csv")
write.csv(ww_vs_ww_network_fb$el,"knowledge_size/ww_EL_fb.csv")
save(ww_vs_ww_network_fb,file = "ww_vs_ww_network_fb.rdata")
save(ww_vs_ww_network,file = "ww_vs_ww_network.rdata")

write.csv(ww_vs_ww_network$nl,"knowledge_size/ww_NL.csv")
write.csv(ww_vs_ww_network$el,"knowledge_size/ww_EL.csv")

write.csv(ww_vs_ww_network_fb$nl,"knowledge_size/ww_NL_fb.csv")
write.csv(ww_vs_ww_network_fb$el,"knowledge_size/ww_EL_fb.csv")
save(ww_vs_ww_network_fb,file = "ww_vs_ww_network_fb.rdata")
save(ww_vs_ww_network,file = "ww_vs_ww_network.rdata")

write.csv(ww_vs_ww_network$gc$nl,"knowledge_size/ww_NL_gc_all.csv")
write.csv(ww_vs_ww_network$gc$el,"knowledge_size/ww_EL_gc_all.csv")
write.csv(ww_vs_ww_network_fb$gc$nl,"knowledge_size/ww_NL_gc_fb.csv")
write.csv(ww_vs_ww_network_fb$gc$el,"knowledge_size/ww_EL_gc_fb.csv")


write.csv(multi_network_output$nl,"knowledge_size/multi_NL.csv")
write.csv(multi_network_output$el,"knowledge_size/multi_EL.csv")

write.csv(multi_network_output_fb$nl,"knowledge_size/multi_NL_fb.csv")
write.csv(multi_network_output_fb$el,"knowledge_size/multi_EL_fb.csv")
save(multi_network_output_fb,file = "multi_network_output_fb.rdata")
save(multi_network_output,file = "multi_network_output.rdata")

write.csv(ow_network$nl,"knowledge_size/ow_NL.csv")
write.csv(ow_network$el,"knowledge_size/ow_EL.csv")

write.csv(ow_network_fb$nl,"knowledge_size/ow_NL_fb.csv")
write.csv(ow_network_fb$el,"knowledge_size/ow_EL_fb.csv")

write.csv(wpk_network$nl,"knowledge_size/wpk_NL.csv")
write.csv(wpk_network$el,"knowledge_size/wpk_EL.csv")

write.csv(wpk_network_fb$nl,"knowledge_size/wpk_NL_fb.csv")
write.csv(wpk_network_fb$el,"knowledge_size/wpk_EL_fb.csv")

#which(names(V(wpk_network$g))=="Ontology")

#incident(wpk_network$g,V(wpk_network$g)[32])

#lapply(i,function(), w,p,status)
write.csv(el_wkp_m,"knowledge_size/E_wkp_ww.csv")
write.csv(m_edg_ww_grouping,"knowledge_size/E_ww_ow.csv")
##### 
# THIRD CHECK for all:
# was to read the rescription of each word's description in wikipedia to 
# learn what they were and confirm. 
# Check the papers linked to the words.

# Words role are defined
# Make a clean list with all the verification steps and their relative results. 
# TO do today [WORK]
# done
# 100 words without classification after the three checks
# 88 words classified in the not exclusive categories of product/service, industry
# discipline

# -------------END-Classifying convergent words [Knowledge size]------------------------
# 10/11/2016
# The main analysis will be realized only with the discplines. 
# Pattern analysis - > new classification or propertity for the kwords.

file<-"excel/classified_keywords.csv" # words that have at least one classification.
dt_filtered_kw<-read.csv(file, header = T,stringsAsFactors =F)
dt_filtered_kw[,1]<-prePmix(dt_filtered_kw[,1])
dt_filtered_kw[,1]# 1257
v_unique_filtered_kw<-unique(dt_filtered_kw[,1])#

# check the union 
# a matrix with the data on the time behaviour of the targeted keywords. 

# PA: they appeared before and were not present in the last year. [convergent]
m_both[,which(colnames(m_both)%in%v_unique_filtered_kw)]#
v_m_both_filtered_words<-colnames(m_both)[which(colnames(m_both)%in%v_unique_filtered_kw)]
# PB: appeared before and also in the last year. [maybe convergent]
m_before[,which(colnames(m_before)%in%v_unique_filtered_kw)]#
v_m_before_filtered_words<-colnames(m_before)[which(colnames(m_before)%in%v_unique_filtered_kw)]
# PC: they appeared in the last year [maybe convergent]
m_last_year[,which(colnames(m_last_year)%in%v_unique_filtered_kw)]
v_m_last_year_filtered_words<-colnames(m_last_year)[which(colnames(m_last_year)%in%v_unique_filtered_kw)]
# PDummy: all of them
#m_filteredkw_centrality<-m_union[,which(colnames(m_union)%in%v_unique_filtered_kw)]#

# Getting the matrix of time behaviour of the union matrix.
m_filteredkw_centrality<-m_union[,which(colnames(m_union)%in%v_m_both_filtered_words)]#

write.csv(m_filteredkw_centrality,"output_time_behaviour_filtered_kw.csv")

# get for these selected words the centrality and the labels they belonged too. 
# I want to know if they are beneficiaries or big data or giver to big data or both. 
v_target<-v_m_both_filtered_words
v_target<-v_m_before_filtered_words
m_clabel_taget_keywords<-get_kword_clabel(f_input_general_clabel,v_target)

write.csv(m_clabel_taget_keywords,"output_labels_filtered_kw.csv")
# 37 words with discpline from 88. from those 37, 5 ambigous.

# se acabo esta vaina. 


###################################################
#Last thing, checking the papers of the sub groups#
###################################################
# confirming manual check with papers related to the term
load("aubc_santi_2016.RData")
s16<-aubc_santi_2016
s16_DE_ok<-s16[s16[,"DE"]!="",]
colnames(s16)
# find to which keywords a ww word refeers to
# use the ww to find which ow are related to it.
colnames(m_status_f)
head(m_status_f)

search_engine_exact_match<-function(w1){
    output<-paste0("\\b",w1,"\\b")
    return(output)
}
search_engine_exact_match_narrow<-function(w1){
    output<-paste0("^",w1,"$")
    return(output)
}



to_regx<-function(wv){
    rx<-unlist(lapply(wv,search_engine_exact_match))
    rx_ok<-paste0(rx,collapse = "|")
    output<-rx_ok
    return(output)
}
to_regx_narrow<-function(wv){
    rx<-unlist(lapply(wv,search_engine_exact_match_narrow))
    rx_ok<-paste0(rx,collapse = "|")
    output<-rx_ok
    return(output)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

split_kw<-function(kw,rx){
    #kw<-s16_DE_ok[1,"DE"]
    #rx<-rx_ow
    t_kw<-trim(unlist(strsplit(kw,";")))
    output<-any(grepl(rx,t_kw,ignore.case = T))
    return(output)
}
one_by_one_kw_check<-function(kwf,rx){
    # for each paper split their kw
    #kwf<-s16_DE_ok[,"DE"]
    #rx<-rx_ow
    # doing the thing
    output<-which(unlist(lapply(kwf,split_kw,rx)))
    return(output)
}

comm_g[1][[1]]$c
rownames(comm_g[2][[1]]$c)

# get all keywords to translate.
# get their ow.
# get the papers linked to their ow.
# save that on the comm object.

ww_f_target<-reverting_node_tag(rownames(comm_g[2][[1]]$c))
get_comm_papers_by_ow<-function(comm,m_status_f,s16_DE_ok){
    #comm<-comm_g[1][[1]]
    x<-comm$c
    ww_f_target<-reverting_node_tag(rownames(x))
    rx_ok<-to_regx(ww_f_target)
    ix<-grep(rx_ok,m_status_f[,"ww"])
    ax<-m_status_f[ix,"ow"]
    rx_ow<-to_regx_narrow(ax)
    pi<-one_by_one_kw_check(s16_DE_ok[,"DE"],rx_ow)
    ps<-s16_DE_ok[pi,c("TI","AB","DE","TC")]
    comm$p<-ps
    output<-comm
    return(output)
}
comm_g_e<-lapply(comm_g,get_comm_papers_by_ow,m_status_f,s16_DE_ok)

length(comm_g_e[11][[1]]$p[,c("TI")])
comm_g_e[11][[1]]$c

#
get_list_papers_by_ow<-function(l,m_status_f,s16_DE_ok){
    x<-reverting_node_tag(l)
    rx_ok<-to_regx(x)
    ix<-grep(rx_ok,m_status_f[,"ww"])
    ax<-m_status_f[ix,"ow"]
    rx_ow<-to_regx_narrow(ax)
    pi<-one_by_one_kw_check(s16_DE_ok[,"DE"],rx_ow)
    output<-length(pi)
    return(output)
}

## LONE WORDS
lw<-list("Ageing_w","Bayesian network_w","Breast cancer_w",
    "Cancer_w","Citizen science_w","Climate_w","Climate change_w",
    "Data fusion_w","Data structure_w","Diabetes mellitus_w",
    "Diagnosis_w","Distributed computing_w","Education_w",
    "Energy conservation_w","Epidemiology_w","Ethics_w",
    "Evaluation_w","Evolutionary computation_w","Fraud_w",
    "Genetics_w","Graph theory_w","Grid computing_w",
    "Health informatics_w","Influenza_w","Language_w",
    "Maximum likelihood estimation_w","Meta-analysis_w",
    "Neuroscience_w","Obesity_w","Outline of object recognition_w",
    "Process mining_w","R_w","Remote sensing_w","Sustainability_w",
    "Technology_w","Theory_w","Uncertainty_w",
    "Wireless sensor network_w")
p_lw<-lapply(lw,get_list_papers_by_ow,m_status_f,s16_DE_ok)
##

## SMALL COMPONENTS
ww_target<-c("List of algorithms","algorithms")
ww_target<-c("Biodiversity","Biomarkers","Biogeography")
ww_target<-c("Performance appraisal","Performance analysis")
ww_target<-c("Software engineering", "Automation")
ww_target<-c("QOS", "Quality of service")
ww_target<-c("Multivariate analysis", "Multivariate statistics")
ww_target<-c("Phylogenetic tree", "Phylogenetics")
ww_target<-c("Management","Workflow","Scientific workflow system")
ww_target<-c("Cloud", "Cloud platform", "Cloud computing")
###


rx_ok<-to_regx(ww_target)

ix<-grep(rx_ok,m_status_f[,"ww"])
#ix<-grep("Phylogenetic tree",m_status_f[,"ww"])
ax<-m_status_f[ix,"ow"]
grep("collection",m_status_f[,"ow"],ignore.case = T,value = T)
#implement an exact keyword matching approach.
#############################################
#format the words to be beginning and end of word
rx_ow<-to_regx_narrow(ax)
#for each paper search, split their keywords and check if they match, 
#
pi<-one_by_one_kw_check(s16_DE_ok[,"DE"],rx_ow)
ps<-s16_DE_ok[pi,c("TI","AB","DE","TC")]
length(ps[,"TI"])

#colnames(ps)
ops<-ps[order(ps[,"TC"],decreasing = T),]
head(ops[,"TI"])
head(ops[,"DE"])

colnames(s16_DE_ok)
write.csv(m_status_f,"m_status_f.csv")
#############################################


#############################

