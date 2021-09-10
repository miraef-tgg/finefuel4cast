# Algotihm for creating 'hi density' structure

# takes in the fuel and prod data, and puts into 'hi density' format
# updating for 2020 on 20210407
# iteratively finds first non-NA values, and then next non-NA values until it reaches the end of each row

# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path


# function make_m :  makes matrix with coords of non-NA values of given matrix

make_m<-function(fuel_data2){
  M<-matrix(data=NA,nrow=nrow(fuel_data2), ncol = ncol(fuel_data2))
  
  for (i in 1:nrow(fuel_data2)){
    for (j in 1:ncol(fuel_data2)){
      M[i,j]<-ifelse(is.na(fuel_data2[i,j]), NA, paste(i,",",j))
    }
  }
  
  M2<-as.vector(t(M))
  M3<-(subset(M2, !is.na(M2)))
  M4<-matrix(data=NA, nrow=length(M3),ncol=2)
  rows<-sapply(strsplit(M3, ","), "[", 1)
  cols<-sapply(strsplit(M3, ","), "[", 2)
  M<-cbind(rows,cols)
  Mrow<-as.vector(as.integer(M[,1]))
  Mcol<-as.vector(as.integer(M[,2]))
  M_final<-as.array(cbind(as.integer(Mrow),as.integer(Mcol)))
  return(M_final)
}



# other_real_data<-readRDS("G:/My Drive/finefuel4cast/Fuels_model/Bayesian_Land/stan_model_scripts/newer_data_list.rds")
real_data<-readRDS("G:/My Drive/finefuel4cast_old/Fuels_model/Bayesian_Land/stan_model_scripts/stan_data/new_data_list.rds")


# View(fuels_model_data)

fuel_data2<-real_data$log_fuel_obs_all
# fuel_data2<-replace(fuel_data2, fuel_data2==0, NA)
fuel_data2<-as.data.frame(fuel_data2)
colnames(fuel_data2)<-paste0("y",(1:ncol(fuel_data2)))

#goal: for every location, return index of all continuous observations

# start of obs for each loc
obs_start<-vector()
for (i in 1:nrow(fuel_data2)){
  obs_start[i]<-min(which(fuel_data2[i,]!='NA'))
}

next_na_vec<-vector()
for (i in 1:nrow(fuel_data2)){
  if (obs_start[i]==25) {next_na_vec[i]<-26
  }else{
    na_ind<-(which(is.na(fuel_data2[i,((obs_start[i]+1):ncol(fuel_data2))])))
    if(length(na_ind)>0){
      next_na_vec[i]<-obs_start[i]+min(na_ind)
    } else {next_na_vec[i]<-26}
    
  }
}
  

new_M1<-cbind(obs_start, (next_na_vec-1))
# View(new_M1)

## --------------------------------------- 2 -------------------------------------

# SECOND start of obs for each loc
obs_start2<-vector()
for (i in 1:nrow(fuel_data2)){
  if (new_M1[i,2] < 24 ) { 
    temp<-(which(fuel_data2[i,((next_na_vec[i]+1):ncol(fuel_data2))]!='NA')) +next_na_vec[i]
    if(length(temp)>0){
        obs_start2[i]<-min(temp)
    }else{obs_start2[i]<-NA}
    
  } else{
    obs_start2[i]<-NA}
}

new_M2<-cbind(new_M1, obs_start2)
# View(new_M2)

# SECOND start of Nas for each loc
obs_start<-obs_start2
next_na_vec<-vector()

for (i in 1:nrow(fuel_data2)){
  if (is.na(new_M2[i,3])) {next_na_vec[i]<-NA
  }else if(new_M2[i,3]==25) {next_na_vec[i]<-26
  }else{
    na_ind<-(which(is.na(fuel_data2[i,((obs_start[i]+1):ncol(fuel_data2))])))
    if(length(na_ind)>0){
      next_na_vec[i]<-obs_start[i]+min(na_ind)
    } else {next_na_vec[i]<-26}
    
  }
}


new_M3<-cbind(new_M2, (next_na_vec-1))
# View(new_M3)

## --------------------------------------- 3 -------------------------------------

# THIRD start of obs for each loc
obs_start3<-vector()

for (i in 1:nrow(fuel_data2)){
  if (!is.na(new_M3[i,4]) & new_M3[i,4] < 24  ) { 
    temp<-(which(fuel_data2[i,((next_na_vec[i]+1):ncol(fuel_data2))]!='NA')) +next_na_vec[i]
    if(length(temp)>0){
      obs_start3[i]<-min(temp)
    }else{obs_start3[i]<-NA}
    
  } else{
    obs_start3[i]<-NA}
}

new_M4<-cbind(new_M3, obs_start3)
# View(new_M4)

# THIRD start of Nas for each loc
obs_start<-obs_start3
next_na_vec<-vector()

for (i in 1:nrow(fuel_data2)){
  if (is.na(new_M4[i,5])) {next_na_vec[i]<-NA
  }else{
    na_ind<-(which(is.na(fuel_data2[i,((obs_start[i]+1):ncol(fuel_data2))])))
    if(length(na_ind)>0){
      next_na_vec[i]<-obs_start[i]+min(na_ind)
    } else {next_na_vec[i]<-26}
    
  }
}

new_M5<-cbind(new_M4, (next_na_vec-1))
# View(new_M5)


## --------------------------------------- 4 -------------------------------------

# FOURTH start of obs for each loc
obs_start4<-vector()

for (i in 1:nrow(fuel_data2)){
  if (!is.na(new_M5[i,6]) & new_M5[i,6] < 24  ) { 
    temp<-(which(fuel_data2[i,((next_na_vec[i]+1):ncol(fuel_data2))]!='NA')) +next_na_vec[i]
    if(length(temp)>0){
      obs_start4[i]<-min(temp)
    }else{obs_start4[i]<-NA}
    
  } else{
    obs_start4[i]<-NA}
}

new_M6<-cbind(new_M5, obs_start4)
# View(new_M6)

# FOURTH start of Nas for each loc
obs_start<-obs_start4
next_na_vec<-vector()

for (i in 1:nrow(fuel_data2)){
  if (is.na(new_M6[i,7])) {next_na_vec[i]<-NA
  }else{
    na_ind<-(which(is.na(fuel_data2[i,((obs_start[i]+1):ncol(fuel_data2))])))
    if(length(na_ind)>0){
      next_na_vec[i]<-obs_start[i]+min(na_ind)
    } else {next_na_vec[i]<-26}
    
  }
}

new_M7<-cbind(new_M6, (next_na_vec-1))
# View(new_M7)



## --------------------------------------- 5 -------------------------------------

# FOURTH start of obs for each loc
obs_start5<-vector()

for (i in 1:nrow(fuel_data2)){
  if (!is.na(new_M7[i,8]) & new_M7[i,8] < 24  ) { 
    temp<-(which(fuel_data2[i,((next_na_vec[i]+1):ncol(fuel_data2))]!='NA')) +next_na_vec[i]
    if(length(temp)>0){
      obs_start5[i]<-min(temp)
    }else{obs_start5[i]<-NA}
    
  } else{
    obs_start5[i]<-NA}
}

new_M8<-cbind(new_M7, obs_start5)
# View(new_M8)

# FIFTH start of Nas for each loc
obs_start<-obs_start5
next_na_vec<-vector()

for (i in 1:nrow(fuel_data2)){
  if (is.na(new_M8[i,9])) {next_na_vec[i]<-NA
  }else{
    na_ind<-(which(is.na(fuel_data2[i,((obs_start[i]+1):ncol(fuel_data2))])))
    if(length(na_ind)>0){
      next_na_vec[i]<-obs_start[i]+min(na_ind)
    } else {next_na_vec[i]<-26}
    
  }
}

new_M9<-cbind(new_M8, (next_na_vec-1))
# View(new_M9)
# View(fuel_data2)
#val 25 represents 2021

# useful rows to check
# row 1 should have 4 sets of start/stop
# row 6 should have one long continuous stretch
# row 129 should have 5 (!)

#next thjing: each row = loc, remove all sets where start=stop

M10<-new_M9
for ( i in 1:nrow(M10)){
  if(M10[i,1]==M10[i,2] & !is.na(M10[i,1])) ( M10[i,1:2]<-NA)
  if(M10[i,3]==M10[i,4] & !is.na(M10[i,3])) ( M10[i,3:4]<-NA)
  if(M10[i,5]==M10[i,6] & !is.na(M10[i,5])) ( M10[i,5:6]<-NA)
  if(M10[i,7]==M10[i,8] & !is.na(M10[i,7])) ( M10[i,7:9]<-NA)
  if(M10[i,9]==M10[i,10] & !is.na(M10[i,9]))( M10[i,9:10]<-NA)
  }

# View(M10)

#replace 25 (2020) w/ 24 (2019)

# M10<-replace(M10, M10==25, 24)
# table(M10)
#next: smush
m_row<-0
M11<-matrix(NA,nrow=100000,ncol=2)
m_row_end=0
m_row_start=1

Fstart_vec<-vector()

f<-1
for ( i in 1:nrow(M10)){
  (row<-as.vector(na.omit(M10[i,])))
  count=1
  Fstart_vec[i]<-row[1]; f=f+1
  
  if(length(row)>0){
  for (j in 1:(length(row)/2)) {
    (seqs<-seq(row[count], row[(count+1)]))
    (new_rows<-cbind(rep(i, length(seqs)),seqs))
    # print(c(count,"  :  " ,(new_rows)))
    m_row_end<-m_row_end+nrow(new_rows)
    # print(c(m_row_start,"  <-start   end ->  " ,(m_row_end)))
    M11[m_row_start:m_row_end,1]<-new_rows[,1]
    M11[m_row_start:m_row_end,2]<-new_rows[,2]
    m_row_start<-m_row_end +1
    count<-count+2
  }
  }
}

M11<-na.omit(M11)

# View(M11)
#remove 2020 ;(

# M12<-replace(M11, M11[,2]==25,NA)
M12<-M11

M12<-na.omit(M12)

# View(M12)
## now move to prod space

#where should prod start in Bayes?
2020-24 #yr 1 obs =1996
# seq(from=1996,by=1, length.out=25)
#1st yr prod = 1986
seq(from=1986,by=1, length.out=34)

#yr 1 in obs = 1996 = yr 11 in prod; subtract 10
seq(from=1986,by=1, length.out=34)[11]

Fvec_orig<-Fstart_vec
Fstart_vec<-Fvec_orig
obs_vec<-cbind(1:length(Fstart_vec), (Fstart_vec-1))
obs_vec<-na.omit(obs_vec)
# View(Fstart_vec)
Fstart_vec<-cbind(1:length(Fstart_vec), (Fstart_vec+9))
Fstart_vec<-na.omit(Fstart_vec)
dim(Fstart_vec)
dim(obs_vec)
# View(Fstart_vec)

#check row 66
seq(1996,2020,1)[obs_vec[16,2]]
seq(1986,2019,1)[Fstart_vec[16,2]]


#what locations are being included?

prod_keeps<-Fstart_vec[!(is.na(Fstart_vec[,2])),1]
# View(prod_keeps)
length(prod_keeps)

translate<-as.data.frame(cbind(unique(M12[,1]),seq(1:length(prod_keeps))))
M13<-as.data.frame(M12)
names(M13)<-c("orig_loc", "yr")
names(translate)<-c("orig_loc", "new_loc")

M14<-plyr::join(M13, translate)
M15<-as.matrix(cbind(M14$new_loc, M14$yr))


## use M10 to index prod
#but remove 25 again?
# M10_new<-replace(M10, M10==25, 24)
M10_new<-M10
M10_new[1:5,]

dim(M10_new)
highest_year<-function(data) {max(data[seq(2,10,2)],na.rm=TRUE)}
lowest_year<-function(data) {min(data[seq(2,10,2)],na.rm=TRUE)}

prod_end<-apply(M10_new[prod_keeps,], 1, max,na.rm=TRUE)
prod_start<-apply(M10_new[prod_keeps,], 1, min, na.rm=TRUE)

prod_M<-cbind(prod_start,prod_end)
dim(prod_M)

prod_M2<-as.data.frame(matrix(NA,nrow=0,ncol=2))
count<-1
m_row_end=0
m_row_start=1
year_length<-vector()

for ( i in 1:nrow(prod_M)){
  row<-seq(prod_M[i,1],prod_M[i,2])
  year_length[i]<-length(row)
  prod_seq<-cbind(rep(i, length(row)), row)
  m_row_end<-m_row_end+nrow(prod_seq)
  prod_M2[m_row_start:m_row_end,1:2]<-prod_seq[,1:2]
  m_row_start<-m_row_end +1
}


# View(prod_M2)

# real_data<-readRDS("G:/My Drive/finefuel4cast/Fuels_model/Bayesian_Land/stan_model_scripts/stan_data/newest_data_list.rds")
# real_data<-readRDS("G:/My Drive/finefuel4cast/Fuels_model/Bayesian_Land/stan_model_scripts/new_data_list.rds")

Fstart<-vector()
new_Fstart<-vector()
for ( i in 1:nrow(Fstart_vec)){
  Fstart[i]<-real_data$raw_prod_all[Fstart_vec[i,1], Fstart_vec[i,2]]
  new_Fstart[i]<- Fstart_vec[i,2]-(9)
  
}
length(Fstart)
sum(is.na(Fstart))
M<-M15

# View(Fstart[1:100,])
# View(new_Fstart)
# new_Fstart[1] + year_length[1] -1

# new_Fstart[1] + year_length[1] -1
prod_yr1_index<-as.data.frame(new_Fstart) 
prod_start_val<-(Fstart)


##### (where I combined these two scripts) ######################
real_data<-readRDS("G:/My Drive/finefuel4cast_old/Fuels_model/Bayesian_Land/stan_model_scripts/stan_data/newest_data_list.rds")
prod_data_orig<-real_data$raw_prod_all[prod_keeps,11:35] #1996-2020
fuel_data_orig<-real_data$raw_fuel_obs_all[prod_keeps,1:25] #1996-2020

#matrices to hold deviations and standardized
fuel_data_dev<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_dev<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))
fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

for(i in 1:nrow(fuel_data_z)){
  fuel_data_dev[i,]<-fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_dev[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))
  fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,], na.rm=T)
  prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
}

# View(fuel_data_z)
year_length<-as.data.frame(year_length)
new_Fstart<-as.data.frame(new_Fstart)
Fstart<-rep(0,length(Fstart))

#replace indexed over with NA
prod_incl<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))
prod_replace<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))

for ( i in 1:nrow(prod_data_z) ){
  replacement<-as.vector(prod_data_z[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))])
  prod_incl[i,(1:(year_length[i,1]))]<-replacement
  prod_replace[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))]<-replacement
}

prod_m<-make_m(prod_replace)
nOutputs<-nrow(prod_m)

#is it a first value?
first_val_vec<-vector()
for ( i in 2:nrow(prod_m)){
  prev<-i-1
  if (prod_m[i,1]!=prod_m[prev,1]) (first_val_vec[i]<-1)
  if (prod_m[i,1]==prod_m[prev,1]) (first_val_vec[i]<-0)
  
}

first_val_vec[1]<-1
prod_data_vec<-na.omit(as.vector(t(as.matrix(prod_replace))))
nLocs<-length(unique(M[,1]))
Fvec<-vector()

prod_m_match<-as.data.frame(prod_m); names(prod_m_match)<-c("loc", "yr")
prod_m_match$loc_yr<-paste0(prod_m_match$loc,"_", prod_m_match$yr)

m_match<-as.data.frame(M); names(m_match)<-c("loc", "yr")
m_match$loc_yr<-paste0(m_match$loc,"_", m_match$yr)

for ( i in 1:nrow(prod_m_match)){
  prod_m_match$match[i]<- prod_m_match$loc_yr[i] %in% m_match$loc_yr
}

prod_m_match$index<-seq(1,nrow(prod_m_match),1)
prod_m_match2<-prod_m_match[prod_m_match$match,]
M_prod_vec<-as.data.frame(prod_m_match2[,5])

#first year for each location
prod_m_match$dupe<-duplicated(prod_m_match$loc)
F0_start<-subset(prod_m_match, prod_m_match$dupe==F)
prod_data_prev_z<-matrix(NA, nrow=nLocs,ncol=26)
prod_data_prev<-real_data$raw_prod_all[prod_keeps,10:35]


for(i in 1:nrow(fuel_data_z)){
  prod_data_prev_z[i,]<-(prod_data_prev[i,]-mean(prod_data_prev[i,]))/sd(prod_data_prev[i,])
}


F0_yr<-F0_start$yr

F0<-vector()
for (i in 1:148) {
  F0[i]<-prod_data_prev_z[i, F0_yr[i]]
}

#data for stan:
print(c("nOutputs: ",  (nOutputs)))
print(c("first_val_vec: ",  class(first_val_vec), length(first_val_vec),sum(first_val_vec==1)))
print(c("prod_data_vec: ", range(prod_data_vec)))
print(c("M_prod_vec: ", nrow (M_prod_vec)))
print(c("prev_prod: F0: ", range (F0)))
print(range(fuel_data_z, na.rm=T))

nYears<-ncol(fuel_data_orig)
nMiss=nrow(M)
###


fuels_model_meta_data<-list(nLocs, nYears, nMiss, nOutputs, 
                            F0,prod_data_vec,first_val_vec,
                            M,M_prod_vec, prod_m_match,
                            year_length, prod_keeps,prod_data_vec, prod_replace,Fstart_vec)
names(fuels_model_meta_data)<-c("nLocs", "nYears", "nMiss", "nOutputs",
                                "P0","prod_data_vec","first_val_vec",
                                "fuels_obs_index","prod_index", "prod_m_match",
                                "year_length", "row_keeps","prod_data_vec", "prod_replace", "Fstart_vec")

#year length = vector of years to include per location
#prod keeps = which locations (rows of original 198) to keep
#new_Fstart

fuels_model_data<-list(real_data$raw_fuel_obs_all, real_data$raw_prod_all, real_data$field_office, real_data$` coords`, real_data$eco,
                       real_data$samp, real_data$site_names_all)
names(fuels_model_data)<-c("fine_fuel_obs", "prod", "field_office", "coords", "eco", "samp", "site_names")


saveRDS(fuels_model_meta_data, "G:/My Drive/finefuel4cast_old/finefuel4cast/Fuels_model/fuels_model_data/fuels_model_meta_data.rds")
saveRDS(fuels_model_data, "G:/My Drive/finefuel4cast_old/finefuel4cast/Fuels_model/fuels_model_data/fuels_model_data.rds")
