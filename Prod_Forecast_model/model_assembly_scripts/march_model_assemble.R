#This combines RAP, spatial, and temporal data csvs and splits, does some seasonal temporal aggregation, and splits dataframes into 1987-2020 and 2021
# changes format from wide to long
# aggregates monthly weather data to October-march, april and may for precipitation (pr), temperature (tmmx) and vapor pressure deficit (vpd)
# selects previous  month for ndvi
# standardizes everything
# makes a lot of plots to check that my code is doing what I think it is

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

prod_orig<-read.csv("gee_4cast_data/prod_4cast_csvs/gee_RAP_data.csv")
clim_data_orig<-read.csv("gee_4cast_data/prod_4cast_csvs/gee_temporal_data.csv")
spat_data_orig<-read.csv("gee_4cast_data/prod_4cast_csvs/gee_spatial_data.csv")

prod<-prod_orig
clim<-clim_data_orig
spat<-spat_data_orig
nLocs<-nrow(spat)

memory.limit(10000000000)

## ---------------------------------------------- agb  ------------------------------------------------
# prev year, and standardized

#adding prev yr, long term, abg, sd to prod df, making into long format
afg_data<-prod[(names(prod) %in% c(names(prod)[(grepl("afgAGB", names(prod)))]))]
pfg_data<-prod[(names(prod) %in% c(names(prod)[(grepl("pfgAGB", names(prod)))]))]
agb_data<-afg_data+pfg_data
# View(head(agb_data)); View(head(afg_data));View(head(pfg_data))
options(scipen=999)
agb_data[,36]<-rowMeans(agb_data, na.rm=TRUE) #per pixel avg
agb_data[,37]<-apply(X=agb_data[,1:35], MARGIN=1, FUN=sd, na.rm=TRUE) #per pixel sd
range(agb_data[,36],na.rm=TRUE)
range(agb_data[,37],na.rm=TRUE)
agb_data<-replace(agb_data, agb_data==0, 1)

agb_data<-as.matrix(agb_data)
new_df<-matrix(NA,nrow=nrow(prod)*36,ncol=5)
yr=1986
for (i in 0:34){
  new_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-agb_data[,i+1]
  if(i!=0)  (new_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-agb_data[,i])
  new_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-yr
  new_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-agb_data[,36]
  new_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-agb_data[,37] 
  yr<-yr+1
}

for (i in 35:35){
  new_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-NA
  new_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-agb_data[,i]
  new_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-yr
  new_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-agb_data[,36]
  new_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-agb_data[,37] 
  yr<-yr+1
}


agb_df<-as.data.frame(new_df)
names(agb_df)<-c("agb", "prev_agb", "yr", "avg_agb", "sd_agb")
agb_df$z_agb<-(agb_df$avg_agb-agb_df$agb)/agb_df$sd_agb 
agb_df$prev_z_agb<-(agb_df$avg_agb-agb_df$prev_agb)/agb_df$sd_agb #right way?

agb_df$yr<-agb_df$yr
agb_df$prev_z_agb<-agb_df$prev_z_agb
hist((agb_df$prev_z_agb))
hist(agb_df$z_agb)

sum(is.infinite(agb_df$z_agb), na.rm=T)
sum(agb_df$avg_agb==0, na.rm=T)

# View(agb_df[(nLocs-100):(nLocs+100),])
# View(agb_df[(nLocs*35-100):(nLocs*35+100),])


## ---------------------------------------------- agb fract ------------------------------------------------
frac_data<-prod[(names(prod) %in% c(names(prod)[(grepl("percent", names(prod)))]))]
frac<-frac_data[(names(frac_data) %in% c(names(frac_data)[!(grepl("shr", names(frac_data)))]))] 
head(frac)

frac_df<-matrix(NA,nrow=nrow(frac)*36,ncol=3)
yr=1986
odds<-seq(1,36*2,2)
evens<-seq(2,36*2,2)

for (i in 0:35){
  nLocs=nrow(frac)
  even<-evens[i]
  odd<-odds[i]
  if(i==0) (frac_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-NA)
  if(i==0)  (frac_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-NA)
  if(i!=0) frac_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-frac[,odd]
  if(i!=0)  (frac_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-frac[,even])
  frac_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-yr
  yr<-yr+1
}

frac_df<-as.data.frame(frac_df)
names(frac_df)<-c("prev_afg", "prev_pfg","year")

par(mfrow=c(1,1))
hist(frac_df$prev_afg)
hist(frac_df$prev_pfg, add=TRUE, col=rgb(1,0,0,.1))

unique(frac_df$year)
## ---------------------------------------------- pr ------------------------------------------------
pr_data<-clim
pr_data<-pr_data[(names(pr_data) %in% c(names(pr_data)[(grepl("pr", names(pr_data)))]))] #just pr
pr_data_all<-pr_data

#all months
pr_april<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_4_")])]
pr_april_avg<-rowMeans(pr_april); pr_april_sd<-apply(pr_april,1,sd,na.rm=TRUE); 
pr_may<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_5_")])]
pr_may_avg<-rowMeans(pr_may); pr_may_sd<-apply(pr_may,1,sd,na.rm=TRUE); 
pr_june<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_6_")])]
pr_june_avg<-rowMeans(pr_june); pr_june_sd<-apply(pr_june,1,sd,na.rm=TRUE); 
pr_july<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_7_")])]
pr_july_avg<-rowMeans(pr_july); pr_july_sd<-apply(pr_july,1,sd,na.rm=TRUE); 
pr_aug<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_8_")])]
pr_aug_avg<-rowMeans(pr_aug); pr_aug_sd<-apply(pr_aug,1,sd,na.rm=TRUE); 
pr_sep<-pr_data[,(names(pr_data)[substring(names(pr_data),2,5)%in% c("r_9_")])]
pr_sep_avg<-rowMeans(pr_sep); pr_sep_sd<-apply(pr_sep,1,sd,na.rm=TRUE); 

yr=1986
prev_yr<-1985
everything_pr_df<-matrix(NA,nrow=nrow(pr_data)*36,ncol=14)
for (i in 0:35){
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-(pr_april[1:nLocs,(i+1)]-pr_april_avg)/pr_april_sd
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-(pr_may[1:nLocs,(i+1)]-pr_may_avg)/pr_may_sd
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-(pr_june[1:nLocs,(i+1)]-pr_june_avg)/pr_june_sd
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-(pr_july[1:nLocs,(i+1)]-pr_july_avg)/pr_july_sd
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),6]<-(pr_aug[1:nLocs,(i+1)]-pr_aug_avg)/pr_aug_sd
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),7]<-(pr_sep[1:nLocs,(i+1)]-pr_sep_avg)/pr_sep_sd
  
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),8]<-ifelse(pr_april[1:nLocs,(i+1)]-pr_april_avg>0, 1, 0)
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-ifelse(pr_may[1:nLocs,(i+1)]-pr_may_avg>0, 1, 0)
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),10]<-ifelse(pr_june[1:nLocs,(i+1)]-pr_june_avg>0, 1, 0)
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),11]<-ifelse(pr_july[1:nLocs,(i+1)]-pr_july_avg>0, 1, 0)
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),12]<-ifelse(pr_aug[1:nLocs,(i+1)]-pr_aug_avg>0, 1, 0)
  everything_pr_df[(nLocs*i+1):(nLocs*i+nLocs),13]<-ifelse(pr_sep[1:nLocs,(i+1)]-pr_sep_avg>0, 1, 0)
  yr<-yr+1;
}
everything_pr_df<-as.data.frame(everything_pr_df)
names(everything_pr_df)<-c("yr",  "z_pr_april", "z_pr_may","z_pr_june","z_pr_july","z_pr_aug","z_pr_sep",
                           "forecast_pr_april", "forecast_pr_may","forecast_pr_june","forecast_pr_july","forecast_pr_aug","forecast_pr_sep" )
hist(everything_pr_df$z_pr_april)
hist(everything_pr_df$z_pr_may)
hist(everything_pr_df$z_pr_june)
hist(everything_pr_df$z_pr_july)
hist(everything_pr_df$z_pr_aug)
hist(everything_pr_df$z_pr_sep)
table(everything_pr_df$forecast_pr_april)
table(everything_pr_df$forecast_pr_may)
table(everything_pr_df$forecast_pr_june)
table(everything_pr_df$forecast_pr_july)
table(everything_pr_df$forecast_pr_aug)
table(everything_pr_df$forecast_pr_sep)

#subsetting to oct-march
keeps<-names(pr_data)[substring(names(pr_data),2,5)%in% c("r_1_","r_2_","r_3_","r_10","r_11","r_12")]
pr_data<-pr_data[, keeps]
# View(pr_data)
pr_data$avg_pr<-rowSums(pr_data, na.rm=TRUE)/35 #avg sum oct-march per year
dim(pr_data)
pr_data$avg_pr_annual<-apply(pr_data_all, 1, sum, na.rm=TRUE)/35 #avg sum per year

# pr_data$sd_pr<-apply(pr_data[,1:249], 1, sd, na.rm=TRUE)
# I *think* I want to sd of current yr sum, so doing later, as vector

yr=1986
prev_yr<-1985
pr_df<-matrix(NA,nrow=nrow(pr_data)*36,ncol=(9))
all_pr_df<-matrix(NA,nrow=nrow(pr_data)*36,ncol=(12))

for (i in 0:35){
  # print(yr)
  pr_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  #cur yr  data
  temp_df<-pr_data[, names(pr_data) %in% c(names(pr_data)[(grepl(as.character(yr), names(pr_data)))])]
  pr_df[(nLocs*i+1):(nLocs*i+nLocs),2:4]<-as.matrix(temp_df[,1:3]) #jan/feb/march of cur yr
  #prev yr data
  prev_df<-pr_data[, names(pr_data) %in% c(names(pr_data)[(grepl(as.character(prev_yr), names(pr_data)))])]
  pr_df[(nLocs*i+1):(nLocs*i+nLocs),5:7]<-as.matrix(prev_df[,4:6])
  pr_df[(nLocs*i+1):(nLocs*i+nLocs),8]<-pr_data$avg_pr
  pr_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-pr_data$avg_pr_annual
  
  #oct-oct rain  all months ucrr year EXCEPT oct nov dec
  # temp_df2<-pr_data_all[, names(pr_data_all) %in% c(names(pr_data_all)[(grepl(as.character(yr), names(pr_data_all)))])]
  # all_pr_df[(nLocs*i+1):(nLocs*i+nLocs),1:9]<-as.matrix(temp_df2[,1:9]) #jan-oct of cur yr
  # all_pr_df[(nLocs*i+1):(nLocs*i+nLocs),10:12]<-as.matrix(prev_df[,4:6]) #oct-dec of prev yr
  # pr_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-mean(all_pr_df[(nLocs*i+1):(nLocs*i+nLocs), 1:12])

  yr<-yr+1; prev_yr<-prev_yr+1
}

pr_df<-as.data.frame(pr_df)
new_names<-c("yr", c(paste0("cur", c(1,2,3)), c(paste0("prev",c(10,11,12)))), "avg_pr", "avg_annual_pr")
names(pr_df)<-new_names
# View(head(pr_df))
pr_df$cur_pr<-pr_df$prev10 +pr_df$prev11 +pr_df$prev12 + pr_df$cur1 + pr_df$cur2 + pr_df$cur3 
pr_df$frac_pr=(pr_df$cur1 + pr_df$cur2 + pr_df$cur3)/pr_df$avg_pr


range(pr_df$frac_pr, na.rm=TRUE)
hist(pr_df$frac_pr)
sum(is.na(pr_df$frac_pr))

sd_pr<-vector()
for (loc in 1:nLocs){
  loc_seq<-seq(loc,35*nLocs,nLocs)
  sd_pr[loc]<-sd(pr_df$cur_pr[loc_seq], na.rm=TRUE)  
}

pr_df$sd_pr<-sd_pr
pr_df$z_pr<-(pr_df$cur_pr-pr_df$avg_pr)/pr_df$sd_pr
hist(pr_df$z_pr)


locs_not_zero<-as.data.frame(cbind(pr_df$sd_pr[1:nLocs],pr_df$avg_pr[1:nLocs]))
locs_not_zero$cv_pr<-locs_not_zero[,1]/locs_not_zero[,2]
(mean(locs_not_zero$cv_pr))


## ---------------------------------------------- tmmx ------------------------------------------------
tmmx_data<-clim
tmmx_data<-tmmx_data[(names(tmmx_data) %in% c(names(tmmx_data)[(grepl("tmmx", names(tmmx_data)))]))] #just tmmx

#all months
tmmx_april<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_4_")])]
tmmx_april_avg<-rowMeans(tmmx_april); tmmx_april_sd<-apply(tmmx_april,1,sd,na.rm=TRUE); 
tmmx_may<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_5_")])]
tmmx_may_avg<-rowMeans(tmmx_may); tmmx_may_sd<-apply(tmmx_may,1,sd,na.rm=TRUE); 
tmmx_june<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_6_")])]
tmmx_june_avg<-rowMeans(tmmx_june); tmmx_june_sd<-apply(tmmx_june,1,sd,na.rm=TRUE); 
tmmx_july<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_7_")])]
tmmx_july_avg<-rowMeans(tmmx_july); tmmx_july_sd<-apply(tmmx_july,1,sd,na.rm=TRUE); 
tmmx_aug<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_8_")])]
tmmx_aug_avg<-rowMeans(tmmx_aug); tmmx_aug_sd<-apply(tmmx_aug,1,sd,na.rm=TRUE); 
tmmx_sep<-tmmx_data[,(names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_9_")])]
tmmx_sep_avg<-rowMeans(tmmx_sep); tmmx_sep_sd<-apply(tmmx_sep,1,sd,na.rm=TRUE); 

yr=1986
prev_yr<-1985
everything_tmmx_df<-matrix(NA,nrow=nrow(tmmx_data)*36,ncol=14)
for (i in 0:35){
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-(tmmx_april[1:nLocs,(i+1)]-tmmx_april_avg)/tmmx_april_sd
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-(tmmx_may[1:nLocs,(i+1)]-tmmx_may_avg)/tmmx_may_sd
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-(tmmx_june[1:nLocs,(i+1)]-tmmx_june_avg)/tmmx_june_sd
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-(tmmx_july[1:nLocs,(i+1)]-tmmx_july_avg)/tmmx_july_sd
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),6]<-(tmmx_aug[1:nLocs,(i+1)]-tmmx_aug_avg)/tmmx_aug_sd
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),7]<-(tmmx_sep[1:nLocs,(i+1)]-tmmx_sep_avg)/tmmx_sep_sd
  
    everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),8]<-ifelse(tmmx_april[1:nLocs,(i+1)]-tmmx_april_avg>0, 1, 0)
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-ifelse(tmmx_may[1:nLocs,(i+1)]-tmmx_may_avg>0, 1, 0)
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),10]<-ifelse(tmmx_june[1:nLocs,(i+1)]-tmmx_june_avg>0, 1, 0)
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),11]<-ifelse(tmmx_july[1:nLocs,(i+1)]-tmmx_july_avg>0, 1, 0)
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),12]<-ifelse(tmmx_aug[1:nLocs,(i+1)]-tmmx_aug_avg>0, 1, 0)
  everything_tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),13]<-ifelse(tmmx_sep[1:nLocs,(i+1)]-tmmx_sep_avg>0, 1, 0)
  yr<-yr+1;
}
everything_tmmx_df<-as.data.frame(everything_tmmx_df)
names(everything_tmmx_df)<-c("yr", "z_tmmx_april", "z_tmmx_may","z_tmmx_june","z_tmmx_july","z_tmmx_aug","z_tmmx_sep",
                             "forecast_tmmx_april", "forecast_tmmx_may","forecast_tmmx_june","forecast_tmmx_july","forecast_tmmx_aug","forecast_tmmx_sep" )
hist(everything_tmmx_df$z_tmmx_april)  
hist(everything_tmmx_df$z_tmmx_may)  
hist(everything_tmmx_df$z_tmmx_june)  
hist(everything_tmmx_df$z_tmmx_july)  
hist(everything_tmmx_df$z_tmmx_aug)  
hist(everything_tmmx_df$z_tmmx_sep)  

table(everything_tmmx_df$forecast_tmmx_april)  
table(everything_tmmx_df$forecast_tmmx_may)  
table(everything_tmmx_df$forecast_tmmx_june)  
table(everything_tmmx_df$forecast_tmmx_july)  
table(everything_tmmx_df$forecast_tmmx_aug)  
table(everything_tmmx_df$forecast_tmmx_sep)  

#subsetting to oct-march
keeps<-names(tmmx_data)[substring(names(tmmx_data),4,7)%in% c("x_1_","x_2_","x_3_","x_10","x_11","x_12")]
tmmx_data<-tmmx_data[, keeps]
tmmx_data$avg_tmmx<-rowMeans(tmmx_data, na.rm=TRUE) #avg sum per year

# tmmx_data$sd_tmmx<-apply(tmmx_data[,1:249], 1, sd, na.rm=TRUE)
# I *think* I want to sd of current yr sum, so doing later, as vector

yr=1986
prev_yr<-1985
tmmx_df<-matrix(NA,nrow=nrow(tmmx_data)*36,ncol=(8))

for (i in 0:35){
  # print(yr)
  tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  #cur yr  data
  temp_df<-tmmx_data[, names(tmmx_data) %in% c(names(tmmx_data)[(grepl(as.character(yr), names(tmmx_data)))])]
  tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),2:4]<-as.matrix(temp_df[,1:3]) #jan/feb/march of cur yr
  #prev yr data
  prev_df<-tmmx_data[, names(tmmx_data) %in% c(names(tmmx_data)[(grepl(as.character(prev_yr), names(tmmx_data)))])]
  tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),5:7]<-as.matrix(prev_df[,4:6])
  tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),8]<-tmmx_data$avg_tmmx
  # tmmx_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-tmmx_data$sd_tmmx
  
  yr<-yr+1; prev_yr<-prev_yr+1
}

tmmx_df<-as.data.frame(tmmx_df)
new_names<-c("yr", c(paste0("cur", c(1,2,3)), c(paste0("prev",c(10,11,12)))), "avg_tmmx")
names(tmmx_df)<-new_names
tmmx_df$cur_tmmx<-(tmmx_df$prev10 +tmmx_df$prev11 +tmmx_df$prev12 + tmmx_df$cur1 + tmmx_df$cur2 + tmmx_df$cur3)/6

sd_tmmx<-vector()
for (loc in 1:nLocs){
  loc_seq<-seq(loc,35*nLocs,nLocs)
  sd_tmmx[loc]<-sd(tmmx_df$cur_tmmx[loc_seq], na.rm=TRUE)  
}

tmmx_df$sd_tmmx<-sd_tmmx
tmmx_df$z_tmmx<-(tmmx_df$cur_tmmx-tmmx_df$avg_tmmx)/tmmx_df$sd_tmmx
hist(tmmx_df$z_tmmx)

locs_not_zero<-as.data.frame(cbind(tmmx_df$sd_tmmx[1:nLocs],tmmx_df$avg_tmmx[1:nLocs]))
locs_not_zero$cv_tmmx<-locs_not_zero[,1]/locs_not_zero[,2]
(mean(locs_not_zero$cv_tmmx))

## ---------------------------------------------- vpd ------------------------------------------------
vpd_data<-clim
vpd_data<-vpd_data[(names(vpd_data) %in% c(names(vpd_data)[(grepl("vpd", names(vpd_data)))]))] #just vpd


#all months
vpd_april<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_4_")])]
vpd_april_avg<-rowMeans(vpd_april); vpd_april_sd<-apply(vpd_april,1,sd,na.rm=TRUE); 
vpd_may<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_5_")])]
vpd_may_avg<-rowMeans(vpd_may); vpd_may_sd<-apply(vpd_may,1,sd,na.rm=TRUE); 
vpd_june<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_6_")])]
vpd_june_avg<-rowMeans(vpd_june); vpd_june_sd<-apply(vpd_june,1,sd,na.rm=TRUE); 
vpd_july<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_7_")])]
vpd_july_avg<-rowMeans(vpd_july); vpd_july_sd<-apply(vpd_july,1,sd,na.rm=TRUE); 
vpd_aug<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_8_")])]
vpd_aug_avg<-rowMeans(vpd_aug); vpd_aug_sd<-apply(vpd_aug,1,sd,na.rm=TRUE); 
vpd_sep<-vpd_data[,(names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_9_")])]
vpd_sep_avg<-rowMeans(vpd_sep); vpd_sep_sd<-apply(vpd_sep,1,sd,na.rm=TRUE); 

yr=1986
prev_yr<-1985
everything_vpd_df<-matrix(NA,nrow=nrow(vpd_data)*36,ncol=8)
for (i in 0:35){
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-(vpd_april[1:nLocs,(i+1)]-vpd_april_avg)/vpd_april_sd
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-(vpd_may[1:nLocs,(i+1)]-vpd_may_avg)/vpd_may_sd
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-(vpd_june[1:nLocs,(i+1)]-vpd_june_avg)/vpd_june_sd
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-(vpd_july[1:nLocs,(i+1)]-vpd_july_avg)/vpd_july_sd
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),6]<-(vpd_aug[1:nLocs,(i+1)]-vpd_aug_avg)/vpd_aug_sd
  everything_vpd_df[(nLocs*i+1):(nLocs*i+nLocs),7]<-(vpd_sep[1:nLocs,(i+1)]-vpd_sep_avg)/vpd_sep_sd
  yr<-yr+1;
}
everything_vpd_df<-as.data.frame(everything_vpd_df)
names(everything_vpd_df)<-c("yr", "z_vpd_april", "z_vpd_may","z_vpd_june","z_vpd_july","z_vpd_aug","z_vpd_sep" )
hist(everything_vpd_df$z_vpd_april)  
hist(everything_vpd_df$z_vpd_may)  
hist(everything_vpd_df$z_vpd_june)  
hist(everything_vpd_df$z_vpd_july)  
hist(everything_vpd_df$z_vpd_aug)  
hist(everything_vpd_df$z_vpd_sep)  

#subsetting to oct-march
keeps<-names(vpd_data)[substring(names(vpd_data),3,6)%in% c("d_1_","d_2_","d_3_","d_10","d_11","d_12")]
vpd_data<-vpd_data[, keeps]
vpd_data$avg_vpd<-rowMeans(vpd_data, na.rm=TRUE) #avg sum per year

yr=1986
prev_yr<-1985
vpd_df<-matrix(NA,nrow=nrow(vpd_data)*36,ncol=(8))

for (i in 0:35){
  # print(yr)
  vpd_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  #cur yr  data
  temp_df<-vpd_data[, names(vpd_data) %in% c(names(vpd_data)[(grepl(as.character(yr), names(vpd_data)))])]
  vpd_df[(nLocs*i+1):(nLocs*i+nLocs),2:4]<-as.matrix(temp_df[,1:3]) #jan/feb/march of cur yr
  #prev yr data
  prev_df<-vpd_data[, names(vpd_data) %in% c(names(vpd_data)[(grepl(as.character(prev_yr), names(vpd_data)))])]
  vpd_df[(nLocs*i+1):(nLocs*i+nLocs),5:7]<-as.matrix(prev_df[,4:6])
  vpd_df[(nLocs*i+1):(nLocs*i+nLocs),8]<-vpd_data$avg_vpd
  # vpd_df[(nLocs*i+1):(nLocs*i+nLocs),9]<-vpd_data$sd_vpd
  
  yr<-yr+1; prev_yr<-prev_yr+1
}

vpd_df<-as.data.frame(vpd_df)
new_names<-c("yr", c(paste0("cur", c(1,2,3)), c(paste0("prev",c(10,11,12)))), "avg_vpd")
names(vpd_df)<-new_names
vpd_df$cur_vpd<-(vpd_df$prev10 +vpd_df$prev11 +vpd_df$prev12 + vpd_df$cur1 + vpd_df$cur2 + vpd_df$cur3)/6

sd_vpd<-vector()
for (loc in 1:nLocs){
  loc_seq<-seq(loc,35*nLocs,nLocs)
  sd_vpd[loc]<-sd(vpd_df$cur_vpd[loc_seq], na.rm=TRUE)  
}

vpd_df$sd_vpd<-sd_vpd
vpd_df$z_vpd<-(vpd_df$cur_vpd-vpd_df$avg_vpd)/vpd_df$sd_vpd
hist(vpd_df$z_vpd)


locs_not_zero<-as.data.frame(cbind(vpd_df$sd_vpd[1:nLocs],vpd_df$avg_vpd[1:nLocs]))
locs_not_zero$cv_vpd<-locs_not_zero[,1]/locs_not_zero[,2]
(mean(locs_not_zero$cv_vpd))

# View(vpd_df[(nLocs-100):(nLocs+100),])


## ---------------------------------------------- ndvi ------------------------------------------------

########### ndvi
#avg val in february
ndvi_data<-clim
ndvi_data<-ndvi_data[(names(ndvi_data) %in% c(names(ndvi_data)[(grepl("ndvi", names(ndvi_data)))]))] #just vpd
# View(ndvi_data)
#all months
ndvi_april<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_4_")])]
ndvi_april_avg<-rowMeans(ndvi_april); ndvi_april_sd<-apply(ndvi_april,1,sd,na.rm=TRUE); 
ndvi_may<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_5_")])]
ndvi_may_avg<-rowMeans(ndvi_may); ndvi_may_sd<-apply(ndvi_may,1,sd,na.rm=TRUE); 
ndvi_june<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_6_")])]
ndvi_june_avg<-rowMeans(ndvi_june); ndvi_june_sd<-apply(ndvi_june,1,sd,na.rm=TRUE); 
ndvi_july<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_7_")])]
ndvi_july_avg<-rowMeans(ndvi_july); ndvi_july_sd<-apply(ndvi_july,1,sd,na.rm=TRUE); 
ndvi_aug<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_8_")])]
ndvi_aug_avg<-rowMeans(ndvi_aug); ndvi_aug_sd<-apply(ndvi_aug,1,sd,na.rm=TRUE); 
ndvi_sep<-ndvi_data[,(names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_9_")])]
ndvi_sep_avg<-rowMeans(ndvi_sep); ndvi_sep_sd<-apply(ndvi_sep,1,sd,na.rm=TRUE); 

yr=1986
prev_yr<-1985
everything_ndvi_df<-matrix(NA,nrow=nrow(ndvi_data)*36,ncol=8)
for (i in 0:35){
  everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-(ndvi_april[1:nLocs,(i+1)]-ndvi_april_avg)/ndvi_april_sd
  everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-(ndvi_may[1:nLocs,(i+1)]-ndvi_may_avg)/ndvi_may_sd
  everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),4]<-(ndvi_june[1:nLocs,(i+1)]-ndvi_june_avg)/ndvi_june_sd
  if (i != 35){
    everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),5]<-(ndvi_july[1:nLocs,(i+1)]-ndvi_july_avg)/ndvi_july_sd
    everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),6]<-(ndvi_aug[1:nLocs,(i+1)]-ndvi_aug_avg)/ndvi_aug_sd
    everything_ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),7]<-(ndvi_sep[1:nLocs,(i+1)]-ndvi_sep_avg)/ndvi_sep_sd
    yr<-yr+1;
  }
}
everything_ndvi_df<-as.data.frame(everything_ndvi_df)
names(everything_ndvi_df)<-c("yr", "z_ndvi_april", "z_ndvi_may","z_ndvi_june","z_ndvi_july","z_ndvi_aug","z_ndvi_sep" )
hist(everything_ndvi_df$z_ndvi_april)  
hist(everything_ndvi_df$z_ndvi_may)  
hist(everything_ndvi_df$z_ndvi_june)  
hist(everything_ndvi_df$z_ndvi_july)  
hist(everything_ndvi_df$z_ndvi_aug)  
hist(everything_ndvi_df$z_ndvi_sep)  

keeps<-names(ndvi_data)[substring(names(ndvi_data),4,7)%in% c("i_2_")]
ndvi_data<-ndvi_data[,keeps]
ndvi_data$avg_ndvi=rowMeans(ndvi_data,na.rm=TRUE)

table(is.na(ndvi_data$avg_ndvi))
range(ndvi_data$avg_ndvi, na.rm=TRUE)

yr=1986
ndvi_df<-matrix(NA,nrow=nrow(ndvi_data)*36,ncol=(3))
nLocs=nrow(ndvi_data)

for (i in 0:35){
  # print(yr)
  ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),1]<-yr
  #cur yr  data
  ndvi_df_temp<-ndvi_data[, names(ndvi_data) %in% c(names(ndvi_data)[(grepl(as.character(yr), names(ndvi_data)))])]
  # if (ncol(ndvi_df_temp) >0) (ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-ndvi_df_temp)
  ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),2]<-ndvi_df_temp
  ndvi_df[(nLocs*i+1):(nLocs*i+nLocs),3]<-ndvi_data$avg_ndvi
  yr<-yr+1; 
}

ndvi_df<-as.data.frame(ndvi_df)
names(ndvi_df)<-c("yr", "cur_ndvi", "avg_ndvi")

sd_ndvi<-vector()
for (loc in 1:nLocs){
  loc_seq<-seq(loc,34*nLocs,nLocs)
  sd_ndvi[loc]<-sd(ndvi_df$cur_ndvi[loc_seq], na.rm=TRUE)  
}

ndvi_df$sd_ndvi<-sd_ndvi
ndvi_df$z_ndvi<-(ndvi_df$cur_ndvi-ndvi_df$avg_ndvi)/ndvi_df$sd_ndvi
hist(ndvi_df$z_ndvi)

locs_not_zero<-as.data.frame(cbind(ndvi_df$sd_ndvi[1:nLocs],ndvi_df$avg_ndvi[1:nLocs]))
locs_not_zero$cv_ndvi<-locs_not_zero[,1]/locs_not_zero[,2]
(mean(locs_not_zero$cv_ndvi, na.rm=TRUE))
(mean(abs(locs_not_zero$cv_ndvi), na.rm=TRUE))

range(ndvi_df$cur_ndvi,na.rm=TRUE)
range(ndvi_df$avg_ndvi,na.rm=TRUE)
sum(is.na(ndvi_df$avg_ndvi))
sum(is.na(ndvi_df$cur_ndvi))
hist(ndvi_df$z_ndvi)
# View(ndvi_df[(nLocs-100):(nLocs+100),])


## ---------------------------------------------- ST data ------------------------------------------------

#making change_var for all ST data
pr_df$change_pr<-pr_df$cur_pr-pr_df$avg_pr
tmmx_df$change_tmmx<-tmmx_df$cur_tmmx-tmmx_df$avg_tmmx
vpd_df$change_vpd<-vpd_df$cur_vpd-vpd_df$avg_vpd
ndvi_df$change_ndvi<-ndvi_df$cur_ndvi-ndvi_df$avg_ndvi


hist(pr_df$z_pr)
hist(tmmx_df$z_tmmx)
hist(vpd_df$z_vpd)
hist(ndvi_df$z_ndvi)


## ---------------------------------------------- Spat data ------------------------------------------------

#basically need to change replicate 36x

########## coords, elev, soil
spat<-spat
spat_df<-data.frame(matrix(NA,nrow=(36*nLocs),ncol=7))

count=1
for (yr in 1986:2021){
  # print(yr)
  spat_df[((count-1)*nLocs+1):(nLocs*count),1:7]<-spat[,1:7]

  count=count+1
}
names(spat_df)<-c(names(spat))
dim(agb_df)

############################################# making 'model_df' #########################
model_df<-as.data.frame(cbind(spat_df,agb_df, frac_df$prev_afg, frac_df$prev_pfg,
                              pr_df$cur_pr,pr_df$avg_pr,pr_df$change_pr,pr_df$z_pr, pr_df$frac_pr,
                              tmmx_df$cur_tmmx,tmmx_df$avg_tmmx,tmmx_df$change_tmmx,tmmx_df$z_tmmx,
                              vpd_df$cur_vpd,vpd_df$avg_vpd,vpd_df$change_vpd,vpd_df$z_vpd,
                              ndvi_df$cur_ndvi,ndvi_df$avg_ndvi, ndvi_df$change_ndvi, ndvi_df$z_ndvi,
                             
                              
                               # not binary future covs
                              everything_pr_df$z_pr_april,everything_pr_df$z_pr_may, everything_pr_df$z_pr_june, 
                              everything_pr_df$z_pr_july,everything_pr_df$z_pr_aug, everything_pr_df$z_pr_sep,
                              everything_tmmx_df$z_tmmx_april,everything_tmmx_df$z_tmmx_may, everything_tmmx_df$z_tmmx_june,
                              everything_tmmx_df$z_tmmx_july,everything_tmmx_df$z_tmmx_aug, everything_tmmx_df$z_tmmx_sep,
                              everything_vpd_df$z_vpd_april,everything_vpd_df$z_vpd_may, everything_vpd_df$z_vpd_june,
                              everything_vpd_df$z_vpd_july,everything_vpd_df$z_vpd_aug, everything_vpd_df$z_vpd_sep,
                              everything_ndvi_df$z_ndvi_april,everything_ndvi_df$z_ndvi_may, everything_ndvi_df$z_ndvi_june,
                              everything_ndvi_df$z_ndvi_july,everything_ndvi_df$z_ndvi_aug, everything_ndvi_df$z_ndvi_sep,
                              
                              
                              #binary forecast covs
                              everything_pr_df$forecast_pr_april,everything_pr_df$forecast_pr_may, everything_pr_df$forecast_pr_june,
                              everything_pr_df$forecast_pr_july,everything_pr_df$forecast_pr_aug, everything_pr_df$forecast_pr_sep,
                              everything_tmmx_df$forecast_tmmx_april,everything_tmmx_df$forecast_tmmx_may, everything_tmmx_df$forecast_tmmx_june,
                              everything_tmmx_df$forecast_tmmx_july,everything_tmmx_df$forecast_tmmx_aug, everything_tmmx_df$forecast_tmmx_sep
))


model_df<-as.data.frame(model_df)
dim(model_df)
# View(head(model_df))
# View(model_df[(nLocs):(nLocs*2+100),]) View(model_df[(nLocs):(nLocs*2+100),])

new_names<-c(names(spat_df), names(agb_df),"prev_afg_frac", "prev_pfg_frac",
                   "cur_pr","avg_pr","change_pr", "z_pr", "pr_frac",
                   "cur_tmmx","avg_tmmx","change_tmmx", "z_tmmx",
                   "cur_vpd","avg_vpd","change_vpd", "z_vpd",
                   "cur_ndvi","avg_ndvi", "change_ndvi",  "z_ndvi",
             "z_pr_april","z_pr_may", "z_pr_june" ,
             "z_pr_july","z_pr_aug", "z_pr_sep" ,
             "z_tmmx_april","z_tmmx_may", "z_tmmx_june" ,
             "z_tmmx_july","z_tmmx_aug", "z_tmmx_sep" ,
             "z_vpd_april","z_vpd_may", "z_vpd_june",
             "z_vpd_july","z_vpd_aug", "z_vpd_sep" ,
             "z_ndvi_april","z_ndvi_may", "z_ndvi_june",
             "z_ndvi_july","z_ndvi_aug", "z_ndvi_sep" ,
             "forecast_pr_april","forecast_pr_may", "forecast_pr_june" ,
             "forecast_pr_july","forecast_pr_aug", "forecast_pr_sep" ,
             "forecast_tmmx_april","forecast_tmmx_may", "forecast_tmmx_june" ,
             "forecast_tmmx_july","forecast_tmmx_aug", "forecast_tmmx_sep" 
            )



names(model_df)<-new_names
#reorder
model_df<-model_df[, c("agb", "prev_agb", "yr", "avg_agb", "sd_agb", "z_agb", "prev_z_agb",
                       
# model_df<-model_df[, c("agb", "prev_agb", "yr", "avg_agb", "sd_agb", "prev_z_agb",
                   "prev_afg_frac", "prev_pfg_frac",
                   "cur_pr","avg_pr","change_pr", "z_pr", "pr_frac",
                   "cur_tmmx","avg_tmmx","change_tmmx", "z_tmmx",
                   "cur_vpd","avg_vpd","change_vpd", "z_vpd",
                   "cur_ndvi","avg_ndvi", "change_ndvi",  "z_ndvi",
                   "z_pr_april","z_pr_may", "z_pr_june" ,
                   "z_pr_july","z_pr_aug", "z_pr_sep" ,
                   "z_tmmx_april","z_tmmx_may", "z_tmmx_june" ,
                   "z_tmmx_july","z_tmmx_aug", "z_tmmx_sep" ,
                   "z_vpd_april","z_vpd_may", "z_vpd_june",
                   "z_vpd_july","z_vpd_aug", "z_vpd_sep" ,
                   "z_ndvi_april","z_ndvi_may", "z_ndvi_june",
                   "z_ndvi_july","z_ndvi_aug", "z_ndvi_sep" ,
                   "long", "lat", "z_elev","z_ssm","smp","z_bulk_dens", "text_bin",
                   "forecast_pr_april","forecast_pr_may", "forecast_pr_june" ,
                   "forecast_pr_july","forecast_pr_aug", "forecast_pr_sep" ,
                   "forecast_tmmx_april","forecast_tmmx_may", "forecast_tmmx_june" ,
                   "forecast_tmmx_july","forecast_tmmx_aug", "forecast_tmmx_sep" 
                   )]
dim(model_df)
#make numeric
# model_df <-as.data.frame(sapply( model_df, as.numeric ))

#check
head(model_df)
options(scipen = 999)
model_df_key<-data.frame()
for (i in 1:ncol(model_df)){
  print(paste0(names(model_df)[i], " : ", range(model_df[,i], na.rm=TRUE)))
  print(paste0(names(model_df)[i], " : ", sum(is.na(model_df[,i]))))
  # hist(as.numeric(model_df[,i]), main=names(model_df)[i])
  model_df_key[i,1]<-names(model_df)[i]
  model_df_key[i,2]<-round(min(model_df[,i], na.rm=TRUE),2) 
  model_df_key[i,3]<-round(max(model_df[,i], na.rm=TRUE),2)
  model_df_key[i,4]<-round(mean(model_df[,i], na.rm=TRUE) ,2)
  model_df_key[i,5]<-round(sd(model_df[,i], na.rm=TRUE) ,2)
  model_df_key[i,6]<-sum(is.na(model_df[,i]))
}
names(model_df_key)<-c("name", "min", "max", "mean", "sd", "count_NA")
# View(model_df[(nLocs):(nLocs*2+100),]) 


## --------------------- cleaning and subsetting ----------------------------------
model_df_pre2021<-subset(model_df, model_df$yr!=2021)
model_df_pre2021<-na.omit(model_df_pre2021)
model_df<-rbind(model_df_pre2021, subset(model_df, model_df$yr==2021))

dim(model_df)


# subsetting so prev_agb always available
model_df2<-subset(model_df, model_df$yr>=1987)
model_df2$row_keeps<-seq(1:nrow(model_df2))

dim(model_df2)
(nLocs<-nrow(model_df2)/34) #number of locations
length(seq(1987,2020,1)) #number of years
length(unique(paste0(round(model_df2$long,7))))
length(unique(paste0(round(model_df2$lat,7))))

# subsetting to only coords in every year (locs that never had any NAs)

model_df2$long_lat<-paste0(substr(as.character(model_df2$long),0,7),"_",
                           substr(as.character(model_df2$lat),0,6))

length(unique(model_df2$long_lat))

coords<-as.data.frame(cbind(model_df2$yr,paste0(substr(as.character(model_df2$long),0,7),"_",
                                                substr(as.character(model_df2$lat),0,6))))
names(coords)<-c("yr", "long_lat")
counts<-coords %>%
  group_by(long_lat) %>%
  tally()
table(counts$n)

coords_keeps<-subset(counts, counts$n==35)
model_df2<-subset(model_df2, model_df2$long_lat %in% as.vector(coords_keeps$long_lat))
dim(model_df2)
nrow(model_df2)/35


# subetting to >100 long term prod 
model_df2<-subset(model_df2, model_df2$avg_agb>log(100))
dim(model_df2)
(nLocs<-nrow(model_df2)/35)

#coords
coord_df<-as.data.frame(cbind(model_df2$long, model_df2$lat))[1:nLocs,]
names(coord_df)<-c("long", "lat")

# adding in interraction terms
model_df2$prev_afg_frac_z_pr<-model_df2$prev_pfg_frac*model_df2$z_pr
model_df2$prev_z_agb_prev_afg_frac<-model_df2$prev_z_agb*model_df2$prev_afg_frac
model_df2$z_pr_z_ssm<-model_df2$z_pr*model_df2$z_ssm #2 way




loc_keeps<-as.data.frame(cbind(model_df2$long,model_df2$lat,model_df2$yr, model_df2$row_keeps ))
names(loc_keeps)<-c("long", "lat", "yr", "row_keeps")
write.csv(loc_keeps,  "prod_model_outputs/loc_keeps.csv")

# names(loc_keeps)<-c("long", "lat", "yr", "row_keeps")
loc_keeps<-subset(loc_keeps, loc_keeps$yr==1987)
# View(loc_keeps)
nrow(loc_keeps)


model_df2020<-subset(model_df2, model_df2$yr!=2021)
model_df2021<-subset(model_df2, model_df2$yr==2021)

unique(model_df2$yr)
dim(model_df2020)
dim(model_df2021)

write.csv(model_df2020,"gee_4cast_data/model_csvs/march_all_model_csv.csv", row.names = F)
write.csv(model_df2021,"gee_4cast_data/model_csvs/march_forecast_2021_csv.csv", row.names = F)


