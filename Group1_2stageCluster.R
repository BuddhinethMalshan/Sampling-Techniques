attach(Dataset_01)
data=Dataset_01

#--------------------------Two Stage Cluster-----------------------------------#

#SAMPLE1

set.seed(013001)
clusters1 = sample(unique(data$Province),size=5,replace=F)
clusters1

one_cluster_samp1 = data[data$Province %in% clusters1,]
one_cluster_samp1

table(one_cluster_samp1$Province)

#n = nrow(one_cluster_samp1)
n=rsampcalc(nrow(one_cluster_samp1),e=3,ci=95)

library(sampler)

two_cluster_samp_size1 = ssampcalc(df=one_cluster_samp1,n ,strata=`Province`)
two_cluster_samp_size1

set.seed(013001)
two_cluster_samp1 = ssamp(df=one_cluster_samp1 ,n ,strata=`Province`)
two_cluster_samp1

pw1=0
for(i in 1:741){
  if(two_cluster_samp1$Province[i]=="Eastern"){
    pw1=round((591/181)*(9/5),2)
  }else if(two_cluster_samp1$Province[i]=="North-central"){
    pw1=round((368/113)*(9/5),2)
  }else if(two_cluster_samp1$Province[i]=="North-western"){
    pw1=round((615/188)*(9/5),2)
  }else if(two_cluster_samp1$Province[i]=="Sabaragamuwa"){
    pw1=round((476/146)*(9/5),2)
  }else if(two_cluster_samp1$Province[i]=="Uva"){
    pw1=round((370/113)*(9/5),2)
  }
  pw1 = c(pw1)
}

two_clust1 = cbind(two_cluster_samp1,pw1)
two_clust1

library(survey)

clust1=svydesign(id=~`Province`+`Grama Niladhari Division ID`, weights=~`pw1`, data=two_clust1)

svymean(~`Birth weight`,design=clust1)

svymean(~`Mother's age`,design=clust1)

svymean(~`No. of clinical visits`,design=clust1)

svymean(~`No. of months pregnant`,design=clust1)

svymean(~`Mother's education`,design=clust1)

svymean(~`Child's gender`,design=clust1)

svymean(~`Wealth index quintile`,design=clust1)

svymean(~`Ethnicity`,design=clust1)

svymean(~`Province`,design=clust1)

svymean(~`Sector`,design=clust1)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=clust1)

svymean(~`Mother's height`,design=clust1)

svymean(~`Mother's BMI`,design=clust1)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=clust1)

svytotal(~`Child's gender`, desig=clust1)

#Regression Estimation

two_clust1
plot(two_clust1$`No. of clinical visits`,two_clust1$`Birth weight`)
#plot(two_clust1$`Birth weight`,two_clust1$`Wealth index quintile`)
plot(two_clust1$`Mother's age`,two_clust1$`Birth weight`)

plot(two_clust1$`No. of months pregnant`,two_clust1$`Birth weight`)
SLR1 = lm(`Birth weight`~`No. of months pregnant`,two_clust1)
SLR1

mean_BirthWeight=-635.3+392.6*mean(data$`No. of months pregnant`)
mean_BirthWeight


#SAMPLE2

set.seed(023001)
clusters2 = sample(unique(data$Province),size=5,replace=F)
clusters2

one_cluster_samp2 = data[data$Province %in% clusters2,]
one_cluster_samp2

table(one_cluster_samp2$Province)

#n = nrow(one_cluster_samp2)

n=rsampcalc(nrow(one_cluster_samp2),e=3,ci=95)

two_cluster_samp_size2 = ssampcalc(df=one_cluster_samp2, n ,strata=`Province`)
two_cluster_samp_size2



set.seed(023001)
two_cluster_samp2 = ssamp(df=one_cluster_samp2 , n ,strata=`Province`)
two_cluster_samp2

pw2=0
for(i in 1:767){
  if(two_cluster_samp2$Province[i]=="Eastern"){
    pw2=round(((591/167)*(9/5)),2)
  }else if(two_cluster_samp2$Province[i]=="Central"){
    pw2=round(((665/188)*(9/5)),2)
  }else if(two_cluster_samp2$Province[i]=="North-western"){
    pw2=round(((615/174)*(9/5)),2)
  }else if(two_cluster_samp2$Province[i]=="Sabaragamuwa"){
    pw2=round(((476/134)*(9/5)),2)
  }else if(two_cluster_samp2$Province[i]=="Uva"){
    pw2=round(((370/104)*(9/5)),2)
  }
  pw2 =(c(pw2))
}

two_clust2 = cbind(two_cluster_samp2,pw2)
two_clust2

library(survey)

clust2=svydesign(id=~`Province`+`Grama Niladhari Division ID`, weights=~`pw2`, data=two_clust2)

svymean(~`Birth weight`,design=clust2)

svymean(~`Mother's age`,design=clust2)

svymean(~`No. of clinical visits`,design=clust2)

svymean(~`No. of months pregnant`,design=clust2)

svymean(~`Mother's education`,design=clust2)

svymean(~`Child's gender`,design=clust2)

svymean(~`Wealth index quintile`,design=clust2)

svymean(~`Ethnicity`,design=clust2)

svymean(~`Province`,design=clust2)

svymean(~`Sector`,design=clust2)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=clust2)

svymean(~`Mother's height`,design=clust2)

svymean(~`Mother's BMI`,design=clust2)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=clust2)

svytotal(~`Child's gender`, desig=clust2)

#Regression Estimation

two_clust2
plot(two_clust2$`No. of clinical visits`,two_clust2$`Birth weight`)
#plot(two_clust2$`Birth weight`,two_clust2$`Wealth index quintile`)
plot(two_clust2$`Mother's age`,two_clust2$`Birth weight`)

plot(two_clust2$`No. of months pregnant`,two_clust2$`Birth weight`)
SLR2 = lm(`Birth weight`~`No. of months pregnant`,two_clust2)
SLR2


mean_BirthWeight=349.4+282.9*mean(data$`No. of months pregnant`)
mean_BirthWeight