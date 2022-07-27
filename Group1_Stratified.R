attach(Dataset_01)
data=Dataset_01

library(sampler)

#--------------------------------Stratified------------------------------------#

#SAMPLE1


Strat_size = ssampcalc(df =data,n = 893, strata =`Mother's BMI`)
Strat_size
set.seed(013001)
stratified1 = ssamp(df=data,n=893, strata =`Mother's BMI`)
stratified1

pw1=0
for(i in 1:893){

if(stratified1$`Mother's BMI`[i]=="BMI < 18.5"){
  pw1 = round((639/105),2)
}else if(stratified1$`Mother's BMI`[i]=="BMI 18.5-24.9"){
  pw1 = round((2651/435),2)
}else if(stratified1$`Mother's BMI`[i]=="BMI 24.9-29.9"){
  pw1 = round((1572/258),2)
}else if(stratified1$`Mother's BMI`[i]=="BMI 30 and over"){
  pw1 = round((584/96),2)
}
#pw=c(print(pw))
}

strat1 = cbind(stratified1,pw1)
strat1


library(survey)

sample1=svydesign(id=~1,strata =~`Mother's BMI`,weights=~`pw1` ,data=strat1)

svymean(~`Birth weight`,design=sample1)

svymean(~`Mother's age`,design=sample1)

svymean(~`No. of clinical visits`,design=sample1)

svymean(~`No. of months pregnant`,design=sample1)

svymean(~`Mother's education`,design=sample1)

svymean(~`Child's gender`,design=sample1)

svymean(~`Wealth index quintile`,design=sample1)

svymean(~`Ethnicity`,design=sample1)

svymean(~`Province`,design=sample1)

svymean(~`Sector`,design=sample1)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=sample1)

svymean(~`Mother's height`,design=sample1)

svymean(~`Mother's BMI`,design=sample1)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=sample1)

svytotal(~`Child's gender`, desig=sample1)

#Regression Estimation

sample1
plot(stratified1$`No. of clinical visits`,stratified1$`Birth weight`)
#plot(stratified1$`Birth weight`,stratified1$`Wealth index quintile`)
plot(stratified1$`Mother's age`,stratified1$`Birth weight`)

plot(stratified1$`No. of months pregnant`,stratified1$`Birth weight`)
SLR1 = lm(`Birth weight`~`No. of months pregnant`,stratified1)
SLR1

mean_BirthWeight=-102.4+337.9*mean(data$`No. of months pregnant`)
mean_BirthWeight


#SAMPLE2

Strat_size = ssampcalc(df =data,n = 893, strata =`Mother's BMI`)
Strat_size
set.seed(023001)
stratified2 = ssamp(df=data,n, strata =`Mother's BMI`)
stratified2

pw2=0
for(i in 1:893){
  
  if(stratified1$`Mother's BMI`[i]=="BMI < 18.5"){
    pw2 = round((639/105),2)
  }else if(stratified1$`Mother's BMI`[i]=="BMI 18.5-24.9"){
    pw2 = round((2651/435),2)
  }else if(stratified1$`Mother's BMI`[i]=="BMI 24.9-29.9"){
    pw2 = round((1572/258),2)
  }else if(stratified1$`Mother's BMI`[i]=="BMI 30 and over"){
    pw2 = round((584/96),2)
  }
  pw2=c(print(pw2))
}

strat2 = cbind(stratified2,pw2)
strat2

sample2=svydesign(id=~1,strata =~`Mother's BMI`, weights=~`pw2`, data=strat2 )

svymean(~`Birth weight`,design=sample2)

svymean(~`Mother's age`,design=sample2)

svymean(~`No. of clinical visits`,design=sample2)

svymean(~`No. of months pregnant`,design=sample2)

svymean(~`Mother's education`,design=sample2)

svymean(~`Child's gender`,design=sample2)

svymean(~`Wealth index quintile`,design=sample2)

svymean(~`Ethnicity`,design=sample2)

svymean(~`Province`,design=sample2)

svymean(~`Sector`,design=sample2)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=sample2)

svymean(~`Mother's height`,design=sample2)

svymean(~`Mother's BMI`,design=sample2)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=sample2)

svytotal(~`Child's gender`, desig=sample2)

#Regression Estimation

stratified2
plot(stratified2$`No. of clinical visits`,stratified2$`Birth weight`)
#plot(stratified2$`Birth weight`,stratified2$`Wealth index quintile`)
plot(stratified2$`Mother's age`,stratified2$`Birth weight`)

plot(stratified2$`No. of months pregnant`,stratified2$`Birth weight`)
SLR2 = lm(`Birth weight`~`No. of months pregnant`,stratified2)
SLR2

mean_BirthWeight=326.4+288.6*mean(data$`No. of months pregnant`)
mean_BirthWeight


#Graphs

svyboxplot(`Mother's BMI`~factor(`Sector`), sample1, all.outliers = TRUE, xlab = "Sectors", ylab = "Mother's BMI", main = "Boxplot of Sectors vs Mother's BMI")

#svyboxplot(`Mother's BMI`~factor(Sector), Strat_design1, all.outliers = TRUE, xlab = "Sectors", ylab = "Mother's BMI", main = "Boxplot of Sectors vs Mother's BMI")

cdf.est=svycdf(~`Mother's BMI`+`Birth weight`+`Mother's age`,strat1)

#cdf.pop=ecdf(data$`Birth weight`)
cdf.samp=ecdf(strat1$`Birth weight`)
plot(cdf.pop,do.points=FALSE)
lines(cdf.samp,do.points=FALSE,lwd=2)


svyplot(strat1$`Birth weight`~strat1$`No. of months pregnant`, design=sample1, style="subsample" ,  main= "Birth weight Vs No. of months pregnant", ylab ="Birth weight",xlab="No. of months pregnant")

svyboxplot(`Birth weight`~factor(`Mother's BMI`), sample1, all.outliers=TRUE, xlab="Mother's BMI", ylab="Birth weight", main="Birth weight vs Mother's BMI")


svyboxplot(`No. of clinical visits`~factor(`Wealth index quintile`), sample1, all.outliers=TRUE, xlab="Wealth index quintile", ylab="No. of clinical visits", main="Boxplot of No. of clinical visits vs Wealth index quintile")


svyboxplot(`No. of clinical visits`~factor(`Mother's education`), sample1, all.outliers=TRUE, xlab="Mother's education", ylab="No. of clinical visits", main="Boxplot of No. of clinical visits VS Mother's education")

