attach(Dataset_01)
data=Dataset_01

#----------------------Population Statistics-----------------------------------#

mean(data$`Birth weight`,na.rm=TRUE)
N = nrow(data)
sqrt(var(data$`Birth weight`,na.rm=TRUE)/N)

mean(data$`Mother's age`,na.rm=TRUE)
sqrt(var(data$`Mother's age`,na.rm=TRUE)/N)

mean(data$`No. of clinical visits`,na.rm=TRUE)
sqrt(var(data$`No. of clinical visits`,na.rm=TRUE)/N)

mean(data$`No. of months pregnant`,na.rm=TRUE)
sqrt(var(data$`No. of months pregnant`,na.rm=TRUE)/N)

data.frame((table(data$`Mother's education`))/length(data$`Mother's education`))
#sqrt(var(`Mother's education`,na.rm=TRUE)/N)


data.frame((table(data$`Child's gender`))/length(data$`Child's gender`))

data.frame(table(data$`Wealth index quintile`)/length(data$`Wealth index quintile`))

data.frame(table(data$Ethnicity)/length(data$Ethnicity))

data.frame(table(data$Province)/length(data$Province))

data.frame(table(data$Sector)/length(data$Sector))

data.frame(table(data$`Low birth weight - 1 if it is a low birth weight, 0 otherwise`)/length(data$`Low birth weight - 1 if it is a low birth weight, 0 otherwise`))

data.frame(table(data$`Mother's height`)/length(data$`Mother's height`))

data.frame(table(data$`Mother's BMI`)/length(data$`Mother's BMI`))

table(`Low birth weight - 1 if it is a low birth weight, 0 otherwise`)

table(`Child's gender`)


library(sampler)

#------------------------------------SRS---------------------------------------#

#SAMPLE1

n=rsampcalc(nrow(data),e=3,ci=95)
set.seed(013001)
SRS_Sample1 = rsamp(df = data,n, rep = FALSE)
SRS_Sample1

library(survey)

SRS1=svydesign(id=~1,data=SRS_Sample1)

bw1=svymean(~`Birth weight`,design=SRS1,na.rm=TRUE)

svymean(~`Mother's age`,design=SRS1,na.rm=TRUE)

svymean(~`No. of clinical visits`,design=SRS1,na.rm=TRUE)

svymean(~`No. of months pregnant`,design=SRS1,na.rm=TRUE)

svymean(~`Mother's education`,design=SRS1,na.rm=TRUE)

svymean(~`Child's gender`,design=SRS1,na.rm=TRUE)

svymean(~`Wealth index quintile`,design=SRS1,na.rm=TRUE)

svymean(~`Ethnicity`,design=SRS1,na.rm=TRUE)

svymean(~`Province`,design=SRS1,na.rm=TRUE)

svymean(~`Sector`,design=SRS1,na.rm=TRUE)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=SRS1,na.rm=TRUE)

svymean(~`Mother's height`,design=SRS1,na.rm=TRUE)

svymean(~`Mother's BMI`,design=SRS1,na.rm=TRUE)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=SRS1)

svytotal(~`Child's gender`, design=SRS1)


#Regression Estimation

SRS_Sample1
plot(SRS_Sample1$`No. of clinical visits`,SRS_Sample1$`Birth weight`)
#plot(SRS_Sample$`Birth weight`,SRS_Sample$`Wealth index quintile`)
plot(SRS_Sample1$`Mother's age`,SRS_Sample1$`Birth weight`)
plot(SRS_Sample1$`No. of months pregnant`,SRS_Sample1$`Birth weight`)

SLR1 = lm(`Birth weight`~`No. of months pregnant`,SRS_Sample1)
SLR1

mean_BirthWeight=-76.78+331.84*mean(data$`No. of months pregnant`)
mean_BirthWeight


#SAMPLE2

n=rsampcalc(nrow(data),e=3,ci=95)
set.seed(023001)
SRS_Sample2 = rsamp(df = data,n, rep = FALSE)
SRS_Sample2

SRS2=svydesign(id=~1,data=SRS_Sample2)

svymean(~`Birth weight`,design=SRS2,na.rm=TRUE)

svymean(~`Mother's age`,design=SRS2,na.rm=TRUE)

svymean(~`No. of clinical visits`,design=SRS2,na.rm=TRUE)

svymean(~`No. of months pregnant`,design=SRS2,na.rm=TRUE)

svymean(~`Mother's education`,design=SRS2,na.rm=TRUE)

svymean(~`Child's gender`,design=SRS2,na.rm=TRUE)

svymean(~`Wealth index quintile`,design=SRS2,na.rm=TRUE)

svymean(~`Ethnicity`,design=SRS2,na.rm=TRUE)

svymean(~`Province`,design=SRS2,na.rm=TRUE)

svymean(~`Sector`,design=SRS2,na.rm=TRUE)

svymean(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`,design=SRS2,na.rm=TRUE)

svymean(~`Mother's height`,design=SRS2,na.rm=TRUE)

svymean(~`Mother's BMI`,design=SRS2,na.rm=TRUE)

svytotal(~`Low birth weight - 1 if it is a low birth weight, 0 otherwise`, design=SRS2)

svytotal(~`Child's gender`, desig=SRS2)

#Regression Estimation

SRS_Sample2
plot(SRS_Sample1$`No. of clinical visits`,SRS_Sample1$`Birth weight`)
#plot(SRS_Sample$`Birth weight`,SRS_Sample$`Wealth index quintile`)
plot(SRS_Sample2$`Mother's age`,SRS_Sample2$`Birth weight`)
plot(SRS_Sample2$`No. of months pregnant`,SRS_Sample2$`Birth weight`)

SLR2 = lm(`Birth weight`~`No. of months pregnant`,SRS_Sample2)
SLR2

mean_BirthWeight=221.3+301.4*mean(data$`No. of months pregnant`)
mean_BirthWeight


barplot(SRS1)

library(rmeta)

forestplot(bw1,coef(bw1),coef(bw1)-sqrt(2)*SE(bw1),coef(bw1)+sqrt(2)*SE(bw1),align=c("1","1"),zero=500)


is.factor(Dataset_01)

