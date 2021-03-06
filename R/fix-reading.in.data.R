#'Turn list of co/chronic mobidity and mental health into "ture" and "false"
#'
#' @param x the levels for patients in the original data frame to be turned into "ture" and "false"
#' @param possible numbers of possible levels for co/chronic mobidity and mental health
#' @return vectors of "ture" and "false" according to the levels of co/chronic mobidity and mental health
extractmorbidity=function(x,possible) #possible=#of possible values for 16 or 23 values for chronic / co-mobidity
{
  if(x[1]==999||is.na(x[1]))
  {
    return(rep(F,possible))
  }else{
    has=rep(F,possible)
    for(i in x){
      has[i]=T
    }
    return(has)
  }
}

hasrepeat=function(x,possible)
{
  tab=table(x)
  m=max(tab)
  w=names(which.max(tab))
  ifelse(m>1,w,NA)
}

#x=c(2,3,2,3,4)
#table(x)
#tab=table(t(data2[2,14:23]))
#tab["23"]

#'built a function that will count numbers of 23(mental health in comorbidity) in one patient
count23=function(x)
{
  tab=table(t(x))
  ans=ifelse(is.na(tab["23"]),0,tab["23"])
}

#count.discharged.status=function(x)
#{
  #tab=table(t(x))
  #ans=ifelse(is.na(tab["2"]),0,tab["2"])
#}

#'built a function that count the total numbers of actual mental health issue for each patient 
countmh=function(x)
{
  return(sum(table(t(x))))
}



#made fuction that label the specific values of each co-mobidity
extractco=function(x)
{
  thenames=c("Other","Eating Disorders","Hypertension","Cardiovascular disease","Chronic kidney disease",
             "Retinopathy or Other eye disease"," Non-healing wounds (greater than 3 months)","Neuropathy","Liver disease (fatty liver)",
             "Peripheral vascular disease","Obesity (BMI > 30)","Current malignancy/cancer treatment","Hyperlipidemia",
             "Thyroid disease (other endocrinopathies POCT, Cushings)","Dementia","Pulmonary disease (COPD, Asthma)","Obstructive sleep apnea",
             "HIV/AIDS","Pancreas Diseases","Celiac Disease","Genetic Syndromes","Deafness and/or Blindness","Mental health") 
  M=extractmorbidity(x,23)
  names(M)=thenames
  return(M)
  
}

#made functionthat label the specific values for each chronic-morbidity
extractchronic=function(x)
{
  thenames=c("Other(Chronic)","Neuropathy","Retinopathy","Blindness","Cardiovascular disease(Chronic)","Wounds (non-healing)",
             "Amputation","Skin conditions (cutaneous manifestations)","Lipohypertrophy","Hypoglycemia unawareness",
             "Diabetic myonecrosis","Foot problems (Charcot’s)","Stiff man’s syndrome","Hearing impairment","Fractures","Nephropathy")
  M=extractmorbidity(x,16)
  names(M)=thenames
  return(M)
}

extractMH=function(x)
{
  thenames=c("Depressive Disorder","Obsessive-compulsive and related disorders","Schizophrenia spectrum and other psychotic disorders",
             "Substance-related and addictive disorders","Bipolar Disorders","Anxiety disorders",
             "Neurodevelopmental disorder","Trauma and Stress-related disorders","Personality Disorders","Feeding and Eating Disorders",
             "Sleep-wake disorders","Type Unspecified")
  
  possible=12
  if(is.na(x[1]))
  {
    y=rep(F,possible)
  }else{
    has=rep(F,possible)
    for(i in x){
      if(!is.na(i)){
        has[i]=T
      }
    }
    y=has
  }
  names(y)=thenames
  return(y)
}



#'function that capture non numeric values
TidyDDScode=function(x){
  y=as.character(x)
  z=as.numeric(y)
  code=ifelse(is.na(z),y,ifelse(z<20,"numeric",y))
  return(code)
}

#'function that capture numeric values that are below 20
TidyDDSnumbers=function(x){
  y=as.character(x)
  z=as.numeric(y)
  code=ifelse(is.na(z),NA,ifelse(z<20,z,NA))
  return(code)
}

#'testing the two functions above - work! 
z=c("1","2","11",".","999",NA,"")
z

data2[,10]->x
x
data2[,11]->e
e

TidyDDScode(z)
TidyDDSnumbers(z)
table(TidyDDScode(e))
table(TidyDDScode(x))
cbind(as.character(x),TidyDDScode(x),TidyDDSnumbers(x))
cbind(as.character(e),TidyDDScode(e),TidyDDSnumbers(e))
cbind(as.character(z),TidyDDScode(z),TidyDDSnumbers(z))




# read data in from csv
# 
# a paragraph with more details about function
# 
# @param file.name name of imported file
#
# @return dataframe containing all variables as appropriatly dates, factor and logicals
# @export 
# @import dplyr
# @import lubridate
library(dplyr)
Readdata=function(file.name)
{
  data=read.csv("inst/extdata/20160715.csv",header=T)
  #refer the column names by numbers
  
  #take the rows that are not NA for location and gender
  #cut = converted the numbers into factors --> converted the numbers to factors of the following variables
  #library(dplyr)
  data %>%
    dplyr::slice(1:622) ->data2
  
  
  locationf=cut(data2[,2],breaks=c(0,1.2,3),labels=c("HALTON","MISSISSAUGA"))
  
  
  genderf=cut(data2[,4],breaks=c(0,1.5,3),labels=c("M","F"))
  
  agef=cut(data2[,3],breaks=c(17,45,65,80,95))
  
  repeatreferral=cut(data2[,7],breaks=c(-1,0.5,2),labels=c("No","Yes"))
  
  
  referralfrom=cut(data2[,8],breaks=c(-1,1.5,2.5,3.5,4.5,6),labels=c("primary","self","specialist","hospital","DEC"))
  
  
  socialissue=cut(data2[,9],breaks=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,1000),
                  labels=c("social","community","income","elderly","smoke","drugs","housing","mobility","language","education","none"))
  #consider changing the short names to the actual names  
  
  profcareplan=cut(data2[,64],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  
  
  SelfMGoals=cut(data2[,65],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  
  
  ExtentSelfM=cut(data2[,66],breaks=c(-1,0.5,1.5,2.5,3.5,1000),labels=c("None","Some","Most","All","N/A"))
  
  
  Pdischwithtrans=cut(data2[,67],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  
  
  DischSt=cut(data2[,69],breaks=c(0,1.5,2.5,3.5),labels=c("withdrawal","discharged","death"))
  
  
  Endorefer=cut(data2[,87],breaks=c(-1,0.5,2),labels=c("no","yes"))
  
  
  #extract the dates and truned them into Rdate
  #obtain different duration between admit date and discharge date
  
  #library(lubridate)
  referdate=lubridate::dmy(data2[,6])
  
  
  admitdate=lubridate::dmy(data2[,34])
  
  
  dischargedate=lubridate::dmy(data2[,68])
  
  
  
  #dis - data2[,73] #column 73 is numbers of weeeks between admitdate and discharge date as calculated in spreadsheet
  #summary(as.numeric(dis) - data2[,73])  #checking if the calculation is consisting
  #column Bu- Time in CCDC doesn't appear in the final spread sheet
  
  #co-mo is from column 14-23
  #chronic comlication is from column 24-33
  
  #made the function for "T=pts has both morbidity and F=pts doesn't have morbidity"
  
  
  como=t(apply(data2[,14:23],1,extractco))
  comorep=apply(data2[,14:23],1,hasrepeat)
  comorep
  which(comorep=="3")
  comomhrep=apply(data2[,14:23],1,count23)
  comomhrep #'counted numbers of "23" for each patient among the comorbidity columns --> fixed: no patients has repeated 23 any more!
  
  which(comomhrep>1) #' no patients are found that has more than one 23
  actualmhrep=apply(data2[,74:78],1,countmh)
  actualmhrep #'counted numbers of actual mental health issue for each patient among the mental health sections
  table(comomhrep>=1,actualmhrep>=1)
  which(comomhrep>=1 & actualmhrep==0) # not an issue any more--> pt354 doesn't have any mental healthy diagnosis listed, but has 23 coded in the comorbidity section 
  
  table(comorep) #show which coded comorbidity has repeates --> only code "1" is repeated, but agree on ignoring it (also row61 has more 22 twice, but it's all good! )
  which(!is.na(comorep)) #shows which patients has more than one unique code
  #only co-morbidity has a lot of repeats 
  
  chronic=t(apply(data2[,24:33],1,extractchronic))
  chronicrep=apply(data2[,24:33],1,hasrepeat)
  chronicrep
  table(chronicrep)
  #no repeats are found in chronic complications
  
  MH=t(apply(data2[,74:78],1,extractMH))
  head(MH)
  MHrep=apply(data2[,74:78],1,hasrepeat)
  MHrep
  table(MHrep)
  #no repeats found for MH diagnosis 
  
  
  #make a new dataframe 
  como=as.data.frame(como)
  chronic=as.data.frame(chronic)
  MH=as.data.frame(MH)
  dataframe=data.frame(locationf,age=data2[,3],genderf,agef,distance=data2[,5],referdate,repeatreferral,referralfrom,socialissue,
                       DDS.Emotional.burden.Enrollment=TidyDDSnumbers(data2[,10]),DDS.Emotional.burden.Enrollment.code=TidyDDScode(data2[,10]),
                       DDS.Physician.related.distress.Enrollment=TidyDDSnumbers(data2[,11]),DDS.Physician.related.distress.Enrollment.code=TidyDDScode(data2[,11]),
                       DDS.Regimen.related.distress.Enrollment=TidyDDSnumbers(data2[,12]),DDS.Regimen.related.distress.Enrollment.code=TidyDDScode(data2[,12]),
                       DDS.Interpersonal.distress.Enrollment=TidyDDSnumbers(data2[,13]),DDS.Interpersonal.distress.Enrollment.code=TidyDDScode(data2[,13]),
                       como,chronic,admitdate,data2[,35:39],admit.waist=as.numeric(as.character(data2[,40])),data2[,41:49],
                       DCweight=as.numeric(as.character(data2[,50])),
                       data2[,51:58],
                       DDS.Emotional.burden.Discharge=TidyDDSnumbers(data2[,59]),DDS.Emotional.burden.Discharge.code=TidyDDScode(data2[,59]),
                       DDS.Physician.related.distress.Discharge=TidyDDSnumbers(data2[,60]),DDS.Physician.related.distress.Discharge.code=TidyDDScode(data2[,60]),
                       DDS.Regimen.related.distress.Discharge=TidyDDSnumbers(data2[,61]),DDS.Regimen.related.distress.Discharge.code=TidyDDScode(data2[,61]),
                       DDS.Interpersonal.distress.Discharge=TidyDDSnumbers(data2[,62]),DDS.Interpersonal.distress.Discharge.code=TidyDDScode(data2[,62]),
                       data2[,63],profcareplan,
                       SelfMGoals,ExtentSelfM,Pdischwithtrans,dischargedate,DischSt,data2[,70:73],MH,data2[,79:86],Endorefer)
  return(dataframe)
}











dataframe=Readdata()
head(dataframe)
#'1a.Numbers of male and female patients from each site:
outofregions <- table(dataframe$locationf,dataframe$genderf)
outofregions
prop.table(outofregions,margin=1)
#'no effect of genders - same percentage of males and females from both regions 

boxplot(dataframe$age~dataframe$locationf)
#'no great difference in midean and spread for both sites

boxplot(dataframe$distance~dataframe$locationf)
#'one outlier on the Halton site (mistake: already been fixed) 

#'1b.numbers of patients with each social issue:
socialissues <- table(dataframe$socialissue)
socialissues
plot(dataframe$socialissue,las=2)
#'compare to the impactful issues you mentioned, the following social issues appears to be more frequent: low income, social support, smoking, elderly 

#'1c.numbers of patients with each co-morbidity 
a=subset(dataframe, select=c("Other","Eating.Disorders","Hypertension","Cardiovascular.disease","Chronic.kidney.disease","Retinopathy.or.Other.eye.disease","Non.healing.wounds..greater.than.3.months.",
                             "Neuropathy","Liver.disease..fatty.liver.","Peripheral.vascular.disease","Obesity..BMI...30.","Current.malignancy.cancer.treatment","Hyperlipidemia","Thyroid.disease..other.endocrinopathies.POCT..Cushings.","Dementia",
                             "Pulmonary.disease..COPD..Asthma.","Obstructive.sleep.apnea","HIV.AIDS","Pancreas.Diseases","Celiac.Disease","Genetic.Syndromes","Deafness.and.or.Blindness","Mental.health"))
a

library(tidyr)
library(dplyr)
a %>% gather(col,v,Other:Mental.health) %>% table()
#'1c.the co-morbidities with the highest frequency:Lipidd


#' 1c.average # of co-morbidities
sumofcomofoeachpt=apply(a,1,sum) #'how many como each pt has
summary(sumofcomofoeachpt) #each pt have avg 3 como

#'1c.numbers of patients with each chronic-morbidity 
b=subset(dataframe, select=c("Other.Chronic.","Neuropathy.1","Retinopathy","Blindness","Cardiovascular.disease.Chronic.","Wounds..non.healing.",
                             "Amputation","Skin.conditions..cutaneous.manifestations.","Lipohypertrophy","Hypoglycemia.unawareness",
                             "Diabetic.myonecrosis","Foot.problems..Charcot.s.","Stiff.man.s.syndrome","Hearing.impairment","Fractures","Nephropathy"))
b
b %>% gather(col,v,Other.Chronic.:Nephropathy) %>% table()

#' 1c.average # of chronic-morbidities
sumofchronicfoeachpt=apply(b,1,sum) #'how many chronicmo each pt has
summary(sumofchronicfoeachpt) #each pt have avg 1 chronic


#'1c.numbers of patients with mental health as co-morbidity:
comomental <- table(dataframe$Mental.health)
comomental
which(dataframe$Mental)

#'numbers of patients with "other" as co-morbidity
conmoother <- table(dataframe$Other)
conmoother
which(dataframe$Other)
#there are lots of duplicates for "other" as these are sepeated issues that don't have their own codes --> but we agree that to ignore it

#'numbers of patients with "eat" as co-morbidity
comoeat <- table(dataframe$Eat)
comoeat
#this is the same total numbers as per excel calculated

chronicother <- table(dataframe$COther)
chronicother

chronicamputation <- table(dataframe$Amput)
chronicamputation
#chronic numebrs are correct, no repeats 

#' 1c.the range of co-morbidities/complication?? -->already did it



#' 1d. How many has mental health as one of the co-morbidities(protions of F and M have metal health issue as co-morbidity) 
comomental <- table(dataframe$Mental.health,dataframe$genderf)
comomental
prop.table(comomental,margin=2)
#'among Male, only 25% have mental health issues, but higher proportion of female than male have mental health issue 

#' logistic data-mental vs. admit A1C
logisticmentaladmitedA1C=glm(Mental~admit.A1C,data=dataframe,family="binomial")
summary(logisticmentaladmitedA1C)
#'not significant (re-do this one with final data)

#' #8.logistic data-mental vs. discharged A1C
logisticmentaldischargedA1C=glm(Mental~D.C.A1C,data=dataframe,family="binomial")
summary(logisticmentaldischargedA1C)
#'almost significant
#'slope is positive = meaning as discharged A1C level increase, the patients are more likely to have mental health issue as one of co-morbidity
#this regression model "true" (2nd one after "false")


#' 2a.comparing admit and discharged clinical matrix (A1C)
aggregate(cbind(admit.A1C,D.C.A1C)~genderf+agef,data=dataframe,mean)
#'average Dischaged A1C level is lower for both male and female! --> good/strong evidence + the effect is also consistant (As the change is about the same for both gender) 
#'(however, the target is 6-7, both male and female patients are above target)

library(dplyr)
library(tidyr)
library(ggplot2)

dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=A1C))+geom_boxplot()+facet_grid(admitdischarge~agef)
#'older pts have lower discharged A1C value + consistant difference between two genders 
#'+ A1C values goes down as age increase 
dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% group_by(agef,genderf) %>% 
  summarise(n=n())
#' resonably large frequency for each age group - good thing

dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  filter(is.na(A1C)) %>% select(c(admitdischarge,A1C)) 
#'found 97 NA(missing values) for A1C in total (2 are from admited A1C)


#'all patients who have missing data D.C.A1C are actually withdrew becuase of death/withdrawal, no missing data are found among those who discharged
c=subset(dataframe, select=c("admit.A1C","D.C.A1C","DischSt"))
c
c %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% filter(is.na(A1C)) 





#'2a.comparing admit and discharged clinical matrix(Lipids level(HDL cholesterol)):
aggregate(cbind(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)~genderf+agef,data=dataframe,mean)
#lower discharged HDL cholesterol level in male, but same for female 

dataframe %>% gather(admitdischarge,LipidHDL,c(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=LipidHDL))+geom_boxplot()+facet_grid(agef~admitdischarge)


#'2a.comparing admit and discharged clinical matrix(Lipids level(HDL cholesterol))
aggregate(cbind(admit.LDL.Cholesterol, D.C.LDL.Cholesterol)~genderf+agef,data=dataframe,mean)
#both male and female have lower discharged LDL cholesterol levle = good thing! 

dataframe %>% gather(admitdischarge,LipidLDL,c(admit.LDL.Cholesterol, D.C.LDL.Cholesterol)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=LipidLDL))+geom_boxplot()+facet_grid(agef~admitdischarge)

#'2a.eGFR:
aggregate(cbind(admit.eGFR, D.C.eGFR)~genderf+agef,data=dataframe,mean)
#both male and female have lower discharged eGFR levle

dataframe %>% gather(admitdischarge,eGFR,c(admit.eGFR, D.C.eGFR)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=eGFR))+geom_boxplot()+facet_grid(agef~admitdischarge)


#'2a.Micro Albumin
aggregate(cbind(admit.Micro.Albumin, D.C.Micro.Albumin)~genderf+agef,data=dataframe,mean)
#increase discharged microalbumin level for male, but decrease discharged microalbumin for female

dataframe %>% gather(admitdischarge,Microalbumin,c(admit.Micro.Albumin, D.C.Micro.Albumin)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=Microalbumin))+geom_boxplot()+facet_grid(agef~admitdischarge)



#'2c.Change in QoL index (emotional burden)
dataframe %>% gather(admitdischarge,emotionalBurden,c(DDS.Emotional.burden.Enrollment,DDS.Emotional.burden.Discharge)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=emotionalBurden))+geom_boxplot()+facet_grid(agef~admitdischarge)

dataframe %>% gather(admitdischarge,emotionalBurden,c(DDS.Emotional.burden.Enrollment,DDS.Emotional.burden.Discharge)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% group_by(agef,genderf) %>% 
  summarise(n=n())
#'lower emotional burden discharge scores for both male and female 

#'scatterplot of emotional burden by age - numeric and classify by admit/discharged by genders
dataframe %>% gather(admitdischarge,emotionalBurden,c(DDS.Emotional.burden.Enrollment,DDS.Emotional.burden.Discharge)) %>% 
  ggplot(aes(x=age,y=emotionalBurden))+geom_point()+facet_grid(genderf~admitdischarge)


#'2c.Change in QoL index (physician related distress)
dataframe %>% gather(admitdischarge,physicianRelated,c(DDS.Physician.related.distress.Enrollment,DDS.Physician.related.distress.Discharge)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=physicianRelated))+geom_boxplot()+facet_grid(agef~admitdischarge)


#'2c.Change in QoL index (Regimen related distress)
dataframe %>% gather(admitdischarge,RegimenRelated,c(DDS.Regimen.related.distress.Enrollment,DDS.Regimen.related.distress.Discharge)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=RegimenRelated))+geom_boxplot()+facet_grid(agef~admitdischarge)


#'2c.Change in QoL index (Interpersonal distress)
dataframe %>% gather(admitdischarge,interpersonalDistress,c(DDS.Interpersonal.distress.Enrollment,DDS.Interpersonal.distress.Discharge)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=interpersonalDistress))+geom_boxplot()+facet_grid(agef~admitdischarge)



#'2d. Change in clinical metrics(A1C) for subgroups: mental health patients
dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  ggplot(aes(x=genderf,y=A1C))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)
#'Discharged A1C levels are substantially lower than admit A1C level for both group of patients with/without mental health diagnosis 
#'there is a treatment effct due to lower dischagred A1C level, however, gender and mental health does not have any effect on the scores


#'2d. Change in clinical metrics(Lipids level(HDL cholesterol)) for subgroups: mental health patients
dataframe$Mental.health
aggregate(cbind(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)~Mental.health,data=dataframe,mean)

#'with outliers
dataframe %>% gather(admitdischarge,HDL.cholestrol,c(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)) %>% 
  ggplot(aes(x=genderf,y=HDL.cholestrol))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)

#'without outliers --> make it easier to compare the median
dataframe %>% gather(admitdischarge,HDL.cholestrol,c(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)) %>%
  filter(HDL.cholestrol<5) %>% 
  ggplot(aes(x=genderf,y=HDL.cholestrol))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)
#'changes in HLD.cholesterol level seems to be quite similar for both group of patients, which might suggest that treatment have no significant effect on patient's HDL.cholesterol level


#'2d. Change in clinical metrics(Lipids level(LDL cholesterol)) for subgroups: mental health patients
dataframe %>% gather(admitdischarge,LDL.cholestrol,c(admit.LDL.Cholesterol, D.C.LDL.Cholesterol)) %>% 
  ggplot(aes(x=genderf,y=LDL.cholestrol))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)
#'mental health doesn't seem to have an effect in the changes of LDL cholesterol level: this is because the admit LDL cholesterol for borth male and female without mental health diagnosis decrease about the same ratio as those with mental health diagnosis
#'however, treatment seem to have bigger effect for male pt than female pt as male pt have substantial lower discharged LDL lipid level than female.

#'2d. Change in clinical metrics(eGFR) for subgroups: mental health patients
dataframe$Mental.health
aggregate(cbind(admit.eGFR, D.C.eGFR)~Mental.health,data=dataframe,mean)

dataframe %>% gather(admitdischarge,eGFR,c(admit.eGFR, D.C.eGFR)) %>% 
  ggplot(aes(x=genderf,y=eGFR))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)
#'mental health might be related to higher eGFR score: both male and female with mental health diagnosis have higher eFGR scores than those who are not diagnosed with mental health.
#'however, the treatment does not seem to improve the score as we see that discharged eGFR remain about the same as admited eGFR for both genders with/without mental health diagnosis.

dataframe %>% mutate(diff=D.C.eGFR-admit.eGFR) %>% lm(diff~Mental.health*genderf,data=.) %>% anova()

dataframe %>% mutate(diff=D.C.eGFR-admit.eGFR) %>% group_by(admitdischarge,Mental.health,genderf) %>% summarise(m=mean(eGFR,na.rm=T))
#'testing: comfirmed that pt with/without mental health have significant effect on eGFR, however, there are no significant interaction between anything and no other significant main effects

dataframe %>% mutate(diff=D.C.eGFR-admit.eGFR) %>%
  group_by(Mental.health,genderf) %>% summarise(m=mean(diff,na.rm=T))
#'this shows that there is slight interaction between mental health and gender (ie., P value is 0.8 in the test!) 
#'for example: comparing line 1 and 3; line 2 and 4- question: since the change for male is bigger than female, are the changes clinically meaningful? 


#'2d. Change in clinical metrics(micro albumin) for subgroups: mental health patients
dataframe %>% gather(admitdischarge,micro.albumin,c(admit.Micro.Albumin, D.C.Micro.Albumin)) %>% 
  ggplot(aes(x=genderf,y=micro.albumin))+geom_boxplot()+facet_grid(Mental.health~admitdischarge)
#'many outliers shown
#'need help with interpretation







#'2d(pending). Change in clinical metrics for subgroups: patients with wounds 
dataframe$CWounds
aggregate(cbind(admit.HDL.Cholesterol, D.C.HDL.Cholesterol)~CWounds,data=dataframe,mean)
#'patients with chronic wounds have slightly lower HDL.cholesterol level as admission

dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  ggplot(aes(x=genderf,y=A1C))+geom_boxplot()+facet_grid(CWounds~admitdischarge)
dataframe %>% gather(admitdischarge,A1C,c(admit.A1C,D.C.A1C)) %>% 
  group_by(CWounds,genderf) %>% 
  summarise(n=n()) 

#'2e.Does # social issues and co-morbidities impact health outcomes, to what extent (i.e.percentage of change is less or more)
#'regression? x=social issues and co-morbidities, Y=health outcome
#'does less social issues and co-morbidity result in better health outcome and vise versa? 
#'or numbers of social issue (in range) vs.admit/discharged health measurment? 
  
#'refer: 1b.numbers of patients with each social issue:
socialissues <- table(dataframe$socialissue)
socialissues
plot(dataframe$socialissue,las=2)

#'refer: 1c.numbers of patients with each co-morbidity 
a=subset(dataframe, select=c("Other","Eating.Disorders","Hypertension","Cardiovascular.disease","Chronic.kidney.disease","Retinopathy.or.Other.eye.disease","Non.healing.wounds..greater.than.3.months.",
                             "Neuropathy","Liver.disease..fatty.liver.","Peripheral.vascular.disease","Obesity..BMI...30.","Current.malignancy.cancer.treatment","Hyperlipidemia","Thyroid.disease..other.endocrinopathies.POCT..Cushings.","Dementia",
                             "Pulmonary.disease..COPD..Asthma.","Obstructive.sleep.apnea","HIV.AIDS","Pancreas.Diseases","Celiac.Disease","Genetic.Syndromes","Deafness.and.or.Blindness","Mental.health"))
a

library(tidyr)
library(dplyr)
a %>% gather(col,v,Other:Mental.health) %>% table()

dataframe %>% gather(admitdischarge,micro.albumin,c(admit.Micro.Albumin, D.C.Micro.Albumin)) %>% 
  ggplot(aes(x=,y=micro.albumin))+geom_boxplot()




#'3a.Change in Hospital and ER usage(compare by avg number of ER and hospital admits per month)
dataframe %>% gather(avg.numbers.of.visit,visit,c(X..of.Diabetes.Related.ER.Visits.in.year.prior.to.CCDC.registration,D.C.Total...of.Diabetes.Related.ER.Visits)) %>% 
  mutate(agef=cut(age,breaks=c(17,44,64,79,95),labels=c("18-44","45-64","65-79","80+"))) %>% 
  ggplot(aes(x=genderf,y=A1C))+geom_boxplot()+facet_grid(admitdischarge~agef)









