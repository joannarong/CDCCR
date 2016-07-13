data=read.csv("inst/extdata/20160713.csv",header=T)
data
dim(data)
names(data)
#refer the column names by numbers

#take the rows that are not NA for location and gender
#cut = converted the numbers into factors --> converted the numbers to factors of the following variables
library(dplyr)
data %>%
  slice(1:622) ->data2

dim(data2)
names(data2)
str(data2)

locationf=cut(data2[,2],breaks=c(0,1.2,3),labels=c("HALTON","MISSISSAUGA"))
locationf

genderf=cut(data2[,4],breaks=c(0,1.5,3),labels=c("M","F"))
genderf

agef=cut(data2[,3],breaks=c(17,45,65,80,95))
agef

repeatreferral=cut(data2[,7],breaks=c(-1,0.5,2),labels=c("No","Yes"))
repeatreferral

referralfrom=cut(data2[,8],breaks=c(-1,1.5,2.5,3.5,4.5,6),labels=c("primary","self","specialist","hospital","DEC"))
referralfrom

socialissue=cut(data2[,9],breaks=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,1000),
                labels=c("social","community","income","elderly","smoke","drugs","housing","mobility","language","education","none"))
socialissue

profcareplan=cut(data2[,64],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
profcareplan

SelfMGoals=cut(data2[,65],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
SelfMGoals

ExtentSelfM=cut(data2[,66],breaks=c(-1,0.5,1.5,2.5,3.5,1000),labels=c("None","Some","Most","All","N/A"))
ExtentSelfM

Pdischwithtrans=cut(data2[,67],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
Pdischwithtrans

DischSt=cut(data2[,69],breaks=c(0,1.5,2.5,3.5),labels=c("withdrawal","discharged","death"))
DischSt

Endorefer=cut(data2[,87],breaks=c(-1,0.5,2),labels=c("no","yes"))
Endorefer

#extract the dates and truned them into Rdate
#obtain different duration between admit date and discharge date
#install.packages("lubridate")
library(lubridate)
referdate=dmy(data2[,6])
referdate

admitdate=dmy(data2[,34])
admitdate

dischargedate=dmy(data2[,68])
dischargedate 


difftime(admitdate,referdate,units="weeks")
difftime(dischargedate,admitdate,units="week")->dis

#dis - data2[,73] #column 73 is numbers of weeeks between admitdate and discharge date as calculated in spreadsheet
#summary(as.numeric(dis) - data2[,73])  #checking if the calculation is consisting
#column Bu- Time in CCDC doesn't appear in the final spread sheet

#co-mo is from column 14-23
#chronic comlication is from column 26-35

#made the function for "T=pts has both morbidity and F=pts doesn't have morbidity"
data2[1,14:23]->pt1co
pt1co
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
extractmorbidity(pt1co,23) #testing pt1 - work!

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
extractco(pt1co) #testing on pt1 -works and it give all the names 

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


#testing for pt1-3 on the chronic functions - works!
data2[1,24:33]->pt1
data2[2,24:33]->pt2
data2[3,24:33]->pt3
extractchronic(pt1)
extractchronic(pt2)
extractchronic(pt3)


data2[1,74:78]->pt2
data2[2,74:78]->pt3
data2[3,74:78]->pt4
data2[9,74:78]->pt10
data2[609,74:78]->pt610
data2[614,74:78]->pt615
extractMH(pt2)
extractMH(pt3)
extractMH(pt4)
extractMH(pt10)
extractMH(pt610)
extractMH(pt615)

    
como=t(apply(data2[,14:23],1,extractco))
chronic=t(apply(data2[,24:33],1,extractchronic))
MH=t(apply(data2[,74:78],1,extractMH))
como=as.data.frame(como)
chronic=as.data.frame(chronic)
MH=as.data.frame(MH)

str(como)
str(chronic)
str(MH)

#make a new dataframe (note that chronic and como factors are displaced one after the other)
dataframe=data.frame(locationf,age=data2[,3],genderf,agef,distance=data2[,5],referdate,repeatreferral,referralfrom,socialissue,
                     DDSa=as.numeric(as.character(data2[,10])),DDSb=as.numeric(as.character(data2[,11])),DDSc=as.numeric(as.character(data2[,12])),
                     DDSd=as.numeric(as.character(data2[,13])),
                     como,chronic,admitdate,data2[,35:39],admit.waist=as.numeric(as.character(data2[,40])),data2[,41:49],
                     DCweight=as.numeric(as.character(data2[,50])),
                     data2[,51:63],profcareplan,
                     SelfMGoals,ExtentSelfM,Pdischwithtrans,dischargedate,DischSt,data2[,70:73],MH,data2[,79:86],Endorefer)
head(dataframe)
dim(dataframe)

#column 59-60: treat 999 N/A 



