data=read.csv("inst/extdata/rawdata1.csv",header=T)
data
dim(data)
names(data)
#refer the column names by numbers

#take the rows that are not NA for location and gender
#cut = converted the numbers into factors --> converted the numbers to factors of the following variables
library(dplyr)
data %>%
  slice(1:622) ->data2

locationf=cut(data2[,4],breaks=c(0,1.2,3),labels=c("HALTON","MISSISSAUGA"))
locationf

genderf=cut(data2[,6],breaks=c(0,1.5,3),labels=c("M","F"))
genderf

repeatreferral=cut(data2[,9],breaks=c(-1,0.5,2),labels=c("No","Yes"))
repeatreferral

referralfrom=cut(data2[,10],breaks=c(-1,1.5,2.5,3.5,4.5,6),labels=c("primary","self","specialist","hospital","DEC"))
referralfrom

socialissue=cut(data2[,11],breaks=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,1000),
                labels=c("social","community","income","elderly","smoke","drugs","housing","mobility","language","education","none"))
socialissue

profcareplan=cut(data2[,66],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
profcareplan

SelfMGoals=cut(data2[,67],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
SelfMGoals

ExtentSelfM=cut(data2[,68],breaks=c(-1,0.5,1.5,2.5,3.5,1000),labels=c("None","Some","Most","All","N/A"))
ExtentSelfM

Pdischwithtrans=cut(data2[,69],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
Pdischwithtrans

DischSt=cut(data2[,71],breaks=c(0,1.5,2.5,3.5),labels=c("withdrawal","discharged","death"))
DischSt

#extract the dates and truned them into Rdate
#obtain different duration between admit date and discharge date
install.packages("lubridate")
library(lubridate)
referdate=dmy(data2[,8])
referdate

admitdate=dmy(data2[,36])
admitdate

dischargedate=dmy(data2[,70])
dischargedate 
#pt 179 is missing dischargdate - need to complete later

difftime(admitdate,referdate,units="weeks")
difftime(dischargedate,admitdate,units="week")->dis

dis - data2[,73] #column 73 is numbers of weeeks between admitdate and discharge date as calculated in spreadsheet
summary(as.numeric(dis) - data2[,73])  #checking if the calculation is consisting

#co-mo is from column 16-25
#chronic comlication is from column 26-35

#made the function for "T=pts has both morbidity and F=pts doesn't have morbidity"
data2[1,16:25]->pt1
pt1
extractmorbidity=function(x,possible) #possible=#of possible values for 16 or 23 values for chronic / co-mobidity
{
  if(x[1]==999)
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
extractmorbidity(pt1,23) #testing pt1 - work!

#made fuction that label the specific values of each co-mobidity
extractco=function(x)
{
  thenames=c("Other","Eat","Hypert","Cardio","Kidney","Retino","Wounds","Neuro","Liver",
             "Vascular","Obesity","Cancer","Lipidd",
             "Thyroid","Dementia","Pulmon","sleep",
             "HIV","Pancreas","Celiac","Genetic","Deafblind","Mental") 
  M=extractmorbidity(x,23)
  names(M)=thenames
  return(M)
  
}
extractco(pt1) #testing on pt1 -works and it give all the names 

#made functionthat label the specific values for each chronic-morbidity
extractchronic=function(x)
{
  thenames=c("COther","CNeuro","CRetino","CBlind","CCardio","CWounds",
             "Amput","Skin","Lipo","Hypo",
             "Diabetic","Foot","Stiff","Hear","Frac","Nephro")
  M=extractmorbidity(x,16)
  names(M)=thenames
  return(M)
}

#testing for pt1-3 on the chronic functions - works!
data2[1,26:35]->pt1
data2[2,26:35]->pt2
data2[3,26:35]->pt3
extractchronic(pt1)
extractchronic(pt2)
extractchronic(pt3)


como=t(apply(data2[,16:25],1,extractco))
chronic=t(apply(data2[,26:35],1,extractchronic))
como=as.data.frame(como)
chronic=as.data.frame(chronic)
str(como)
str(chronic)

#make a new dataframe (note that chronic and como factors are displaced one after the other, and chronics factors start from "Cother")
dataframe=data.frame(locationf,age=data2[,5],genderf,distance=data2[,7],referdate,repeatreferral,referralfrom,socialissue,
                     data2[,12:15],como,chronic,admitdate,data2[,37:50],DCweight=as.numeric(data2[,51]),data2[,52:65],profcareplan,
                     SelfMGoals,ExtentSelfM,Pdischwithtrans,dischargedate,DischSt,data2[,72:81])
head(dataframe)
str(dataframe)
table(data2[,51])
data2[,51]


