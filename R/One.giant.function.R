#making a griant function to read in the data
library(dplyr)
library(lubridate)

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


extractchronic=function(x)
{
  thenames=c("COther","CNeuro","CRetino","CBlind","CCardio","CWounds",
             "Amput","Skin","Lipo","Hypo",
             "Diabetic","Foot","Stiff","Hear","Frac","Nephro")
  M=extractmorbidity(x,16)
  names(M)=thenames
  return(M)
}


Readdata=function(file.name){
  data=read.csv(file.name,header=T)
  #refer the column names by numbers
  
  #take the rows that are not NA for location and gender
  #cut = converted the numbers into factors --> converted the numbers to factors of the following variables
  
  data %>%
    slice(1:622) ->data2
  
  locationf=cut(data2[,4],breaks=c(0,1.2,3),labels=c("HALTON","MISSISSAUGA"))
  genderf=cut(data2[,6],breaks=c(0,1.5,3),labels=c("M","F"))
  repeatreferral=cut(data2[,9],breaks=c(-1,0.5,2),labels=c("No","Yes"))
  referralfrom=cut(data2[,10],breaks=c(-1,1.5,2.5,3.5,4.5,6),labels=c("primary","self","specialist","hospital","DEC"))
  socialissue=cut(data2[,11],breaks=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,1000),
                  labels=c("social","community","income","elderly","smoke","drugs","housing",
                           "mobility","language","education","none"))
  profcareplan=cut(data2[,65],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  SelfMGoals=cut(data2[,66],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  ExtentSelfM=cut(data2[,67],breaks=c(-1,0.5,1.5,2.5,3.5,1000),labels=c("None","Some","Most","All","N/A"))
  Pdischwithtrans=cut(data2[,68],breaks=c(-1,0.5,2,1000),labels=c("No","Yes","N/A"))
  DischSt=cut(data2[,70],breaks=c(0,1.5,2.5,3.5),labels=c("withdrawal","discharged","death"))
  
  #extract the dates and truned them into Rdate
  #obtain different duration between admit date and discharge date
  referdate=dmy(data2[,7])
  admitdate=dmy(data2[,35])
  dischargedate=dmy(data2[,69])
 
  #pt 179 is missing dischargdate - need to complete later
  
  #difftime(admitdate,referdate,units="weeks")
  #difftime(dischargedate,admitdate,units="week")->dis
  
  #dis - data2[,73] #column 73 is numbers of weeeks between admitdate and discharge date as calculated in spreadsheet
  #summary(as.numeric(dis) - data2[,73])  #checking if the calculation is consisting
  
  #co-mo is from column 16-25
  #chronic comlication is from column 26-35
  
  como=t(apply(data2[,15:24],1,extractco))
  chronic=t(apply(data2[,25:34],1,extractchronic))
  como=as.data.frame(como)
  chronic=as.data.frame(chronic)
  
  #make a new dataframe (note that chronic and como factors are displaced one after the other, and chronics factors start from "Cother")
  dataframe=data.frame(locationf,age=data2[,4],genderf,distance=data2[,6],referdate,repeatreferral,referralfrom,socialissue,
                       data2[,11:14],como,chronic,admitdate,data2[,36:50],DCweight=as.numeric(data2[,51]),data2[,52:64],profcareplan,
                       SelfMGoals,ExtentSelfM,Pdischwithtrans,dischargedate,DischSt,data2[,72:80])

  
  return(dataframe)
}




f="inst/extdata/rawdata1.csv"
D=Readdata(f)
D





