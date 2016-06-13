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
extractmorbidity(pt1co,23) #testing pt1 - work!

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
extractco(pt1co) #testing on pt1 -works and it give all the names 

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

extractMH=function(x)
{
  thenames=c("depress","OCD","schizophrenia","alcoholism","bipolar","anxiety","develop","delusional",
             "psychosis","mood","drug","adjustment","alzheimers","cognitive","personality","klinfelter",
             "PTSD","agaraphobia","binge","panic","cope","socialanx","manic","hoarding","eating","maladaptive",
             "imsomnia","clusterc","schizo","ADD","generalizeanx","addiction")
  M=extractmorbidity(x,32)
  names(M)=thenames
  return(M)
}






#' read data in from csv
#' 
#' a paragraph with more details about function
#' 
#' @param file.name name of imported file
#' 
#' @return dataframe containing all variables as appropriatly dates, factor and logicals
#' @export 
#' @import dplyr
#' @import lubridate
Readdata=function(file.name)
  {
  data=read.csv("inst/extdata/finaldata.csv",header=T)
  #refer the column names by numbers
  
  #take the rows that are not NA for location and gender
  #cut = converted the numbers into factors --> converted the numbers to factors of the following variables
  #library(dplyr)
  data %>%
    dplyr::slice(1:622) ->data2
  
  
  locationf=cut(data2[,2],breaks=c(0,1.2,3),labels=c("HALTON","MISSISSAUGA"))
  
  
  genderf=cut(data2[,4],breaks=c(0,1.5,3),labels=c("M","F"))
  
  
  repeatreferral=cut(data2[,7],breaks=c(-1,0.5,2),labels=c("No","Yes"))
  
  
  referralfrom=cut(data2[,8],breaks=c(-1,1.5,2.5,3.5,4.5,6),labels=c("primary","self","specialist","hospital","DEC"))
  
  
  socialissue=cut(data2[,9],breaks=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,1000),
                  labels=c("social","community","income","elderly","smoke","drugs","housing","mobility","language","education","none"))
  
  
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
  
  
  
  #testing for pt1-3 on the chronic functions - works!
  
  
  como=t(apply(data2[,14:23],1,extractco))
  chronic=t(apply(data2[,24:33],1,extractchronic))
  MH=t(apply(data2[,74:78],1,extractMH))
  como=as.data.frame(como)
  chronic=as.data.frame(chronic)
  MH=as.data.frame(MH)
  
  
  #make a new dataframe (note that chronic and como factors are displaced one after the other, and chronics factors start from "Cother")
  dataframe=data.frame(locationf,age=data2[,3],genderf,distance=data2[,5],referdate,repeatreferral,referralfrom,socialissue,
                       data2[,10:13],como,chronic,admitdate,data2[,35:49],DCweight=as.numeric(data2[,50]),data2[,51:63],profcareplan,
                       SelfMGoals,ExtentSelfM,Pdischwithtrans,dischargedate,DischSt,data2[,70:73],MH,data2[,79:86],Endorefer)
  
  
  #column 59-60: treat 999 N/A 
  return(dataframe)
}



