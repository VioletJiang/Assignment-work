#Assignment 1
library(lubridate)
library(timeDate)
get.hours<- function(iso, peak.type, period){#Input variables should be character type, i.e. with double quotes ""
  library(lubridate)
  library(timeDate)
  Sys.setlocale("LC_TIME", "English")
  area<-ifelse(iso %in% c("WECC","CAISO"),"Western","Eastern")
  month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  year<-substr(period,1,4)
  get.wl.fl<-function(start.date,end.date,year,area){#Distinguish between working days and rest days in start and end days
    freeday<-character()
    workday<-character()
    end.date=as.Date(end.date)
    start.date=as.Date(start.date)
    days=as.numeric(end.date-start.date)
    if(area=="Eastern"){
      for(i in 0:days){          
        if(weekdays(start.date+i) %in% c("Saturday","Sunday")=="TRUE"){
          freeday<-append(freeday,as.Date.character(start.date+i))
          i=i+1
        }
        else{
          workday<-append(workday,start.date+i)
          i=i+1
        }
      }
    }else{
      for(i in 0:days){          
        if(weekdays(start.date+i)=="Sunday"){
          freeday<-append(freeday,as.Date.character(start.date+i))
          i=i+1
        }
        else{
          workday<-append(workday,start.date+i)
          i=i+1
        }
      }
    }
    
    h<-holidayNERC(as.numeric(year)) #consider NERC holiday
    length(h)
    wl<-length(workday)
    fl<-length(freeday)
    for (j in 1:length(h)){
      if (as.Date(h[j]) %in% workday=="TRUE") {
        wl<-wl-1
        fl<-fl+1
        j=j+1
      }
      else{
        j=j+1
      }
    }
    return(c(wl,fl))
  }
  
  
  
  num.hour<-function(peak.type,wl,fl){      #Calculate hours based on peak.type
    if(peak.type=="onpeak"){
      num.hour<-wl*(22-6)
    }else if(peak.type=="offpeak"){
      num.hour<-fl*24+wl*8
    }else if(peak.type=="flat"){
      num.hour<-(wl+fl)*24
    }else if(peak.type=="2x16H"){
      num.hour<-fl*(22-6)
    }else if(peak.type=="7x8"){
      num.hour<-(wl+fl)*8
    }
    return(num.hour)
  }
  
  
  if(substr(period,5,5)=="Q"){      #Convert period to specific start and end days
    
    mon<-substr(period,6,6)
    startdate=ifelse(mon=="1",paste(year, "1", "1", sep = "-"),ifelse(mon=="2",paste(year, "4", "1", sep = "-"),ifelse(mon=="3",paste(year, "7", "1", sep = "-"),paste(year, "10", "1", sep = "-"))))
    start.date<-as.Date(startdate)
    enddate=ceiling_date(start.date%m+%months(2),'month')-1
    end.date<-as.Date(enddate)
    
    wl<-get.wl.fl(start.date,end.date,year,area)[1]
    fl<-get.wl.fl(start.date,end.date,year,area)[2]
    numhour<-num.hour(peak.type,wl,fl)
    
    if(iso=="MISO"){    #daylight saving adjust
      numhour<-numhour
    }else{
      if(mon=="1"){
        numhour<-numhour-1
      }else if(mon=="4"){
        numhour<-numhour+1
      }else{
        numhour<-numhour
      }
    }
  }else if(substr(period,5,nchar(period)) %in% month=="TRUE"){
    mon<-as.numeric(which(month == substr(period,5,nchar(period))))
    startdate=paste(year,as.character(mon),"1",sep="-")
    start.date<-as.Date(startdate)
    enddate=ceiling_date(start.date%m+%months(0),'month')-1
    end.date<-as.Date(enddate)
    
    wl1<-get.wl.fl(start.date,end.date,year,area)[1]
    fl1<-get.wl.fl(start.date,end.date,year,area)[2]
    numhour<-num.hour(peak.type,wl1,fl1)
    
    if(iso=="MISO"){    #daylight saving adjust
      numhour<-numhour
    }else{
      if(mon==3){
        numhour<-numhour-1
      }else if(mon==11){
        numhour<-numhour+1
      }else{
        numhour<-numhour
      }
    }
    
  }else if(substr(period,5,nchar(period))=="A"){
    startdate=paste(year,"1","1",sep="-")
    start.date<-as.Date(startdate)
    enddate=ceiling_date(start.date%m+%months(11),'month')-1
    end.date<-as.Date(enddate)
    
    wl2<-get.wl.fl(start.date,end.date,year,area)[1]
    fl2<-get.wl.fl(start.date,end.date,year,area)[2]
    numhour<-num.hour(peak.type,wl2,fl2)
  }else{
    startdate=period
    start.date<-as.Date(startdate)
    enddate=period
    end.date<-as.Date(enddate)
    
    wl3<-get.wl.fl(start.date,end.date,year,area)[1]
    fl3<-get.wl.fl(start.date,end.date,year,area)[2]
    numhour<-num.hour(peak.type,wl3,fl3)
  }
  listdata<-list(iso,peak.type,start.date,end.date,numhour)
  names(listdata)<-c("iso","peak.type","start.date","end.date","num.hour")
  return(listdata)
}

get.hours("CAISO","offpeak","2022Mar")
get.hours("CAISO","flat","2022Q1")
get.hours("CAISO","offpeak","2022Q1")
get.hours("CAISO","offpeak","2022A")

get.hours("MISO","flat","2022Mar")
