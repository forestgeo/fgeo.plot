
# 
# <function>
# <name>
# growth
# </name>
# <description>
# The principle growth function, constructed like
# recruitment and mortality. It requires two complete datasets, one per census,
# with dbh, pom, and date for every individual of all species in at least 2 censuses (see Data Format). 
# It calculates the mean growth rate in one or more categories defined by the split variables, split1
# and split2. The column date is required for annualizing rates. 
# The columns status and stemID are both required, in order to determine which stems should have dbh change calculated. 
# The function trim.growth handles all
# checks for trees to include; excluded are cases where the stemID changes, extreme values
# based on err.limit and maxgrow, and trees below a minimum dbh in the
# first census. See the description of trim.growth for more information.
# Growth requires fill.dimension in utilities.r. <br><br>
# Output of the growth function is a list with components:
# <ul>
# <li>rate, the mean annualized growth rate per category selected, either dbh increment, or relative growth  
# <li>N, the number of individuals included in the mean (not counting any excluded)
# <li>clim, width of confidence interval; add this number to the mean rate to get upper confidence limit, substract to get lower
# <li>dbhmean, mean dbh in census 1 of individuals included
# <li>time, mean time interval in years
# <li>date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
# <li>date2, mean date in census 2 
# </ul>
# Pass the list to assemble.demography (in utilities.r) with type="g" to convert the list to a data.frame.

# </description>
# <arguments>
# <ul>
# <li> Usually use rounddown=FALSE; if TRUE, all dbh<55 are rounded down to the nearest multiple of 5
# <li> With method='I', annual dbh increment is calculated, (dbh2-dbh1)/time; with method='E', relative growth rate, (log(dbh2)-log(dbh1))/time
# <li> With stdev=FALSE, confidence limits are returned, otherwise the SD in growth rate per group 
# <li> dbhunit must be 'mm' or 'cm'
# <li> mindbh is the minimum dbh to include in results
# <li> growthcol defines how growth is measured, either 'dbh' or 'agb' (agb=biomass)
# <li> for err.limit and maxgrow, see trim.growth()
# <li> split1 and split2 must both be vectors of character variables with exactly as many elements as there are rows in the tables census1 and census2
# (or both can be NULL), for instance, species names, dbh categories, or quadrat numbers
# </ul>
# </arguments>
# <sample>
# CTFSplot("bci",5:6)<br>
# growth.data=growth(bci.full5,bci.full6)<br>
# growth.data$rate<br>
# growth.data=growth(bci.full5,bci.full6,split1=bci.full5$sp)<br>
# growth.data$rate<br>
# assemble.demography(grow.data,type='g')
# </sample>
# <source>
growth=function(census1,census2,rounddown=FALSE,method='I',stdev=FALSE,dbhunit='mm',mindbh=10,growthcol="dbh",
                err.limit=4,maxgrow=75,split1=NULL,split2=NULL)
{
 size1=census1[,growthcol]
 size2=census2[,growthcol]
 
 if(is.null(split1)) split1=rep('all',dim(census1)[1])
 if(is.null(split2)) split2=rep('all',dim(census2)[1])

 if(is.null(census2$codes)) census2$codes=rep('.',length(size2))

 time=(census2$date-census1$date)/365.25

 if(rounddown)
  {
   sm=((size1<55 | size2<55) & !is.na(size1) & !is.na(size2))
   size1[sm]=rndown5(size1[sm])
   size2[sm]=rndown5(size2[sm])
  }

 if(method=='I') growthrate=(size2-size1)/time
 else if(method=='E') growthrate=(log(size2)-log(size1))/time
 good=trim.growth(census1,census2,time,err.limit=err.limit,maxgrow=maxgrow,mindbh=mindbh)
 growthrate[!good]=NA

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 splitgood=list(split1[good],split2[good])

 mean.grow=tapply(growthrate[good],splitgood,mean,na.rm=TRUE)
 sd.grow=tapply(growthrate[good],splitgood,sd,na.rm=TRUE)
 N=tapply(growthrate[good],splitgood,length)
 meandbh=tapply(census1$dbh[good],splitgood,mean,na.rm=TRUE) 
 meansize=tapply(size1[good],splitgood,mean,na.rm=TRUE) 
 interval=tapply(time[good],splitgood,mean,na.rm=TRUE)
 startdate=tapply(census1$date[good],splitgood,mean,na.rm=TRUE)
 enddate=tapply(census2$date[good],splitgood,mean,na.rm=TRUE)

 mean.grow=fill.dimension(mean.grow,class1,class2,fill=NA)
 sd.grow=fill.dimension(sd.grow,class1,class2,fill=NA)
 N=fill.dimension(N,class1,class2,fill=0)
 meandbh=fill.dimension(meandbh,class1,class2,fill=NA)
 interval=fill.dimension(interval,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)

 ci.grow=sd.grow
 ci.grow[N==0]=NA
 ci.grow[N>0]=sd.grow[N>0]*qt(0.975,N[N>0])/sqrt(N[N>0])

 # ord=order(drp(meandbh))
 if(!stdev) 
    result=list(rate=drp(mean.grow),N=drp(N),clim=drp(ci.grow),dbhmean=drp(meandbh),
                time=drp(interval),date1=drp(startdate),date2=drp(enddate))
 else  
    result=list(rate=drp(mean.grow),N=drp(N),sd=drp(sd.grow),dbhmean=drp(meandbh),
                time=drp(interval),date1=drp(startdate),date2=drp(enddate))
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# biomass.growth
# </name>
# <description>
# Like growth(), but calculates change in biomass (agb) instead of dbh. The census tables must have a column
# called agb. There is no trimming done at all -- every tree is included, and its entire biomass (the agb column in the
# standard CTFS data object has total agb, all stems included.)
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
biomass.growth=function(census1,census2,rounddown=FALSE,stdev=FALSE,mindbh=10,split1=NULL,split2=NULL)
{ 
 size1=census1[,"agb"]
 size2=census2[,"agb"]
 
 if(is.null(split1)) split1=rep('all',dim(census1)[1])
 if(is.null(split2)) split2=rep('all',dim(census2)[1])

 time=(census2$date-census1$date)/365.25

 if(rounddown)
  {
   sm=((size1<55 | size2<55) & !is.na(size1) & !is.na(size2))
   size1[sm]=rndown5(size1[sm])
   size2[sm]=rndown5(size2[sm])
  }

 growthrate=(size2-size1)/time

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 good=census1$status=='A' & census2$status=='A' & census1$dbh>=mindbh
 splitgood=list(split1[good],split2[good])

 netgain=tapply(growthrate[good],splitgood,sum,na.rm=TRUE)
 N=tapply(growthrate[good],splitgood,length)
 meanagb=tapply(size1[good],splitgood,mean,na.rm=TRUE) 
 meandbh=tapply(census1$dbh[good],splitgood,mean,na.rm=TRUE) 
 interval=tapply(time[good],splitgood,mean,na.rm=TRUE)
 startdate=tapply(census1$date[good],splitgood,mean,na.rm=TRUE)
 enddate=tapply(census2$date[good],splitgood,mean,na.rm=TRUE)

 netgain=fill.dimension(netgain,class1,class2,fill=NA)
 N=fill.dimension(N,class1,class2,fill=0)
 meanagb=fill.dimension(meanagb,class1,class2,fill=NA)
 meandbh=fill.dimension(meandbh,class1,class2,fill=NA)
 interval=fill.dimension(interval,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)

 # ord=order(drp(meandbh))
 result=list(net=netgain,N=N,agbmean=meanagb,dbhmean=meandbh,time=interval,date1=startdate,date2=enddate)
 
 return(result)
}

# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# growth.eachspp
# </name>
# <description>
# This calculates growth for each species in given dbh categories. It creates the split
# variables then uses growth(). Other arguments are as in growth().
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# growth.result=growth.eachspp(bci.full5,bci.full6,classbreak=c(10,50,100,300,500))
# </sample>
# <source>
growth.eachspp=function(census1,census2,classbreak=c(10,100,300),dbhunit='mm',mindbh=10,growthcol="dbh",
                        err.limit=4,maxgrow=75,rounddown=FALSE,method='I',stdev=FALSE)
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))
 sp=census1$sp
 
 result=growth(census1,census2,rounddown=rounddown,dbhunit=dbhunit,mindbh=mindbh,method=method,growthcol=growthcol,
               err.limit=err.limit,maxgrow=maxgrow,stdev=stdev,split1=sp,split2=dbhclass)

 return(result)
}

# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# growth.dbh
# </name>
# <description>
# This calculates forest-wide growth in given dbh categories. Arguments as for growth().
# </description>
# <arguments>
# </arguments>
# <sample>
# growth.dbh=growth.eachspp(bci.full5,bci.full6,classbreak=c(10,50,100,300,500))
# </sample>
# <source>
growth.dbh=function(census1,census2,classbreak=c(10,100,300),dbhunit='mm',growthcol='dbh',
                    err.limit=4,maxgrow=75,rounddown=FALSE,method='I',stdev=FALSE,mindbh=10)
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))
  
 result=growth(census1,census2,mindbh=mindbh,dbhunit=dbhunit,rounddown=rounddown,method=method,growthcol=growthcol,
               err.limit=err.limit,maxgrow=maxgrow,stdev=stdev,split2=dbhclass)

 ord=order(result$dbhmean)
 for(i in 1:length(result)) result[[i]]=result[[i]][ord]

 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# growth.indiv
# </name>
# <description>
# This returns a complete table with growth rate of every individual, both relative and dbh-increment. The table
# also includes most other key pieces of information for every individual: species, dbh, coordinates. Growth is trimmed with trim.growth,
# and growth is returned as NA if the individual is excluded; note, though, that every individual tree is always included in the table, even
# if growth=NA. Arguments are the same as in growth().
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# growth.table=growth.indiv(bci.full5,bci.full6)
# </sample>
# <source>
growth.indiv=function(census1,census2,rounddown=FALSE,mindbh=10,dbhunit='mm',err.limit=4,maxgrow=75)
{
 if(is.null(census2$codes)) census2$codes=rep('.',length(census2$dbh))

 time=(census2$date-census1$date)/365.25

 if(rounddown)
  {
   sm=((census1$dbh<55 | census2$dbh<55) & !is.na(census1$dbh) & !is.na(census2$dbh))
   census1$dbh[sm]=rndown5(census1$dbh[sm])
   census2$dbh[sm]=rndown5(census2$dbh[sm])
  }

 incgrowth=(census2$dbh-census1$dbh)/time
 expgrowth=(log(census2$dbh)-log(census1$dbh))/time
 good=trim.growth(census1,census2,time,err.limit=err.limit,maxgrow=maxgrow,mindbh=mindbh,dbhunit=dbhunit)
 good[census1$dbh<mindbh]=FALSE
 incgrowth[!good]=expgrowth[!good]=NA

 growthdata=data.frame(treeID=census1$treeID,tag=I(census1$tag),sp=I(census1$sp),gx=census1$gx,gy=census1$gy,dbh1=census1$dbh,dbh2=census2$dbh,time=time,
                       incgr=incgrowth,expgr=expgrowth,status1=census1$status,status2=census2$status)

 return(growthdata)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# trim.growth
# </name>
# <description>
# This is where growth rates are excluded. It is based on 
# a linear model estimating the standard deviation of dbh measures (due to error, that
# is); the parameters slope and intercept define the linear relationship between
# this error deviation and dbh. Any case where the second dbh measure is more than
# 4 standard deviations below the first is marked false, meaning it will be excluded from
# growth measurements. The default values of slope and intercept are based on dbh
# remeasure tests done in both 1995 and 2000 at BCI. A line was fitted through the absolute 
# dbh errors as a function of dbh in both years; the average slope and intercept is
# used here. The function also excludes any growth rate > 75 mm per yr, cases
# where the stemID changes, and if the POM changes by more than 5%. 
# All parameters for excluding growth measures based on error can be adjusted: 
# to include all measures, set maxgrow and err.limit to very high numbers, such as 10000;
# to include POM changes, set pomcut to a high number, such as 10;
# to include cases where stemID changed, set exclude.stem.change=FALSE.
# This function is usually only used inside the other growth functions. 
# With exclude.stem.change==FALSE, keep all cases where stem changes, regardless of growth (it does not make sense to exclude
# a record based on growth when the growth is based on different stems).
# Note that trees are exclude if cens1$dbh<mindbh, but not if cens2$dbh<mindbh. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
trim.growth=function(cens1,cens2,time,slope=0.006214,intercept=.9036,err.limit=4,maxgrow=75,pomcut=0.05,mindbh=10,dbhunit='mm',
                     exclude.stem.change=TRUE)
{
 if(dbhunit=='cm') intercept=intercept/10
 stdev.dbh1=slope*cens1$dbh+intercept

 growth=(cens2$dbh-cens1$dbh)/time

 bad.neggrow=which(cens2$dbh<=(cens1$dbh-err.limit*stdev.dbh1)) 
 bad.posgrow=which(growth>maxgrow)

 pomdiff=abs(as.numeric(cens2$pom)-as.numeric(cens1$pom))/as.numeric(cens1$pom)
 
 accept=rep(TRUE,length(cens1$dbh))
 accept[pomdiff>pomcut]=FALSE
 accept[bad.neggrow]=FALSE
 accept[bad.posgrow]=FALSE
 accept[is.na(growth)]=FALSE
 
 if(exclude.stem.change) accept[cens1$stemID!=cens2$stemID]=FALSE
 # BAD MISTAKE INCLUDING THIS!! else accept[cens1$stemID!=cens2$stemID]=TRUE

 accept[cens1$dbh<mindbh]=FALSE
 accept[is.na(cens1$dbh) | is.na(cens2$dbh) | cens2$dbh<=0]=FALSE

 return(accept)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# growth.biomass.indiv
# </name>
# <description>
# Like growth.indiv but based on agb growth, not dbh growth. Extreme growth rates (based on dbh growth) are
# excluded, but cases where the stemID changed are not excluded. 
# Here pomcut is used in a very specific way probably only relevant at BCI. If the second pom is higher than the first by more than
# the pomcut, the record is not used. The function trim.growth has already eliminated cases where the stemID is unchanged and pom
# changes, so this will only serve for cases where two different stemIDs have measurements. At BCI, in most cases where the second pom
# is lower than the first and the stem changed, it is a legitimate stem change. But where the second pom is higher, it is really the
# same stem measured at a different pom, and with a different stemID because BCI lacks stem tags. 
# For most plots, especially with stem tags, the default behavior means changes in stem allow changes in pom to be included in biomass growth.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
growth.biomass.indiv=function(census1,census2,mindbh=10,steminfo=FALSE,dbhunit="mm",err.limit=4,maxgrow=75,rounddown=NULL,
                              exclude.stem.change=TRUE,pomcut=10000)
{
 time=(census2$date-census1$date)/365.25

 incgrowth=(census2$agb-census1$agb)/time
 expgrowth=(log(census2$agb)-log(census1$agb))/time

 good=trim.growth(census1,census2,time,err.limit=err.limit,maxgrow=maxgrow,mindbh=mindbh,dbhunit=dbhunit,exclude.stem.change=exclude.stem.change)

 pomdiff=(as.numeric(census2$pom)-as.numeric(census1$pom))/as.numeric(census1$pom)

 include=which(census1$dbh>=mindbh & census1$status=='A' & census2$status=='A' & pomdiff<pomcut)
 incgrowth[-include]=expgrowth[-include]=NA
 incgrowth[!good]=NA
 
 if(steminfo)
  growthdata=data.frame(treeID=census1$treeID,tag=I(census1$tag),stemID=census1$stemID,stemtag=I(census1$StemTag),
                        sp=I(census1$sp),gx=census1$gx,gy=census1$gy,dbh1=census1$dbh,dbh2=census2$dbh,
                        agb1=census1$agb,agb2=census2$agb,time=time,incgr=incgrowth,expgr=expgrowth,
                        status1=I(census1$status),status2=I(census2$status),codes1=I(census1$codes),codes2=I(census2$codes))
  
 else
  growthdata=data.frame(treeID=census1$treeID,tag=I(census1$tag),sp=I(census1$sp),gx=census1$gx,gy=census1$gy,dbh1=census1$dbh,dbh2=census2$dbh,
                        agb1=census1$agb,agb2=census2$agb,time=time,incgr=incgrowth,expgr=expgrowth,
                        status1=I(census1$status),status2=I(census2$status),codes1=I(census1$codes),codes2=I(census2$codes))

 return(growthdata)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# DBHtransition
# </name>
# <description>
# Calculates a transition matrix of individuals by diameter categories from two censuses.
# The missing code (M) is checked in codes field if misscode is set; otherwise, status=M is assumed to mean missing
# and status=AB is assumed to mean the stem was lost, so there is no dbh.
# Growth rates above maxgrow and below mingrow are excluded, where max and min are annual increments.
# (Not tested recently and not part of the supported CTFS R package.)

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
DBHtransition=function(data1,data2,dbhbreaks=c(10,50,100,300,10000),maxgrow=45,misscode='M')
{
 dbhcat1=as.character(cut(data1$dbh,breaks=dbhbreaks,labels=FALSE,right=FALSE))
 dbhcat2=as.character(cut(data2$dbh,breaks=dbhbreaks,labels=FALSE,right=FALSE))

 dbhcat1[data1$status=='P' & data2$status=='A']='recruit'
 dbhcat2[data2$status=='D']='dead'

 if(!is.null(misscode)) 
  {
   dbhcat1[data1$status=='A' & is.na(data1$dbh) & data1$codes!=misscode]=0
   dbhcat2[data2$status=='A' & is.na(data2$dbh) & data2$codes!=misscode]=0
  }
 else 
  {
   dbhcat1[data1$status=='AB']=0
   dbhcat2[data2$status=='AB']=0
  }
   
 if(!is.null(misscode))
  {
   dbhcat1[data1$codes==misscode & data1$status=='A']='alive.missing'
   dbhcat1[data1$codes==misscode & data1$status=='D']='dead.missing'
   dbhcat1[data1$codes==misscode & data1$status!='A' & data1$status!='D']='blank.missing'
   dbhcat2[data2$codes==misscode & data2$status=='A']='alive.missing'
   dbhcat2[data2$codes==misscode & data2$status=='D']='dead.missing'
   dbhcat2[data2$codes==misscode & data2$status!='A' & data2$status!='D']='blank.missing'
  }
 else 
  {
   dbhcat1[data1$status=='M']='missing'
   dbhcat2[data2$status=='M']='missing'
  }

 missingtree=rep(FALSE,length(dbhcat1))
 missingtree[grep('missing',dbhcat1)]=TRUE
 missingtree[grep('missing',dbhcat2)]=TRUE
 missingtree[is.na(dbhcat1) | is.na(dbhcat2)]=TRUE

 goodgrowth=trim.growth.mismeasure(dbh1=data1$dbh,dbh2=data2$dbh,time=(data2$date-data1$date)/365.25,maxgrow=maxgrow)
 goodrecruit=trim.growth.mismeasure(dbh1=rep(10,length(data1$dbh)),dbh2=data2$dbh,time=(data2$date-data1$date)/365.25,maxgrow=maxgrow)
 goodgrowth[!goodrecruit & data1$status=='P']=FALSE

 goodrecord=table(dbhcat1[!missingtree & goodgrowth],dbhcat2[!missingtree & goodgrowth])
 badrecord=table(dbhcat1[!missingtree & !goodgrowth],dbhcat2[!missingtree & !goodgrowth])
 return(list(good=order.by.rowcol(goodrecord),bad=order.by.rowcol(badrecord)))
}



 
 

# </source>
# </function>
# 
# 
