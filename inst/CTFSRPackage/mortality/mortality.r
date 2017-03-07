# <function>
# <name>
# mortality
# </name>
# <description>
# Mortality is the main function, and is constructed like 
# growth and recruitment. It requires two complete datasets, one per census,
# with dbh, pom, and date for every individual of all species in at least 2 censuses (see Data Format). 
# It can then calculate mortality for the entire forest, or based on one or two user-submitted factors. <br>

# Mortality is based on only on the column status: any tree without an alivecode in census 2 is considered dead.  
# Individuals whose status is NA in either census are deleted from all counts,
# since it's impossible to count them either as survivors or dead.

# It requires fill.dimension and climits in utilities.r.<br><br>
# Output of the mortality function is a list with components:
# <ul>
# <li>N, the number of individuals alive in the census 1 per category selected
# <li>D, the number of individuals no longer alive in census 2
# <li>rate, the mean annualized mortality rate constant per category selected, calculated as (log(N)-log(S))/time 
# <li>upper, upper confidence limit of mean rate
# <li>lower, lower confidence limit of mean rate
# <li>time, mean time interval in years
# <li>date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
# <li>date2, mean date in census 2 
# <li>dbhmean, mean dbh in census 1 of individuals included
# </ul>

# Pass the list to assemble.demography (in utilities.r) with type="m" to convert the list a data.frame.
# </description>
# <arguments>
# <ul>
# <li> Generally, alivecode="A" suffices, as this is the standard in CTFS data for a living tree; "AS" and "AB" are seldom used now
# <li> split1 and split2 must both be vectors of character variables with exactly as many elements as there are rows in the tables census1 and census2
# (or both can be NULL), for instance, species names, dbh categories, or quadrat numbers<br>
# </ul>
# </arguments>
# <sample>
# CTFSplot("bci",5:6)<br>
# mort.data=mortality(bci.full5,bci.full6)<br>
# mort.data$rate<br>
# mort.data=growth(bci.full5,bci.full6,split1=bci.full5$sp)<br>
# mort.data$rate<br>
# assemble.demography(mort.data,type='m')
# </sample>
# <source>
mortality=function(census1,census2,alivecode=c("A","AB","AS"),split1=NULL,split2=NULL)
{
 if(is.null(split1)) split1=rep("all",dim(census1)[1])
 if(is.null(split2)) split2=rep("all",dim(census2)[1])

 inc=!is.na(census1$status) & !is.na(census2$status) & census1$status!="M" & census2$status!="M"
 census1=census1[inc,]
 census2=census2[inc,]
 split1=split1[inc]
 split2=split2[inc]

 time=(census2$date-census1$date)/365.25

 alive1=alive2=rep(FALSE,dim(census1)[1])
 alive1[census1$status=="A"]=TRUE
 for(i in 1:length(alivecode)) alive2[census2$status==alivecode[i]]=TRUE
 
 class1=sort(unique(split1))
 class2=sort(unique(split2))
 splitN=list(split1[alive1],split2[alive1])
 splitS=list(split1[alive1&alive2],split2[alive1&alive2])

 N=tapply(census1$dbh[alive1],splitN,length)
 S=tapply(census1$dbh[alive1&alive2],splitS,length)
 meantime=tapply(time[alive1],splitN,mean,na.rm=T)
 meandbh=tapply(census1$dbh[alive1],splitN,mean,na.rm=T) 
 startdate=tapply(census1$date[alive1],splitN,mean,na.rm=T)
 enddate=tapply(census2$date[alive1],splitN,mean,na.rm=T)

 N=fill.dimension(N,class1,class2)
 S=fill.dimension(S,class1,class2)
 meantime=fill.dimension(meantime,class1,class2,fill=NA)
 meandbh=fill.dimension(meandbh,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)

 if(sum(N)==0)
   return(list(N=rep(NA,length(class1)),D=rep(NA,length(class1)),
               rate=rep(NA,length(class1)),
               lower=rep(NA,length(class1)),upper=rep(NA,length(class1)),
               time=rep(NA,length(class1)),dbhmean=rep(NA,length(class1)),
               date1=rep(NA,length(class1)),date2=rep(NA,length(class1))
              ) 
         )

 m=mortality.calculation(N=as.matrix(N),S=as.matrix(S),meantime=as.matrix(meantime))

 # ord=order(drp(meandbh))
 result=list(N=drp(m$N),D=drp(m$D),rate=drp(m$rate),lower=drp(m$lowerCI),upper=drp(m$upperCI),time=drp(m$time),
             date1=drp(startdate),date2=drp(enddate),dbhmean=drp(meandbh))

 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# mortality.eachspp
# </name>
# <description>
# Calculate mortality for each species in given dbh categories. It sets the split variables using the species name and
# submitted dbh classbreaks and then uses mortality to do the calculation. See argument descriptions for mortality. Return object
# is the list from mortality and can be passed to assemble.demography for a convenient format. 
# </description>
# <arguments>
# </arguments>
# <sample>
# CTFSplot("bci",5:6)<br>
# mort.data=mortality.eachspp(bci.full5,bci.full6)<br>
# mort.table1=assemble.demography(mort.data,type="m",whichdbhcat=1)<br>
# mort.table2=assemble.demography(mort.data,type="m",whichdbhcat=2)<br>
# mort.table3=assemble.demography(mort.data,type="m",whichdbhcat=3)<br>
# </sample>
# <source>
mortality.eachspp=function(census1,census2,classbreak=c(10,100,300),alivecode=c("A","AB","AS"))
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))

 sp=census1$sp
 result=mortality(census1,census2,alivecode=alivecode,split1=sp,split2=dbhclass)
 return(result)
}
# </source>
# </function>

# <function>
# <name>
# mortality.dbh
# </name>
# <description>
# Calculate forest-wide mortality in given dbh categories. See mortality and mortality.eachspp, which have same arguments and same output format.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
mortality.dbh=function(census1,census2,classbreak=c(10,100,300),alivecode=c("A","AB","AS"))
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))

 result=mortality(census1,census2,alivecode=alivecode,split2=dbhclass)
 ord=order(result$dbhmean)
 for(i in 1:length(result)) result[[i]]=result[[i]][ord]
 
 return(result)
}
# </source>
# </function>


# <function>
# <name>
# mortality.calculation
# </name>
# <description>
# This is the calculation of mortality rate and confidence limits, given N 
# (number alive at the outset), S (number of survivors), and time (time interval).
# All three can be arrays, vectors, or scalars, but all three must be identical size. 
# It relies on find.climits. Used by mortality function, but can be used alone.
# </description>
# <arguments>
# </arguments>
# <sample>
# mortality.calculation(N=c(100,1000),S=c(75,750),meantime=c(5.1,5.1))
# </sample>
# <source>
mortality.calculation=function(N,S,meantime)
{
 lower.ci=find.climits(N,(N-S),kind="lower")
 upper.ci=find.climits(N,(N-S),kind="upper")

 mort.rate=(log(N)-log(S))/meantime
 upper.rate=(log(N)-log(N-upper.ci))/meantime
 lower.rate=(log(N)-log(N-lower.ci))/meantime

 mort.rate[S==0]=upper.rate[S==0]=Inf
 upper.rate[upper.ci==N]=Inf
 lower.rate[lower.ci==N]=0
 mort.rate[N==0]=lower.rate[N==0]=upper.rate[N==0]=NA
 
 if(is.null(dim(N)))
   return(data.frame(N=N,S=S,D=N-S,rate=mort.rate,lowerCI=lower.rate,upperCI=upper.rate,
                     time=meantime))
 else
   return(list(N=N,S=S,D=N-S,rate=mort.rate,lowerCI=lower.rate,upperCI=upper.rate,
               time=meantime))
}
# </source>
# </function>

# 
# <function>
# <name>
# find.climits
# </name>
# <description>
# Calculates confidence limits around a number of deaths, D, out of N individuals.
# It uses the beta distribution as the conjugate of the binomial, so the beta is the posterior of the number
# dying. N and D can be vectors or matrices, but must have matching dimensions.
# </description>
# <arguments>
# <ul>
# <li> N, number of individuals alive at the outset
# <li> D, number of deaths by the end
# <li> alpha, the critical probability (default alpha=0.05 gives 95% confidence limits)
# <li> kind, either "upper" or "lower"
# </arguments>
# <sample>
# find.climits(10,5,kind='lower')
# </sample>
# <source>
find.climits=function(N,D,alpha=.05,kind='upper')
{
 if(kind=='lower')
  {
   result=N*(1-qbeta(1-alpha/2,shape1=N-D+1,shape2=D+1))
   result[D==0]=0
  }
 else if(kind=='upper')
  {
   result=N*(1-qbeta(alpha/2,shape1=N-D+1,shape2=D+1))
   result[D==N]=N[D==N]
  }
  
 return(result)
 
}
# </source>
# </function>


