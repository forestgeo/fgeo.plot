# <function>
# <name>
# recruitment
# </name>
# <description>
# Functions for calculating recruitment rates.

# Recruitment is the main function, and is constructed like 
# growth and mortality. It requires two complete datasets, one per census,
# with dbh, pom, and date for every individual of all species in at least 2 censuses. 
# It can then calculate mortality based on up to user-submitted factors. The two
# datasets have exactly the same individuals, in exactly the same order, one 
# individual per row.

# Recruitment is based on status and dbh. Any status indicating a live tree can be submitted
# in the variable alivecode. Survivors are all individuals alive in both censuses,
# with status==A in the first census, and larger than the minimum dbh in the first
# census. The total population in the second census includes all those alive,
# above the minimum dbh, plus any other survivors.

# As in mortality, individuals whose status is NA in either census are deleted
# from all calculations.

# Requires fill.dimension and climit function in utilities.r.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
recruitment=function(census1,census2,mindbh=10,alivecode=c("A","AB","AS"),split1=NULL,split2=NULL)
{
 if(is.null(split1)) split1=rep("all",dim(census1)[1])
 if(is.null(split2)) split2=rep("all",dim(census2)[1])

 inc=!is.na(census1$status) & !is.na(census2$status) & census1$status!="M" & census2$status!="M"
 census1=census1[inc,]
 census2=census2[inc,]
 split1=split1[inc]
 split2=split2[inc]
 
 time=(census2$date-census1$date)/365.25

 survivor=alive1=alive2=rep(FALSE,length(census1$status))
 alive1[census1$status=="A"]=TRUE
 for(i in 1:length(alivecode))
  {
   survivor[census1$status=="A" & census2$status==alivecode[i]]=TRUE
   alive2[census2$status==alivecode[i]]=TRUE
  }

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 
 S.inc = survivor & census1$dbh>=mindbh
 N2.inc = (alive2 & census2$dbh>=mindbh) | S.inc
 
 splitS=list(split1[S.inc],split2[S.inc])
 splitN=list(split1[alive1],split2[alive1]) 
 splitN2=list(split1[N2.inc],split2[N2.inc])
   
 S=tapply(census2$dbh[S.inc],splitS,length)
 N2=tapply(census2$dbh[N2.inc],splitN2,length)
 timeint=tapply(time[N2.inc],splitN2,mean,na.rm=T)
 startdate=tapply(census1$date[alive1],splitN,mean,na.rm=T)
 enddate=tapply(census2$date[N2.inc],splitN2,mean,na.rm=T)
 
 S=fill.dimension(S,class1,class2)
 N2=fill.dimension(N2,class1,class2)
 timeint=fill.dimension(timeint,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)

 if(sum(N2)==0)
   return(list(N2=rep(NA,length(class1)),R=rep(NA,length(class1)),
               rate=rep(NA,length(class1)),lower=rep(NA,length(class1)),
               upper=rep(NA,length(class1)),time=rep(NA,length(class1)),
               date1=rep(NA,length(class1)),date2=rep(NA,length(class1))))
               
 lower.ci=upper.ci=N2
 lower.ci=find.climits(as.matrix(N2),as.matrix(S),kind="lower")
 upper.ci=find.climits(as.matrix(N2),as.matrix(S),kind="upper")

 rec.rate=(log(N2)-log(S))/timeint
 upper.rate=(log(N2)-log(lower.ci))/timeint
 lower.rate=(log(N2)-log(upper.ci))/timeint

 rec.rate[S==0]=upper.rate[S==0]=Inf
 upper.rate[lower.ci==0]=Inf
 rec.rate[N2==0]=lower.rate[N2==0]=upper.rate[N2==0]=NA

 result=list(N2=drp(N2),R=drp(N2-S),rate=drp(rec.rate),lower=drp(lower.rate),upper=drp(upper.rate),
             time=drp(timeint),date1=drp(startdate),date2=drp(enddate))

 return(result)
}
# </source>
# </function>




# <function>
# <name>
# recruitment.eachspp
# </name>
# <description>
# A wrapper to calculate recruitment for each species.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
recruitment.eachspp=function(census1,census2,mindbh=10,alivecode=c("A","AB","AS"))
{
 result=recruitment(census1,census2,mindbh=mindbh,alivecode=alivecode,split1=census1$sp)
 return(result)
}
# </source>
# </function>

