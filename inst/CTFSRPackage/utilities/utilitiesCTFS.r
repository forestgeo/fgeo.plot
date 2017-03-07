# <function>
# <name>
# CTFSplot
# </name>
# <description>
# A convenience for attaching the R Analytical Tables, including the species table. One or more censuses can be requested, and either the
# 'full' or the 'stem' tables. The path in which R Tables are stored is submitted; if in a folder called CTFSRPackage, then the default works.
# Within that folder, there must be subfolders named full, stem, and species for the three types of tables. The function 'attach_if_needed'
# is used, so there is no penalty to requesting a table that is already attached.
# </description>
# <arguments>
# <ul>
# <li> plot: Name of plot as it appears in the names of the R Analytical Tables, in quote marks
# <li> census: census numbers as they appear in the names of the R Analytical Tables; can be a vector
# <li> type: either full or stem, in quote marks
# <li> path: the name of the folder in which the tables are stored, defaults to CTFSRPackage
# <li> remove: if TRUE, the tables are detached, otherwise they are attached
# <li> includespp: can be set to FALSE if the species table is not available
# </ul>
# </arguments>
# <sample>
# CTFSplot(plot='bci',census=1:2)
# CTFSplot(plot='bci',census=2:3)
# CTFSplot(plot='sinharaja,census=3,type='stem',path='C:/SinharajaRDataTables')
# </sample>
# <source>
CTFSplot=function(plot,census=1,type='full',path='CTFSRPackage',remove=FALSE,includespp=TRUE)
{
 if(includespp) 
  {
   spfile=pst(path,'/species/',plot,'.spptable.rdata')
   if(remove) detachfiles(spfile)
   else attach_if_needed(spfile)
  }
  
 filename=paste(path,'/',type,'/',plot,'.',type,census,'.rdata',sep='')
 if(!remove) return(attach_if_needed(filename))
 
 detachfiles(filename)
}


# </source>
# </function>
# 
# 


# <function>
# <name>
# load.species
# </name>
# <description>
#  A function for extracting a single species' dataframe from the large spp dataset
# (list of dataframes, one per species). The split data file must come
# as a name, that is in quote marks.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
load.species=function(spname,sppdatafile)
{
 index=which(names(sppdatafile)==spname)
 return(sppdatafile[[index]])
}

# </source>
# </function>
# 
# 
# <function>
# <name>
# rndown5
# </name>
# <description>
#  Rounds a numeric value to the next lowest multiple of 5.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rndown5=function(s) 
 return(5*floor(s/5))

# </source>
# </function>
# 
# 
# <function>
# <name>
# countspp
# </name>
# <description>
#  Returns the number of elements in a numeric vector > 0. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
countspp=function(x) 
 return(length(subset(x,x>0)))

# </source>
# </function>

# 
# <function>
# <name>
# assemble.demography
# </name>
# <description>
# Takes output of a demographic analysis (produced by functions growth, mortality, or pop.change)
# and converts into one dataframe. Only indicated dbh categories are included; be sure that whichdbhcat does 
# not exceed the number of columns in the data submitted. Type is 'g' for growth, 'm'
# for mortality, 'ba' for basal area, 'agb' for biomass, 'r' for recruitment, and 'a' for abundance.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# data=pop.change(bci.full5,bci.full6,split1=bci.full5$sp);
# result=assemble.demography(data,type='a',whichdbhcat=1)
# data=growth(bci.full5,bci.full6,split1=bci.full5$sp)
# result=assemble.demography(data,type='g',whichdbhcat=1)
# data=mortality.eachspp(bci.full5,bci.full6,classbreak=c(10,100))
# result1=assemble.demography(data,type='m',whichdbhcat=1)
# result2=assemble.demography(data,type='m',whichdbhcat=2)
# </sample>
# <source>
assemble.demography=function(output,type='g',whichdbhcat=1,date1='1960-1-1')
{
 noclass=length(output$rate)
 len=1:noclass
 julian1=as.integer(tojulian(date1,'%Y-%m-%d'))

 for(i in 1:length(output)) output[[i]]=data.frame(output[[i]])
 
 if(type=='g') 
  {
   result=data.frame(output$rate[,whichdbhcat],output$rate[,whichdbhcat]+output$clim[,whichdbhcat],
                     output$rate[,whichdbhcat]-output$clim[,whichdbhcat],output$N[,whichdbhcat],
                     output$time[,whichdbhcat],output$dbhmean[,whichdbhcat])

   result=cbind(result,fromjulian(output$date1[,whichdbhcat]+julian1,'%d%b%Y'),
                       fromjulian(output$date2[,whichdbhcat]+julian1,'%d%b%Y'))
   
   result=convert.factor(result)
   ## rownames not maintained when output is a dataframe, thus must be assigned
   rownames(result)=rownames(output$rate)
   
   headers=outer(c('rate','upper','lower','N','time','dbhmean','start','end'),whichdbhcat,paste,sep='.')
   colnames(result)=as.vector(t(headers))
  }
  
 else if(type=='m')
  {
   result=data.frame(output$rate[,whichdbhcat],output$upper[,whichdbhcat],output$lower[,whichdbhcat],
                     output$N[,whichdbhcat],output$N[,whichdbhcat]-output$D[,whichdbhcat],
                     output$time[,whichdbhcat],output$dbhmean[,whichdbhcat])

   result=cbind(result,fromjulian(output$date1[,whichdbhcat]+julian1,'%d%b%Y'),
                       fromjulian(output$date2[,whichdbhcat]+julian1,'%d%b%Y'))

   result=convert.factor(result)

   rownames(result)=rownames(output$rate)
   headers=outer(c('rate','upper','lower','N','S','time','dbhmean','start','end'),whichdbhcat,paste,sep='.')
   colnames(result)=as.vector(t(headers))
  }
                    
 else if(type=='a')
  {
   result=data.frame(N.1=output$N.1[,whichdbhcat],N.2=output$N.2[,whichdbhcat],
                     interval=output$interval[,whichdbhcat],
                     little.r=output$little.r[,whichdbhcat],
                     I(fromjulian(output$date1[,whichdbhcat]+julian1,'%d%b%Y')),
                     I(fromjulian(output$date2[,whichdbhcat]+julian1,'%d%b%Y')))
   rownames(result)=rownames(output$N.1)
   colnames(result)=c('N.1','N.2','interval','little.r','start','end')
  }
                    
 else if(type=='ba')
  {
   result=data.frame(BA.1=output$BA.1[,whichdbhcat],BA.2=output$BA.2[,whichdbhcat],
                     interval=output$interval[,whichdbhcat],
                     little.r=output$little.r[,whichdbhcat],
                     I(fromjulian(output$date1[,whichdbhcat]+julian1,'%d%b%Y')),
                     I(fromjulian(output$date2[,whichdbhcat]+julian1,'%d%b%Y')))
   rownames(result)=rownames(output$BA.1)
   colnames(result)=c('BA.1','BA.2','interval','little.r','start','end')
  }

 else if(type=='agb')
  {
   result=data.frame(AGB.1=output$AGB.1[,whichdbhcat],AGB.2=output$AGB.2[,whichdbhcat],
                     interval=output$interval[,whichdbhcat],
                     little.r=output$little.r[,whichdbhcat],
                     I(fromjulian(output$date1[,whichdbhcat]+julian1,'%d%b%Y')),
                     I(fromjulian(output$date2[,whichdbhcat]+julian1,'%d%b%Y')))
   rownames(result)=rownames(output$AGB.1)
   colnames(result)=c('AGB.1','AGB.2','interval','little.r','start','end')
  }

 else if(type=='r')
  {
   result=data.frame(output$N2[,whichdbhcat],output$R[,whichdbhcat],output$rate[,whichdbhcat],
                     output$upper[,whichdbhcat],output$lower[,whichdbhcat],output$time[,whichdbhcat],
                     I(fromjulian(output$date1+julian1,'%d%b%Y')),
                     I(fromjulian(output$date2+julian1,'%d%b%Y')))
   rownames(result)=rownames(output$N2) 
   colnames(result)=c('N2','R','rate','clim.up','clim.low','time','start','end')
    
  }
 
 return(result)
}

# </source>
# </function>
# 

# 
# <function>
# <name>
# clean.demography
# </name>
# <description>
#  This takes a CTFS demography table, output by functions mortality, growth, or recruitment,
# and removes rows where N==0, or key data are NA. The rownames are assumed to refer to species names, and
# some codes can be excluded using the argument excludespp. The four columns
# can be submitted by name or number using default column headers, or by
# setting type to 'mort' or 'abund'.
# It returns a logical vector indicating with TRUE which rows to keep, not the cleaned table itself.
# This was formerly in utilities.r.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# rows.include=clean.demography(demogtable,type='growth',Ncol='N',ratecol='ratecol');
# result=demogtable[rows.include,]

# </sample>
# <source>
clean.demography=function(demogdata,type='mort',Ncol=NULL,Scol=NULL,timecol=NULL,ratecol=NULL,excludespp=NULL)
{
 if(type=='mort') { ratecol='rate.1'; Ncol='N.1'; Scol='S.1'; timecol='time.1' }
 else if(type=='abund') { ratecol='little.r'; Ncol='N.1'; Scol='N.2'; timecol='interval' }
 else if(type=='recruit') { ratecol='rate'; Ncol='N2'; Scol='R'; timecol='time' }

 records=dim(demogdata)[1]

 include=N=S=time=rate=rep(TRUE,records)

 include[unidentified.species(rownames(demogdata),excludespp)]=FALSE

 if(!is.null(Scol)) S=demogdata[,Scol]
 if(!is.null(timecol)) time=demogdata[,timecol]
 if(!is.null(ratecol)) rate=demogdata[,ratecol]
 if(!is.null(Ncol)) N=demogdata[,Ncol]

 include[is.na(N) | is.na(S) | is.na(time) | is.na(rate) | N==0]=FALSE

 return(include)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# unidentified.species
# </name>
# <description>
# Takes a string of species names or codes and returns a logical vector indicating
# with TRUE those that should be excluded. Any species name (code) matching precisely the names in exactstr
# are excluded, as well as any which has characters matching partialstr. Either or both exactstr and partialstr
# can be NULL. The typical use if for excluding species whose codes or names
# indicate they are not fully identified. It returns a logical vector which is TRUE for those to be excluded.
# This was formerly in utilities.r.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
unidentified.species=function(spnames,exactstr=c('UNIDEN','UNID','uniden','unid'),partialstr=c('XX','**'))
{
 remove=numeric()
 unidname=rep(FALSE,length(spnames))
 
 if(is.null(exactstr) & is.null(partialstr)) return(unidname)
 
 for(i in 1:length(partialstr))
   remove=c(remove,grep(partialstr[i],spnames,fixed=TRUE))
   
 for(i in 1:length(exactstr))
   remove=c(remove,which(spnames==exactstr[i]))
   
 unidname[remove]=TRUE
 unidname[is.na(spnames)]=TRUE
 return(unidname)
} 



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# exclude.unidentified
# </name>
# <description>
# A more specialized version of unidentified species. It excludes species codes matching any listed in speciesnames
# but only for one specific plot. This way a code can be eliminated from one plot's results, but not any other plot.
# It returns a logical vector, TRUE for species to be excluded. This was formerly in utilities.r.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
exclude.unidentified=function(speciesnames,plotname)
{
 if(length(grep('bci',plotname))>0) excludespp=c('BACT','INGACO','INGAM2','INGATH')
 else if(length(grep('mudum',plotname))>0) excludespp=c('bama')
 else excludespp='nothingexcludedhere'
   
 return(unidentified.species(speciesnames,partialstr=c('XX',excludespp)))
}   
# </source>
# </function>
# 
# 
# 
