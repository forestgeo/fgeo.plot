# Topographic calculation based on stake and the height difference between them.

# <function>
# <name>
# solve.topo
# </name>
# <description>
# This is based on the problem and solution outlined in my book on plot methods (1998):
# Each of N stakes i has an estimated height E[i] and a true height e[i]. 
# Pairs of stakes have a height difference d[i,j], where e[i]+d[i,j]=e[j],
# but only estimated height differences D[i,j] are known.<br><br>

# The least-squares estimate E[i] of e[i] can be written as the mean of the n[i]
# points j for which D[i,j] was measured:
# <ul>
# <li> E[i]=mean(E[j]-D[i,j]) = (1/n[i])*sum(E[j]) - (1/n[i])*sum(D[i,j])
# <li> n[i]*E[i]-sum(E[j]) = -sum(D[i,j])
# </ul>
# The latter produces N equations in N unknowns, but they are exactly singular.
# One of the points must be assigned a value, and it's easiest to set e[1] = 0.
# The effect is to exclude the equation for i=0, but all j for which D[0,j] was
# measured have unchanged equations (D[0,j] are included).<br><br>

# The equations are written in matrix form. The coefficients are a matrix M whose
# diagonal elements are the n[i], so M[i,i] = n[i] for all i. All other entries are -1 
# where D[i,j] was measured otherwise zero. The estimated E[i] are a vector, and the
# vector V[i] = -sum(D[i,j]), where the summation is over j only. Then M*%*E=V, Minv
# is the inverse of M, and E=Minv*%*V.<br><br>

# In theory, the program will accept duplicate measures of the same pair of plots.
# They are treated as replicates with equal weight to all other estimates.
# There would be -2 or -3 etc off the diagonal.<br><br>

# The data are in columns. One column has the label of one stake (column header pt1), the second
# has the label of the second stake (column header pt2), and the final column has the height
# difference at pt2 minus pt1 (column header diff). The point labelled basept is assigned elevation baseelev. 
# The last 5 arguments allow those column headers to be reassigned.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# See topography tutorial
# </sample>
# <source>
solve.topo=function(coldata,column1="pt1",column2="pt2",diffcolumn="htdiff",basept="1",baseelev=0,debug=NULL)
{
 fromcol=which(colnames(coldata)==column1)
 tocol=which(colnames(coldata)==column2)
 Dcol=which(colnames(coldata)==diffcolumn)
 
 pt1=coldata[,fromcol]
 pt2=coldata[,tocol]
 D=coldata[,Dcol]

 coldata=data.frame(pt1,pt2,D)
 
 ptLabels=sort(unique(c(pt1,pt2)))
 nopts=length(ptLabels)
 
 M=matrix(0,nrow=nopts,ncol=nopts)
 rownames(M)=colnames(M)=ptLabels
 V=numeric(nopts)
 names(V)=ptLabels

 for(i in 1:nopts)
  {
   links2=getTopoLinks(index=i,labels=ptLabels,data=coldata,from=fromcol,to=tocol,backward=FALSE)
   links1=getTopoLinks(index=i,labels=ptLabels,data=coldata,from=tocol,to=fromcol,backward=TRUE)
   
   if(links2$error | links1$error) return("remove sightings from any point to itself")
   if(!is.null(debug)) if(ptLabels[i]==debug) browser()
   
   V[i]=-(links2$diff+links1$diff)
   
   M[i,i]=sum(links2$count)+sum(links1$count)
   
   M[i,links2$indices]=M[i,links2$indices]-links2$count
   M[i,links1$indices]=M[i,links1$indices]-links1$count
   
   if(i%%100==1) cat("at point ", i, "\n")
  }
  
 baseIndex=which(ptLabels==basept)
 if(length(baseIndex)==0) { cat("Error: You must supply a valid base point\n"); return(NULL) }
 Mcorr=M[-baseIndex,-baseIndex]
 Vcorr=V[-baseIndex]
 
 Minv=solve(Mcorr)
 elev=Minv%*%Vcorr
 
 result=data.frame(label=I(c(basept,names(Vcorr))),elev=c(baseelev,elev))
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# getTopoLinks
# </name>
# <description>
# This is solely for use by solve.topo. It finds all points linked via a sighting to a given point. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
getTopoLinks=function(index,labels,data,from,to,backward=FALSE)
{
 focalLabel=labels[index]
 
 oneptSightings=subset(data,data[,from]==focalLabel)
 if(dim(oneptSightings)[1]==0) return(list(name=NA,count=numeric(0),diff=0,error=FALSE))
 
 allLinkLabels=oneptSightings[,to]
 
 linkIndices=match(allLinkLabels,labels)
   
 m=match(index,linkIndices)
 if(length(m[!is.na(m)])>0) return(list(error=TRUE))
 
 linkCount=table(linkIndices)
# if(length(which(linkCount>1))>0) browser()
 
 uniqueLinkIndices=as.numeric(names(linkCount))
 distsum=sum(oneptSightings$D)
 if(backward) distsum=(-distsum)

# browser()
 return(list(indices=uniqueLinkIndices,labels=allLinkLabels,count=linkCount,diff=distsum,error=FALSE))
}
# </source>
# </function>
# 
# 

# <function>
# <name>
# rearrangeSurveyData
# </name>
# <description>
# Takes a table of survey sightings with columns of x and y locations of two points, and converts it to the format
# required by solve.topo. The input table must have columns x1, y1, x2, and y2. The return value is a list consisting of two dataframes: 
# <ul>
# <li> all points found in the input table, with an integer designation assigned to each. The designation is called pt. 
# <li> the second table matches the input table, 
# but instead of x-y coordinates for the two points, only columns pt1 and pt2 are included to 
# indicate the two points between which a sighting was taken. 
# </ul>
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rearrangeSurveyData=function(inputtable)
{
 startpts=inputtable[,c('x1','y1')]
 endpts=inputtable[,c('x2','y2')]
 colnames(startpts)=colnames(endpts)=c('x','y')
 allpt=unique(rbind(startpts,endpts))
 allpt$pt=1:dim(allpt)[1]
 
 m=match.dataframe(inputtable[,c('x1','y1')],allpt[,c('x','y')])
 inputtable$pt1=allpt$pt[m]

 m=match.dataframe(inputtable[,c('x2','y2')],allpt[,c('x','y')])
 inputtable$pt2=allpt$pt[m]

 outputcol=unique(c('pt1','pt2',colnames(inputtable)))
 remove=outputcol %in% c('x1','y1','x2','y2')
 
 result=subset(inputtable,select=outputcol[!remove])
 return(list(allpt=allpt,IDtable=result))
}
# </source>
# </function>
# 
# 
