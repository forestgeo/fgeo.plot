# <function>
# <name>
# fullplot.imageJ
# </name>
# <description>
# Reads quadrat maps created by imageJ, converting the digitized pixel to coordinates within a quadrat (local coordinates).<br />
# The input imageJ files must be named in a very specific way: all should have the same prefix and the quadrat names should
# have 4 digits. If there are subquadrats, the names should end with the same consistent suffixes. Only those files with this 
# specific naming convention are read and converted. Updated Aug 2014 to handle files with no trees (just 4 corner labels). 
# </description>
# <arguments>
# <ul>
# <li> path: the complete path name where the map files to be converted are found
# <li> include.subdir: whether the subfolders are to be searched for map files also
# <li> outfile: the name of the text file where the results will be saved. This file will be saved in the folder specified by the path. If outfile=NULL, 
# the results will not be written to a file.
# <li> corners: specify the tags used for the map corners used for calibration. They must be named in a clockwise direction starting with the lower left corner. 
# These tags must be the same in each and every map file.
# <li> colrange: specifies the range of the columns, found as the first two digits of the quadrat name
# <li> rowrange: specifies the range of the rows, found in the last two digits of the quadrat name
# <li> prefix: the prefix used for all the map files before the quadrat name
# <li> suffix: the extension used for the map files. The imageJ default is ".txt".
# <li> subquadratsuffix: used for map files that are smaller than 20x20m (i.e. 10x10m). They should be named clockwise from the lower left subquadrat.
# <li> gridsize: size of each individual map
# </ul>
# </arguments>
# <sample>
# mapfolder='/maps/rabi/'
# coords=
# fullplot.imageJ(path=mapfolder,include.subdir=T,gridsize=c(10,10),outfile='location.txt',corners=c('p2','p1','p4','p3'),
#                 prefix='Map_',colrange=c(0,49), rowrange=c(0,24),subquadsuffix=c('_1','_2','_3','_4'))
# head(coords)
# dim(coords)
# range(coords$lx)
# range(coords$ly)
# </sample>
# <source>
fullplot.imageJ=function(path="",outfile='plotLxLy.txt',delim=',', include.subdir=T, corners=c('p1','p2','p3','p4'),
                         colrange=c(0,49), rowrange=c(0,24), prefix='Map_', suffix=".txt",
						 subquadsuffix=c('_1','_2','_3','_4'),gridsize=c(10,10),debug=NULL)
{
 # If include.subdir=T, all subfolders will be checked, else only the folder specified in the path will be checked <br>
 if(include.subdir != T) include.subdir=F
  
 if (path=="") path='.'
 
 # If path does not end with "/", add this to path <br>
 lenpath = length(unlist(strsplit(path,'')))
 if (substr(path,lenpath,lenpath+1) != '/')    path=paste(path,'/',sep='')
 
  # Get all the quadrat names
 allcol=colrange[1]:colrange[2]
 allrow=rowrange[1]:rowrange[2]
 allquad=as.vector(t(outer(convert.rowcol(allcol),convert.rowcol(allrow),pst)))

 # Get all the files specified by the path and subdirectories if included
 filelist=list.files(recursive=include.subdir, path=path)
 
 # Screen the files for those that have quadrat names, and the specified prefix and suffix.
 quadfiles=character()
 for(q in allquad) 
   quadfiles=c(quadfiles,filelist[logical.grep(q,filelist) & logical.grep(prefix,filelist) & logical.grep(suffix,filelist)])
     
 nofiles = length(quadfiles)
 
 if (nofiles==0) return('....There are NO files to convert....\n')

 fullresult=data.frame()

  # Get length of quadratname and prefix
 lenprefix = nchar(prefix)
 lenquad = nchar(allquad[1])
 
 for(i in 1:nofiles)
     {
      infile=quadfiles[i]

	  # Get quadrat and subquadrat names from file name
	  
	  # Find position of last slash
	  infilesplit = unlist(strsplit(infile,''))
	  if (length(grep('/',infilesplit))==0) locationslash=0    
	  else locationslash=max(grep('/',infilesplit))
	  
      # browser()
	  
	  startposition= locationslash+lenprefix
	  
	  quadrat = substr(infile,startposition+1,startposition+lenquad)
	  
	  lastchar =  gregexpr(suffix,tolower(infile),fixed=TRUE)
	  # lastchar =  gregexpr(suffix,tolower(infile))
      subquad = substr(infile,startposition+lenquad+1,lastchar[[1]]-1)
	  
      if(!is.null(debug)) if(quadrat==debug) browser()
	  
	  completefile=paste(path,infile,sep='')

      onemap=imageJ.to.lxly(textfile=completefile,lowerleft=corners[1],upperleft=corners[2],upperright=corners[3],lowerright=corners[4],
                            delim=delim,gridsize=gridsize)

      if(onemap$missing!=0) browser()
      if(dim(onemap$coord)[1]>0)
         {
          # Correct for subquadrat
          coord=SectionCorrection(onemap$coord,subquad,gridsize,subquadsuffix=subquadsuffix)
          coord$quadrat=I(quadrat)
       
          fullresult=rbind(fullresult,coord)
         }
             
      cat('Finished calculating coordinates for quadrat ', quadrat, '\n')
     }
  
 if(!is.null(outfile))
  {
   if (path=='.')  path=''
   output=pst(path,outfile)
   write.table(fullresult,file=output,quote=FALSE,sep='\t',row.names=FALSE,col.names=TRUE)
  }
  
 return(fullresult)
}

# </source>
# </function>


# <function>
# <name>
# SectionCorrection
# </name>
# <description>
# This function is called by fullplot.imageJ in the case that the maps are subquadrats to correct the coordinates and make them from 0 to 20 depending on
# what section of the map the subquadrat belongs to.
# </description>
# <arguments>
# <ul>
# <li> pts: the coordinates to be corrected
# <li> subquad: the subquadrat to be corrected
# <li> gridsize: size of each individual map
# <li> subquadratsuffix: the suffixes used to indicate what section of the map the subquadrat refers to. They should be named clockwise from the lower left.
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
SectionCorrection=function(pts,subquad,gridsize,subquadsuffix)
{
 if(subquad==subquadsuffix[2] | subquad==subquadsuffix[3]) pts$ly=pts$ly+gridsize[2]
 if(subquad==subquadsuffix[3] | subquad==subquadsuffix[4]) pts$lx=pts$lx+gridsize[1]
 
 return(pts)
}

# </source>
# </function>


# <function>
# <name>
# imageJ.to.lxly
# </name>
# <description>
# Convert map pixels from digitzing program to plot coordinates. Uses 2 functions from geometry.r:
# pts.to.interceptslope=function(pt1,pt2)
# perpendicular.distance=function(b,m,x,y) return(sqrt((y-m*x-b)^2/(1+m^2)))
# intersection.of.lines=function(b1,m1,b2,m2)
# </description>
# <arguments>
# <ul>
# <li> textfile: the complete name of the textfile to convert, including the path
# <li> lowerleft, upperleft, upperright, lowerright: the tags in each of the map files with the calibrated corners
# <li> delim: the delimiter used to separate the fields in the map files
# <li> gridsize: size of each individual map
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
imageJ.to.lxly=function(textfile=samplemapfile,delim=",",lowerleft="P2",upperleft="P1",upperright="P4",lowerright="P3",gridsize=c(20,20))
{
 data=read.table(textfile,sep=delim,as.is=TRUE,header=TRUE)
 
 lowerlabel=tolower(data$label)
 lowerleft=tolower(lowerleft)
 upperleft=tolower(upperleft)
 upperright=tolower(upperright)
 lowerright=tolower(lowerright)
 
 ptLL=subset(data,lowerlabel==lowerleft,select=c("x","y"))
 ptUL=subset(data,lowerlabel==upperleft,select=c("x","y"))
 ptUR=subset(data,lowerlabel==upperright,select=c("x","y"))
 ptLR=subset(data,lowerlabel==lowerright,select=c("x","y"))
 
 bottom=as.matrix(pts.to.interceptslope(ptLL,ptLR))
 left=as.matrix(pts.to.interceptslope(ptLL,ptUL))
 top=as.matrix(pts.to.interceptslope(ptUL,ptUR))
 right=as.matrix(pts.to.interceptslope(ptUR,ptLR))
 
 trees=subset(data,lowerlabel!=lowerleft & lowerlabel!=upperleft & lowerlabel!=upperright & lowerlabel!=lowerright,select=c("x","y","label"))
 coord=trees[,c("x","y")]
 labels=trees$label
 if(dim(coord)[1]==0) return(list(coord=data.frame(tag=I(trees$label),lx=coord$x,ly=coord$y),orig=data,lefterror=NA,bottomerror=NA,missing=0))
 
 leftresult=t(apply(coord,1,distance.to.side,target=left,side1=top,side2=bottom))
 rightresult=t(apply(coord,1,distance.to.side,target=right,side1=top,side2=bottom))
 topresult=t(apply(coord,1,distance.to.side,target=top,side1=left,side2=right))
 bottomresult=t(apply(coord,1,distance.to.side,target=bottom,side1=left,side2=right))
 
 mPerPix=leftresult[,1]+rightresult[,1]
 leftdist=gridsize[1]*leftresult[,1]/mPerPix
 lefterror=gridsize[1]*sqrt(2*leftresult[,2]^2+rightresult[,2]^2)

 mPerPix=topresult[,1]+bottomresult[,1]
 bottomdist=gridsize[2]*bottomresult[,1]/mPerPix
 bottomerror=gridsize[2]*sqrt(2*bottomresult[,2]^2+topresult[,2]^2)
  
 missing=max(c(length(leftdist)-dim(coord)[1]),c(length(bottomdist)-dim(coord)[1]))
 
 return(list(coord=data.frame(tag=I(trees$label),lx=leftdist,ly=bottomdist),orig=data,
             lefterror=mean(lefterror),bottomerror=mean(bottomerror),missing=missing))
}


# </source>
# </function>


# <function>
# <name>
# distance.to.side
# </name>
# <description>
# Called from imageJ.to.lxly where for each point, distance to each side is calculated in 3 different ways: perpendicular, parallel to one adjacent side, 
# and parallel to other adjacent. These 3 distances are averaged, and the discrepancy used as a measure of error, taken as the CV of the three measures.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
distance.to.side=function(pt,target,side1,side2)
{
 perp=perpendicular.distance(b=target[1],m=target[2],x=pt[1],y=pt[2])
 
 parallel=drop(as.matrix(parallel.line(b=side1[1],m=side1[2],x=pt[1],y=pt[2])))
 intersect=intersection.of.lines(b1=parallel[1],m1=parallel[2],b2=target[1],m2=target[2])
 dist1=xydist(x1=pt[1],y1=pt[2],x2=intersect[1],y2=intersect[2])

 parallel=drop(as.matrix(parallel.line(b=side2[1],m=side2[2],x=pt[1],y=pt[2])))
 intersect=intersection.of.lines(b1=parallel[1],m1=parallel[2],b2=target[1],m2=target[2])
 dist2=xydist(x1=pt[1],y1=pt[2],x2=intersect[1],y2=intersect[2])
 
 m=mean(c(perp,dist1,dist2))
 cv=sd(c(perp,dist1,dist2))/m
 
 return(c(m,cv))
}

# </source>
# </function>
