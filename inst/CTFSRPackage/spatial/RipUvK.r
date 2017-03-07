# <function>
# <name>
# RipUvK
# </name>
# <description>
# Ripley's K using splancs. Computes the univariate Ripley's K for a list of species (all species at a site 
# is the default). Also calculates the O-ring statistic, or density (per area) of
# conspecifics in a series of annuli defined by rseq, and the omega statistic,
# which is O-ring divided by overall density.<br><br>

# Works through all the elements of a split datafile, each of which must have gx, gy
# coordinates, a status code, and dbh. This would usually be species, but could
# be any other division of the data. RipUvK always performs calculations on each element of the
# splitdata list submitted.<br><br>

# If the data have no status field, all records are included. If there is a status field, only "A"
# values are included. If mindbh is set to 0, all records are
# included and no dbh field is consulted. But if mindbh is set, there must be a dbh field, and records
# are excluded if smaller.<br><br>

# The output is a list of 5 components.
# <ul>
# <li> K is the Ripley's K object produced by the khat function in the splancs package; please see help for that package for details.
# <li> O is the mean density of neighbors in each successive distance interval; it is a list of vectors, one per species, each vector of length matching length of rseq.
# <li> omega is the omega-statistic, or O in each successive distance interval divided the plot-wide density; density is expressed per 10,000 square distance units, which means per hectare if units are meters.
# <li> abund is a vector of species abundances, including individuals used in the calculation.
# <li> midpts is a vector of the same length as rseq, giving the midpoint of each distance interval, for graphing purposes.
# </ul>

# </description>
# <arguments>
# <ul>
# <li> splitdata: A complete CTFS plot dataset comprised of a list, each element having a dataframe for one species. There must be columns for x and y coordinates; the names of those two columns are passed as arguments (default is the CTFS standard gx, gy). 
# <li> plotdim: The x and y dimensions of the plot. 
# <li> rseq: The distances defining intervals in which Ripley's statistic is calculated.
# <li> mindbh: the minimum dbh to include in results.
# <li> xcol and ycol: the names of the columns holding x and y coordinates.
# <li> debug: TRUE or FALSE, calls browser during execution to pause program; only set TRUE to debug.
# <li> show: creates a graph to show one omega value for each species, as a way to track progress.
# </ul>
# </arguments>
# <sample>
# If a split database has been created by split.data and stored, CTFSplot will load it.<br>
# CTFSplot("bci",6,type="split")<br>
# Otherwise start with a standard R Analytical Table (could be stem also):<br>
# CTFSplot("bci",6,type="full")<br>
# bci.split6=split.data(bci.full6)<br>
# A quick test, run on only the first 10 species:<br>
# rip=RipUvK(splitdata=bci.split6[1:10],plotdim=c(1000,500),rseq=c(10,20,30,40,50,60),mindbh=10,xcol="gx",ycol="gy")<br>
# All the species (takes several minutes):<br>
# rip=RipUvK(splitdata=bci.split6,plotdim=c(1000,500),rseq=c(10,20,30,40,50,60),mindbh=10,xcol="gx",ycol="gy")<br>
# str(rip$K[[1]])<br>
# plot(rip$midpts,rip$omega[2,],ylim=c(0,10))
# </sample>
# <source>
RipUvK=function(splitdata,plotdim=c(1000,500),rseq=c(5,10,20,30,40,50),mindbh=10,xcol="gx",ycol="gy",debug=FALSE,show=FALSE)
{
  spp.names <- names(splitdata)
  plotarea=plotdim[1]*plotdim[2]/1e4
  
  omega=matrix(nrow=length(spp.names),ncol=length(rseq))
  rip.list=Ovalue=list()
  abund=numeric()
  
  poly=spoints(c(0,0,plotdim[1],0,plotdim[1],plotdim[2],0,plotdim[2]))
# calculate K and the (number of conspecific) individuals for each tree within 
# distances of rseq

  sppcounter=1
  for (i in 1 : length(spp.names)) 
  {
    if(mindbh==0) size=rep(0,dim(splitdata[[i]])[1])
    else size=splitdata[[i]]$dbh

    if(is.null(splitdata[[i]]$status)) alivestatus=rep("A",dim(splitdata[[i]])[1])
    else alivestatus=splitdata[[i]]$status

    gx=(splitdata[[i]][,xcol])
    gy=(splitdata[[i]][,ycol])
    include=which(alivestatus=="A" & size>=mindbh & gx>=0 & gx<plotdim[1] & gy>=0 & gy<plotdim[2])
    inc=(alivestatus=="A" & size>=mindbh & gx>=0 & gx<plotdim[1] & gy>=0 & gy<plotdim[2])
    gx=gx[include]
    gy=gy[include]
    abund[i]=length(gx)
    if(debug) browser()
    
    if(abund[i]>1)
     {
      sppi.pts <- spoints(rbind(gx,gy),length(gx))
      rip.list[[i]] <- khat(as.points(sppi.pts),poly=poly,rseq,newstyle=TRUE)
  
      ringarea=Annuli(data.frame(gx,gy),rseq,plotdim)

      Ovalue[[i]]=colSums(rip.list[[i]]$counts)/(ringarea$total/1e4)
      rip.list[[i]]$area=ringarea$each

      lambda=abund[i]/plotarea
      if(debug) browser()
      omega[i,]=Ovalue[[i]]/lambda

	  if(show)
	     {
	      if(sppcounter==1) plot(i,omega[i,1],log="y",xlim=c(0,1200),ylim=c(.1,60))
	      else points(i,omega[i,1])
         }
        
      sppcounter=sppcounter+1
     }
    else rip.list[[i]]=Ovalue[[i]]=NA
  }

  names(Ovalue)= names(rip.list)=names(abund)=rownames(omega)=spp.names
  
  nopts=length(rseq)
  midpts=c(0,rseq[-nopts])+diff(c(0,rseq))/2

  return(list(K=rip.list,O=Ovalue,omega=data.frame(omega),abund=abund,midpts=midpts))
}
# </source>
# </function>

# <function>
# <name>
# Annuli
# </name>
# <description>
# Calculates the total area in a series of annuli, summed over all individuals in the
# submitted dataset. Calls CalcRingArea to do the calculations for the area within
# a distance r of every individual, then subtracts successive areas to get 
# the area within annuli. This is ordinarily only used as a subroutine of RipUvK.
# </description>
# <arguments>
# <ul>
# <li> spdata: A single dataframe with x-y coordinates of individuals. 
# <li> r: A vector of distances defining the successive annuli.
# <li> plotdim: The x and y dimensions of the plot in which the individuals are mapped. 
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
Annuli=function(spdata,r,plotdim)
{
 TotalAreaPerRing=numeric()
 AreaPerCircle=matrix(nrow=dim(spdata)[1],ncol=length(r)+1)

 TotalAreaPerRing[1]=0
 AreaPerCircle[,1]=0

 for(i in 1:length(r)) 
  {
   ringarea=CalcRingArea(spdata,r[i],plotdim)

   TotalAreaPerRing[i+1]=ringarea$total
   AreaPerCircle[,i+1]=ringarea$each
  }

 AreaPerRing=t(apply(AreaPerCircle,1,diff))

 return(list(total=diff(TotalAreaPerRing),each=AreaPerRing))
}
# </source>
# </function>
# 


# <function>
# <name>
# CalcRingArea
# </name>
# <description>
# Given a dataframe that includes gx, gy coordinates, and a single value radius,
# finds the area within radius from all points (summed). In the 
# absence of edge corrections, this would be simply N*pi*radius^2, where N is the
# number of points. But this uses the function partialcirclearea to do the edge
# correction. This is ordinarily only used as a subroutine for RipUvK.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
CalcRingArea=function(data,radius,plotdim)
{
 nopts=dim(data)[1]
 internalArea=numeric()
 
 for(i in 1:nopts)
  {
   xdist=data$gx[i]
   if(plotdim[1]-data$gx[i]<xdist) xdist=plotdim[1]-data$gx[i]

   internalArea[i]=partialcirclearea(radius,xdist,data$gy[i],plotdim[2]-data$gy[i])
  }

 return(list(total=sum(internalArea),each=internalArea))
} 
# </source>
# </function>
# 

   

# <function>
# <name>
# circlearea
# </name>
# <description>
# Simply returns area of a circle of radius r.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
circlearea=function(r) { return(pi*r^2) }
# </source>
# </function>


# <function>
# <name>
# partialcirclearea
# </name>
# <description>
# Calculates the area of a circle of radius r that is inside a rectangular plot.  
# The distance from the circle center to plot edges are c2, cy1, cy2, where cy1 is the shortest
# distance to a y-boundary and cy3 the longest, while c2 is the shortest x distance.
# The longest x distance is not needed. <br><br>

# This will not work if the longest x distance < radius of the
# circle.  The greatest radius allowed is thus half the x dimension of the plot, generally
# 250 or 500 m.<br><br>

# Ordinarily only used as a subroutine of RipUvK.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
partialcirclearea=function(r, c2, cy1, cy2)
{
 if(cy1<=cy2) { c1=cy1; c3=cy2 }
 else { c1=cy2; c3=cy1 }

 if(r>c1) { alpha1=acos(c1/r); y1=sqrt(r*r-c1*c1) }
 if(r>c2) { alpha2=acos(c2/r); y2=sqrt(r*r-c2*c2) }
 if(r>c3) { alpha3=acos(c3/r); y3=sqrt(r*r-c3*c3) }

 cornerdist1=sqrt(c1*c1+c2*c2)
 cornerdist3=sqrt(c2*c2+c3*c3)

 if(r<=c1 && r<=c2) 
   area=circlearea(r)

 else if(r>c1 && r<=c2 && r<=c3)
   area = r*r*(pi-alpha1) + c1*y1

 else if(r<=c1 && r>c2 && r<=c3)
   area = r*r*(pi-alpha2) + c2*y2

 else if(r>c1 && r>c2 && r<=c3)
   {
    if(r>cornerdist1)
      area = r*r*(3*pi/4-alpha1/2-alpha2/2)+c1*c2+(c1*y1+c2*y2)/2 
    else 
      area = r*r*(pi-alpha1-alpha2)+c1*y1+c2*y2 
   }

 else if(r<=c2 && r>c1 && r>c3)
   area = r*r*(pi-alpha1-alpha3)+c1*y1+c3*y3 

 else if(r>c2 && r>c1 && r>c3)
   {
    if(r>cornerdist3)
      area = r*r*(pi-alpha1-alpha3)/2+c1*c2+c2*c3+(c1*y1+c3*y3)/2 
    else if(r>cornerdist1 && r<=cornerdist3)
      area = r*r*(3*pi/4-alpha1/2-alpha2/2-alpha3)+(c1*y1+c2*y2)/2+c3*y3+c1*c2 
    else if(r<=cornerdist1 && r<=cornerdist3)
      area = r*r*(pi-alpha1-alpha2-alpha3)+c1*y1+c2*y2+c3*y3 
   }

 return(area)
}
# </source>
# </function>
# 
