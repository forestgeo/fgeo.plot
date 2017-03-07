# <function>
# <name>
## spparea.sq
# </name>
# <description>
# Function for calculating the number of species in replicated, randomly-placed
# quadrats of various areas. The variable size refers to the dimension (side)
# of the square, and can be a vector; replicates is the
# number of random draws per dimension. Full plot data are submitted as censdata.
# Species are counted as the number of unique values of sp in the R Analytical Table; unidentified species are not counted,
# based on a default unidennames; see the function unidentified.species in utilitiesCTFS.r.<br><br>
# The return value is a list of two components. The first is a table giving the mean number (and SD) of individuals and species
# in all quadrat sizes submitted. The second is a table giving the number of individuals and species in every random quadrat created.
# In addition, a graph of species vs. individuals in every quadrat is created as the program runs. <br><br>
# This can also be used to calculate genus- or family-area curves with use of the spcolumn argument. The censdata table must have
# a new column added, for example the genus for every record, then spcolumn can be set to 'genus'. <br><br>
# Note: randomly-placed quadrats produce statistically preferable species-area curves than checkerboards of non-overlapping quadrats. If
# required, though, the function abundanceperquad() in abundance.r offers a fast way to count the number of species in checkerboard-type
# quadrats of different sizes.
# <arguments>
# <ul>
# <li> censdata: one R Analytical Table, either full or stem
# <li> spcolumn: name of the column in the table having the species; defaults to 'sp', but can be set to 'genus' for 'family' if desired
# <li> size: a vector of quadrat sizes, referring to the x-dimension of a rectangular quadrat
# <li> rectdim: the ratio of y to x dimensions of the rectangles; rectdim=1 (the default) for squares
# <li> mindbh: the minimum dbh included
# <li> plotdim: the x and y dimensions of the entire plot
# <li> replicates: the number of random quadrats to create, of each size
# <li> unidennames: a vector of species names that should not be included in species counts (see the function unidentified.species()
# </ul>
# </arguments>
# <sample>
# speciesPerArea=spparea.sq(bci.full6,size=c(10,20,50),mindbh=10,plotdim=c(1000,500),replicates=5,unidennames=c('unid'))
# rowmatch=match(bci.full6$sp,bci.spptable$sp)
# bci.full6$Genus=bci.spptable$Genus[rowmatch]
# genusPerArea=spparea.sq(bci.full6,spcolumn='Genus',size=c(10,20,50),mindbh=10,plotdim=c(1000,500),replicates=5,unidennames=c('unid'))
# </sample>
# <source>
spparea.sq=function(censdata,spcolumn='sp',size,rectdim=1,mindbh=NULL,plotdim=c(1000,500),replicates=10,unidennames=c("**","UNID","uniden","UNIDEN"))
{
 toolong=size[size>plotdim[1]]
 size=size[size*rectdim<=plotdim[2]]

 if(length(toolong)>0)
   {
    cat("size ", min(toolong))
    if(length(toolong)>1) cat(", ", max(toolong))
    cat (" too big for plot\n")
   }

 if(is.null(mindbh))
    censdata=subset(censdata,status=="A" & !unidentified.species(spcolumn,exactstr=unidennames))
 else
    censdata=subset(censdata,status=="A" & dbh>=mindbh & !unidentified.species(spcolumn,exactstr=unidennames))

 allsize=selectrandomquad(size,rectdim,replicates,plotdim)
 noquad=dim(allsize)[1]
 
 spp=ind=numeric()
 for(i in 1:noquad)
  {
   data=subset(censdata,gx>=allsize$xlo[i] & gx<allsize$xhi[i] & gy>=allsize$ylo[i] & gy<allsize$yhi[i])
 
   spp[i]=length(unique(data[,spcolumn]))
   ind[i]=length(data[,spcolumn])
 
   if(i==1) plot(allsize$area[i],spp[i],pch=16,ylim=c(1,1200),xlim=c(.01,60),log="xy")
   else points(allsize$area[i],spp[i],pch=16)
  }

 full=data.frame(area=allsize$area,taxa=spp,ind)
 taxa=tapply(full$taxa,full$area,mean)
 SDtaxa=tapply(full$taxa,full$area,sd)
 indiv=tapply(full$ind,full$area,mean)
 SDindiv=tapply(full$ind,full$area,sd)
 area=tapply(full$area,full$area,mean)
 
 no.area=length(size)
 taxa[no.area+1]=length(unique(censdata[,spcolumn]))
 indiv[no.area+1]=length(censdata[,spcolumn])
 SDtaxa[no.area+1]=SDindiv[no.area+1]=NA
 area[no.area+1]=plotdim[1]*plotdim[2]/1e4
 xdim=c(size,plotdim[1])
 ydim=c(size*rectdim,plotdim[2])
 
 return(list(spparea=data.frame(xdim,ydim,area,indiv,SDindiv,taxa,SDtaxa),full=full))
}
# </source>
# </function>



# <function>
# <name>
## selectrandomquad
# </name>
# <description>
# Draws rectangular quadrats in a plot at random. Returns a dataframe of coordinates defining the corners of all the quadrats. 
# The variable size is a vector of dimensions; shape allows a rectangle to be drawn; rep is the
# number of replicated random quadrats per dimension submitted. <br><br>

# Quadrats are chosen simply: a point is chosen by randomly drawing one x and one y coordinates to serve as the lower-left corner.
# The x is chosen from a uniform distribution between 0 and plotdim[1]-size; y is chosen similarly. <br><br>

# All 3 algorithms for creating random quadrats under sample the plot corners. See selectrandomquad2() and selectrandomquad3(), alternative
# intended to overcome the bias (but neither does). 
# </description>
# <arguments>
# <ul>
# <li> censdata: one R Analytical Table, either full or stem
# <li> size: a vector of quadrat sizes, referring to the x-dimension of a rectangular quadrat
# <li> shape: the ratio of y to x dimensions of the rectangles; rectdim=1 (the default) for squares
# <li> plotdim: the x and y dimensions of the entire plot
# <li> rep: the number of random quadrats to create, of each size
# <li> graphit: whether to graph the locations of the chosen quadrats on a plot map
# </arguments>
# <sample>
# 
# </sample>
# <source>
selectrandomquad=function(size,shape,rep,plotdim=c(1000,500),graphit=FALSE)
{
 noquad=length(size)
 allsize.x=rep(size,each=rep)
 allsize.y=rep(size*shape,each=rep)
 total=length(allsize.x)
 
 xlo=runif(total,min=0,max=plotdim[1]-allsize.x)
 ylo=runif(total,min=0,max=plotdim[2]-allsize.y)
 xhi=xlo+allsize.x
 yhi=ylo+allsize.y

 result=data.frame(xlo,ylo,xhi,yhi)
 if(graphit) graph.quadrats(result)

 return(data.frame(xlo,ylo,xhi,yhi,area=(allsize.x*allsize.y)/1e4))
}
# </source>
# </function>



# <function>
# <name>
# selectrandomquad3
# </name>
# <description>
# Creates randomly drawn quadrats, using same arguments and producing same return value as selectrandomquad, but using a different algorithm.
# The lower left x, y coordinates are drawn at random from a range extending outside the plot dimensions, then quadrats are used only if
# all 4 corners fall inside the plot.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
selectrandomquad3=function(size,shape,rep,plotdim=c(1000,500),graphit=FALSE)
{
 noquad=length(size)
 allsize.x=rep(size,each=rep)
 allsize.y=rep(size*shape,each=rep)
 total=length(allsize.x)
 
 xlo=runif(total,min=0-2*allsize.x,max=plotdim[1]+2*allsize.x)
 ylo=runif(total,min=0-2*allsize.y,max=plotdim[2]+2*allsize.y)
 xhi=xlo+allsize.x
 yhi=ylo+allsize.y

 coord=c(0,0,plotdim[1],plotdim[2])
 good.lowerleft=are.ptsinside(pts=data.frame(xlo,ylo),coord=coord)
 good.upperleft=are.ptsinside(pts=data.frame(xlo,yhi),coord=coord)
 good.upperright=are.ptsinside(pts=data.frame(xhi,ylo),coord=coord)
 good.lowerright=are.ptsinside(pts=data.frame(xhi,yhi),coord=coord)
 good = good.lowerleft & good.upperleft & good.upperright & good.lowerright

 result=data.frame(xlo,ylo,xhi,yhi)
 if(graphit) graph.quadrats(result,plotdim=plotdim+2*size)

 result=data.frame(xlo,ylo,xhi,yhi)[good,]
 if(graphit) graph.quadrats(result[good,],clr="red",add=TRUE)

 return(data.frame(xlo,ylo,xhi,yhi,area=(allsize.x*allsize.y)/1e4)[good,])
}
# </source>
# </function>




# <function>
# <name>
## selectrandomquad2
# </name>
# <description>
# Creates randomly drawn quadrats, using same arguments and producing same return value as selectrandomquad, but using a different algorithm 
# aimed at capturing corners. The result, however, is not to capture corners any better than selectrandomquad() does.<br><br>

# Imagine a line running vertically at x=0 from y=0 to y=plotdim[2]-size, then continues at x=1 from 0 to plotdim[2]-size,
# etc. It's wrapping analogous to the way quadrat indices wrap (see gxgy.to.index in quadfunc.r). 
# This line has length (plotdim[1]-size)*(plotdim[2]-size).
# Draw a random number on that line, and place the lower left corner of random square at that point. <br><br>

# A position x on the line is converted to plot coordinates gx, gy using function index.to.gxgy with grid=1
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
selectrandomquad2=function(size,rep,plotdim=c(1000,500),graphit=FALSE)
{
 linelen=(plotdim[1]-size)*(plotdim[2]-size)
 r=runif(rep,min=0,max=linelen)

 coord=index.to.gxgy(r,grid=1,plotdim=plotdim-size)

 xlo=coord$gx
 ylo=coord$gy
 xhi=xlo+size
 yhi=ylo+size

 result=data.frame(xlo,ylo,xhi,yhi)
 if(graphit) graph.quadrats(result)

 return(data.frame(xlo,ylo,xhi,yhi,area=(size*size)/1e4))
}
# </source>
# </function>



# <function>
# <name>
## graph.quadrats
# </name>
# <description>
# Make a graph of a series of quadrats whose corners are given by the rows of coord: xlo, ylo, xhi, yhi. This is used in illustrating
# randomly selected quadrats. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>

# </sample>
# <source>
graph.quadrats=function(coord,plotdim=c(1000,500),clr="black",add=FALSE)
{
 aspect=plotdim[2]/plotdim[1]
 oldpar=par(pin=c(5,5*aspect))

 xlo=coord[,1]
 ylo=coord[,2]
 xhi=coord[,3]
 yhi=coord[,4]

 if(!add) plot(xlo,ylo,xlim=c(0,plotdim[1]),ylim=c(0,plotdim[2]),pch=16,cex=.5,col="white")
 segments(xlo,ylo,xlo,yhi,col=clr)
 segments(xlo,yhi,xhi,yhi,col=clr)
 segments(xhi,yhi,xhi,ylo,col=clr)
 segments(xhi,ylo,xlo,ylo,col=clr)

 par(oldpar)
}
# </source>
# </function>


# <function>
# <name>
## coverage.diag
# </name>
# <description>
# Draws a diagonal across a plot, from lower left to upper right (if slope==1), upper left to lower right (if slope==-1),
# or straight across the middle (if slope==0).
# and determines for every point along the diagonal what fraction of a series of quadrats it is inside.
# The quadrats are defined by their four corners in randomquads: xlo, ylo, xhi, yhi.<br><br>
# This is only used in testing how well random quadrat draws include corners and edges of a plot.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
coverage.diag=function(randomquads,slope=1,plotdim=c(1000,500),graphit=FALSE)
{
 x=0:plotdim[1]
 if(slope==1) y=x*(plotdim[2]/plotdim[1])
 else if(slope==(-1)) y=plotdim[2]-x*(plotdim[2]/plotdim[1])
 else if(slope==0) y=rep(plotdim[2]/2,length(x))
 else return("Enter slope of 1, 0, of -1\n")

 pts=data.frame(x,y)

 covered=apply(pts,1,ispt.inside,coord=randomquads)
 if(graphit) plot(x,covered,type="l")

 return(covered)
}
# </source>
# </function>



