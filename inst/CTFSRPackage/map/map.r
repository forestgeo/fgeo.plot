# <function>
# <name>
# map
# </name>
# <description>
# Function to draw map of one or more species from one plot. Must give the complete
# plot dataset, in split format (list of one dataframe per species), and one or more
# species code. Other arguments are optional, however, many defaults can be adjusted
# to get a good map on the screen or exported. This calls the functions maptopo() and map1species()
# for the actual mapping. <br><br>

# A common problem is graph size, leading to an error message "Plot region too large". This happens if the export="no" option
# is used with the option plotside too large for the default graph size (usually 7 inches). If you use the option export="Windows", 
# "Mac", or "Unix", then height and width can be set with arguments ht, wd, and plotside can be as large as you please. 
# </description>
# <arguments>
# <ul>
# <li> splitdatafile: A full plot dataset in list format.
# <li> species: One or more species codes to be mapped.
# <li> spplist: A table with species codes and Latin names can be submitted as well, so that the full genus-species is added to plot. This must have species codes as row names. It should be the CTFS R format species table (eg, bci.spptable).
# <li> It can be set to NULL if not available, then only the species code (as submitted) appears on the map.
# <li> xrange and yrange: Minimum and maximum x coordinates and y coordinates to map. Allows a portion of plot to be drawn. Defaults to the entire plot.
# <li> plotdim: The x and y dimensions of the plot. This is used often in R package. Note it assumes the starting coordinates are zero. If they are not, then xrange and yrange must be used.
# <li> elevdata: Elevation data can be submitted, then a topo map is overlaid. Elevation data must be submitted as a matrix (as described in readelevdata in utilities.r).
# <li> cutoff: Diameter breaks for changing size of plotting points.
# <li> size: The size of plotting points, to match the number of diameter breaks. If NULL, a default set is assigned. This can require fiddling, as big points do not work for really abundant species, and small points for rare species.
# <li> deadtree: Set NULL to map all trees, alive and dead; TRUE for dead only, FALSE for live only (relies on status in the R table).
# <li> maintitle: A title to appear at the top of the page, above the species name.
# <li> titlepos: The position to place the title. The default is above the center of the plot, higher than the species name. It may require some fiddling on different screens to get it the right distance above.
# <li> clrs: A vector of color names, one for each species. If set to NULL, default values are assigned. See bw.
# <li> bw: If TRUE, only black, white, and grays are used.
# <li> bgcolor: The background color. Defaults to white. For presentation exports, try bgcolor="transparent".
# <li> symbols: A vector of symbols, one per species. Can be anything accepted by R for pch (plot character in the function plot()). If NULL, defaults are assigned.
# <li> addlegend, legpos, legsize: For the species name, whether to include, where to place, and font size. Try the defaults first before fiddling, or just set addlegend=FALSE to remove.
# <li> ht, wd, plotside: These are the height and width of the overall graph, and the vertical dimension (inches) of the map. The default work for pdf export or mapping to the screen, and ht and wd are inches. But if export is png, jpg, emf, height and width are pixels and need to be 500-1000.
# <li> labsize: Size of axis labels.
# <li> bty: Type of box to appear around species name. The default, 'n', means no box; set to 'o' to see the box.
# <li> axspos: Distance between axis numbers and axis.
# <li> topoint: Interval for topolines, if elevdata are submitted.
# <li> topoclr: Color of topolines.
# <li> export: See function define.graphwindow. 
# <li> filepath: The folder to which map will be exported.
# <li> outfile: The name of the file to export to.
# </ul>
# </arguments>
# <sample>
# If you have saved split formatted data: <br> CTFSplot(plot='bci',census=6,type='split')<br>
# If you have not saved: <br> 
# CTFSplot(plot='bci',census=6)
# <br>
# bci.split6=split.data(censdata=bci.full6,splitcol='sp')
# <br>
# nospp=length(species)
# <br>
# map(splitdatafile=bci.split6,species='poular')
# <br>
# map(splitdatafile=bci.split6,species='poular',export='pdf',filepath='~/data/maps/',outfile='Poulsenia1995map',plotside=10,ht=8.5,wd=11)
# <br>
# map(splitdatafile=bci.split6,species=c('guatdu','poular'))
# </sample>
# <source>
map=function(splitdatafile,species,spplist=NULL,plotdim=c(1000,500),xrange=c(0,plotdim[1]),yrange=c(0,plotdim[2]),elevdata=NULL,
             cutoff=c(10,100,300,3000),size=NULL,deadtree=FALSE,maintitle='',titlepos=c(plotdim[1]/2,1.32*plotdim[2]),clrs=NULL,
             bw=FALSE,bgcolor='white',symbols=NULL,xaxis=TRUE,yaxis=TRUE,addlegend=TRUE,legpos=c(plotdim[1]/2,1.16*plotdim[2]),legsize=0.75,
             labsize=1.15,axisdiv=100,bty='n',ht=6,wd=9,plotside=6,axspos=0.5,topoclr='gray80',topoint=0,
             export='no',filepath='',outfile=NULL)
{ 
 filename=get.filename(file=outfile,path=filepath,exp=export,species=species)
 define.graphwindow(exp=export,h=ht,w=wd,file=filename)
 if(export!="unix" & export!="no" & export!="mac" & export!="windows") on.exit(graphics.off())
  
 col=latin=character()
 symb=numeric()

 if(!is.null(spplist)) latin=spplist[species,]$Latin
 else latin=species
 
 nogrp=length(cutoff)-1
 if(is.null(size)) size=setsize(nogrp,dim(load.species(species[1],splitdatafile))[1])

 if(is.null(clrs))
  {
   if(!bw) col=c("blue","red","black","green","yellow","lightblue","orange")
   else col=c("black","black","gray80","gray80","gray30","gray30")
  }
 else col=clrs

 if(is.null(symbols)) 
  {
   if(bw)  symb=rep(16,6)
   else symb=c(16,1,16,1,16,1)
  }
 else symb=symbols

 topo=FALSE
 if(!is.null(elevdata)) topo=TRUE
 
 if(topo) 
  maptopo(elev=elevdata,plotdim=plotdim,xaxis=xaxis,yaxis=yaxis,interval=topoint,ht=ht,wd=wd,plotside=plotside,labelsize=labsize,
          axspos=axspos,bgcolor=bgcolor,clr=topoclr)
 # browser()
 for(i in 1:length(species))
  {
   if(species[i] %in% names(splitdatafile)) sppdata=load.species(species[i],splitdatafile)
   else sppdata=data.frame(gx=numeric(),gy=numeric(),dbh=numeric(),status=character())
   
   if(is.null(deadtree)) sppdata=subset(sppdata,gx>=0 & gy>=0 & gx<plotdim[1] & gy<plotdim[2])
   else if(deadtree) sppdata=subset(sppdata,gx>=0 & gy>=0 & gx<plotdim[1] & gy<plotdim[2] & status=='D') 
   else sppdata=subset(sppdata,gx>=0 & gy>=0 & gx<plotdim[1] & gy<plotdim[2] & status=='A') 

   if(i==1 & !topo)
     map1species(sppdata,color=col[i],symbol=symb[i],size=size,xrange=xrange,yrange=yrange,xaxis=xaxis,yaxis=yaxis,
                 cutoff=cutoff,plotdim=plotdim,plotside=plotside,bgcolor=bgcolor,axspos=axspos,labsize=labsize,axisdiv=axisdiv)
   else
     map1species(sppdata,color=col[i],symbol=symb[i],cutoff=cutoff,xrange=xrange,yrange=yrange,
                 size=size,plotdim=plotdim,plotside=plotside,add=TRUE)
  }

 # browser()
 oldpar=par(xpd=TRUE,font=3,xpd=NA)

 text(x=titlepos[1],y=titlepos[2],labels=maintitle,col=col[1],font=2,cex=1.25)

 if(addlegend) legend(x=legpos[1],y=legpos[2],legend=latin,xjust=0.5,col=col,text.col='black',bty=bty,pch=16,horiz=T,y.intersp=1.2,cex=legsize)
 par(oldpar)
 
 return(filename)
}
# </source>
# </function>

# 
# <function>
# <name>
# pdf.allplot
# </name>
# <description>
# Export a pdf with one or more species maps. If singlefile=TRUE, 
# all maps will be in one big pdf, otherwise, a pdf for every species is created. The file exported will be named with Map.pdf
# in the path name given. <br> 
# This calls the map() function for all species
# in the splitdata list; it is designed for all species from a plot (default is all species from the BCI plot).
# See description of map() for details. 
# </description>
# <arguments>
# </arguments>
# <sample>
# 
# </sample>
# <source>
pdf.allplot=function(splitdata=bci.split6,spplist=bci.spptable,elev=ctfs.elev$bci$mat,plotdim=c(1000,500),cutoff=c(10,100,300,3000),topoint=2,bw=FALSE,
                     topoclr='gray80',bty='o',plotside=9,h=8,w=10,size=c(.3,.45,.65),deadtree=FALSE,symbols=NULL,legsize=0.75,legpos=c(plotdim[1]/2,1.16*plotdim[2]),
                     export=TRUE,singlefile=TRUE,path='/home/condit/data/maps/bci/')
{
 on.exit(graphics.off())
 if(singlefile) pdf(file=pst(path,'allsppMap.pdf'),width=w,height=h)

 allspp=names(splitdata)
 for(i in 1:length(allspp))
  {
   if(!singlefile & export) pdf(file=pst(path,allspp[i],'Map.pdf'),width=10,height=8)
   map(splitdata,species=allspp[i],spplist=spplist,elevdata=elev,plotdim=plotdim,cutoff=cutoff,size=size,deadtree=deadtree,bw=bw,topoclr=topoclr,
       bty=bty,symbols=symbols,legsize=legsize,legpos=legpos,plotside=plotside,topoint=topoint,export="no")
   if(!singlefile & export) graphics.off()
  }
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# png.allplot
# </name>
# <description>
# Export species maps as png. There will be one for every species chosen. See pdf.allplot. 
# </description>
# <arguments>
# </arguments>
# <sample>
# 
# </sample>
# <source>
png.allplot=function(splitdata=bci.split6,spplist=plotspp$bci,elev=ctfs.elev$bci$mat,plotdim=c(1000,500),topoint=2,plotside=9,h=850,w=1100,
                     ptsizes=c(.3,.45,.65),dbhcut=c(10,100,300,3000),dead=FALSE,export=TRUE,path='/home/condit/data/maps/bci/')
{
 allspp=names(splitdata)
 for(i in 1:length(allspp))
   map(splitdata,species=allspp[i],spplist=spplist,elevdata=elev,plotdim=plotdim,cutoff=dbhcut,size=ptsizes,deadtree=dead,topoint=topoint,
       plotside=plotside,export='png',filepath=path,ht=h,wd=w,legsize=1)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# complete.plotmap
# </name>
# <description>
# This creates a map with every individual shown. The area to be mapped can be chosen, allowing maps of small areas.
# On top of the map of every individual, individual species can be overlaid in different colors. If nospp is set NULL, no species
# are added. If spnames is set NULL, then the most abundant species in the plot are chosen, up to the number nospp. 
# </description>
# <arguments>
# <ul>
# <li>cns: a full census dataset (all species)
# <li>spnames: names of species to map, using the mnemonic in the R tables
# <li>mindbh: smallest dbh to include
# <li>export: set to 'no' to graph to screen, 'pdf' to export to pdf (see define.graphwindow function)
# <li>nospp: number of species to overlay; can be NULL or 0 for none
# <li>plotdim: x and y plot dimensions
# <li>clrlist: colors to use for the species to be overlaid
# <li>ptsize: size of points, the first used for the background of all species, the second for the individual species
# <li>xrange: minimum and maximum x coordinates of area graphed
# <li>yrange: minimum and maximum y coordinates of area graphed
# <li>wd: graph width; see map() function
# <li>ht: graph height, same units as wd
# <li>side: the side in inches of the graph; see map()
# <li>filepath: folder to save output
# <li>outfile: filename for output
# </ul>
# </arguments>
# <sample>
# 
# </sample>
# <source>
complete.plotmap=function(cns=bci.full6,spnames=NULL, mindbh=10,export='no',nospp=3,plotdim=c(1000,500),clrlist=c('blue','green','red','yellow','gray'),
                          ptsize=c(.45,0.3),xrange=c(0,100),yrange=c(0,100),wd=1100,ht=850,side=6,labsize=1.75,axisdiv=10,
                          filepath='/home/condit/data/maps/',outfile='fullplotmap')
{
 filename=get.filename(file=outfile,path=filepath,exp=export,species="FullPlot")
 define.graphwindow(exp=export,h=ht,w=wd,file=filename)
 if(export!="unix" & export!="no" & export!="mac" & export!="windows") on.exit(graphics.off())
  
 alive=subset(cns,status=='A' & dbh>=mindbh & insideRectangle(gx,gy,xrange,yrange))
 N=table(alive$sp)
 if(is.null(spnames)) spnames=names(sort(N,decreasing=TRUE))[1:nospp]
 map1species(alive,color='black',plotdim=plotdim,plotside=side,cutoff=c(mindbh,10000),size=ptsize[2],
             xrange=xrange,yrange=yrange,axisdiv=axisdiv,labsize=labsize)

 if(is.null(nospp)) return('')
 if(nospp==0) return('')
 
 for(i in 1:nospp)
   {
    onespdata=subset(alive,sp==spnames[i])
    map1species(onespdata,color=clrlist[i],cutoff=c(mindbh,10000),size=ptsize[1],add=TRUE)
   }
}
# </source>
# </function>
# 
# 
# 
# 

# <function>
# <name>
# define.graphwindow
# </name>
# <description>
# This defines an export device for a graph, based on the argument export. The default, export='no', does nothing, so
# the next graph uses the default R device. With this option, the size of the graph cannot have been set larger than
# the default size, which is usually 7 inches. If export="unix", a new graphics window in unix, size h and width w, will be opened
# allowing you to alter the size of the output. To do the same in Windows, set export="windows", or for Mac, export="mac". 
# To export the graph to a file, export can be "pdf", "png", "jpg", "bmp", "emf". In all cases, it is quoted. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
define.graphwindow=function(export="no",w,h,file)
{
 if(export=="unix") X11(width=w,height=h)
 else if(export=="windows") win.graph(width=w,height=h)
 else if(export=="mac") quartz(width=w,height=h)
 else if(export=="bmp") bmp(file,width=w,height=h)
 else if(export=="jpg") jpeg(file,width=w,height=h,quality=100)
 else if(export=="emf") win.metafile(file,width=w,height=h)
 else if(export=="pdf") pdf(file,width=w,height=h)
 else if(export=="png") png(file,width=w,height=h)
}
# </source>
# </function>
# 

# <function>
# <name>
# get.filename
# </name>
# <description>
# This sets a name for outputting a map (or any graph) to a file. The argument file may be NULL, then the argument species is used
# to name the file, or if there are more than one species, the word multispp is used. The argument exp is the export type and becomes
# the extension (ie, .pdf). By default, type assumes a Map, but it can be set otherwise. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
get.filename=function(file,path,exp,species,type="Map.")
{
 if(is.null(file))
  {
   if(length(species)==1) filename=pst(path,species,type,exp)
   else filename=pst(path,"multispp",type,exp)
  }
 else filename=pst(path,file,".",exp)

 return(filename)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# maptopo
# </name>
# <description>
# Draws contours on a plot map. Elevmat has elevation data in matrix form; plot
# dimensions are not needed, instead it calculates them from elevmat and the
# gridsize. If add=TRUE, they are added to an existing map. If new=TRUE, a new screen window is created.
# This is called by map(), and see the description of map() for the other arguments. It can be used
# on its own though. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
maptopo=function(elevmat,plotdim,add=FALSE,new=FALSE,export="no",interval=0,xrange=NULL,yrange=NULL,xaxis=TRUE,yaxis=TRUE,
                 axspos=0.5,labelsize=1.15,maintitle="",ht=6,wd=9,plotside=4.5,filepath="",clr='black',bgcolor="transparent")
 {    
  if(export!="no") on.exit(graphics.off())

  if(new) X11(width=wd,height=ht)
  if(export=="pdf") 
   {
    filename=pst(filepath,"topomap.pdf")
    pdf(filename,width=wd,height=ht)
   }

  lowelev=min(elevmat)
  hielev=max(elevmat)
  r.elev=hielev-lowelev

  xdim=plotdim[1]
  ydim=plotdim[2]
  aspect=xdim/ydim

  graphsize=c(plotside,plotside/aspect)
  if(!add) oldpar=par(cex=1.7,col=clr,pin=graphsize,bg=bgcolor,mgp=c(3,axspos,0))

  if(interval==0)
   {
    if(r.elev>100) interval=5
    else interval=2
   }

  if(is.null(xrange)) xrange=c(0,plotdim[1])
  if(is.null(yrange)) yrange=c(0,plotdim[2])

  contour(x=seq(0,xdim,len=dim(elevmat)[2]),y=seq(0,ydim,len=dim(elevmat)[1]),t(elevmat),levels=seq(lowelev,hielev,by=interval),xlim=xrange,ylim=yrange,
                add=add,drawlabels=FALSE,col=clr,axes=FALSE,main=maintitle)

  ticksize=(-.2)
  if(!add)
   {
    axis(side=1,pos=0,at=c(seq(0,xdim,by=100),xdim),cex.axis=labelsize,labels=xaxis,tcl=ticksize)
    axis(side=3,pos=ydim,at=c(seq(0,xdim,by=100),xdim),labels=FALSE,tcl=ticksize)

    axis(side=2,pos=0,at=c(seq(0,ydim,by=100),ydim),cex.axis=labelsize,labels=yaxis,tcl=ticksize)
    axis(side=4,pos=xdim,at=c(seq(0,ydim,by=100),ydim),labels=FALSE,tcl=ticksize)
   } 
  
  # browser()
  ## par changes unexpectedly during execution (8 Sept 2012), presumably because some graphing functions change it?
  ## Changing par setting like pin, fin after a graph is opened, though, makes no sense, and it seemed to lead to failures, with points being excluded near the edges
  ## Correctly setting par across functions is mistake-prone. It must change between versions of R, since I have had successful versions later fail due to par, though I make no changes.
  if(!add) par(cex=oldpar['cex'])
  # browser()
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# map1species
# </name>
# <description>
# This does the mapping for a single species, called from map() but also useful on its own.
# With add=TRUE, points are added to an existing map. All other parameters are described with map(). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
map1species=function(sppdata,plotdim=c(1000,500),xrange=c(0,plotdim[1]),yrange=c(0,plotdim[2]),color="black",bgcolor="white",axspos=.5,
                     xaxis=TRUE,yaxis=TRUE,symbol=16,size=rep(-1,3),cutoff=c(10,100,300,3000),axisdiv=100,labsize=1.75,plotside=4,add=FALSE)
 {
  xdim=diff(xrange)
  ydim=diff(yrange)
  aspect=xdim/ydim

  nogrp=length(cutoff)-1
  grp=matrix(FALSE,nrow=nogrp,ncol=dim(sppdata)[1])

  if(size[1]<0) size=setsize(nogrp,length(sppdata$gx))

  for(i in 1:nogrp) grp[i,]=(sppdata$dbh>=cutoff[i] & sppdata$dbh<cutoff[i+1])
 # browser()

  if(add) 
   {
    # browser()  
    points(sppdata$gx[grp[1,]],sppdata$gy[grp[1,]],cex=size[1],col=color,pch=symbol)
    if(nogrp>1)
      for(i in 2:nogrp) points(sppdata$gx[grp[i,]],sppdata$gy[grp[i,]],cex=size[i],col=color,pch=symbol)
    return(0)
   }
  
  graphsize=c(plotside,plotside/aspect)
  oldpar=par(pch=symbol,col=color,col.main="black",bg=bgcolor,pin=graphsize,col.axis="black",mgp=c(3,axspos,0))

  plot(sppdata$gx[grp[1,]],sppdata$gy[grp[1,]],xlim=xrange,ylim=yrange,axes=F,xlab="",ylab="",cex=size[1],col=color)
  if(nogrp>1) for(i in 2:nogrp) points(sppdata$gx[grp[i,]],sppdata$gy[grp[i,]],cex=size[i],col=color)
  # browser()
  
  atx=c(seq(xrange[1],xrange[2]-.5,by=axisdiv),xrange[2])
  aty=c(seq(yrange[1],yrange[2]-.5,by=axisdiv),yrange[2])
  if(xaxis) axis(side=1,pos=yrange[1],at=atx,cex.axis=labsize)
  if(yaxis) axis(side=2,pos=xrange[1],at=aty,cex.axis=labsize)
  if(xaxis) axis(side=3,at=atx,labels=FALSE,pos=yrange[2])
  if(yaxis) axis(side=4,at=aty,labels=FALSE,pos=xrange[2])

 # par(oldpar)
 # browser()
 }





# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# setsize
# </name>
# <description>
# An internal function, called by map() to choose size of plotting points. It uses the number of dbh categories (n) and abundance of the species (s) 
# to help determine the size. Note that the user can override these defaults by submitting sizes.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
setsize=function(n,s)
 {
  if(s>1000)     size=0.4+.3*(0:(n-1))
  else if(s>400) size=.55+.3*(0:(n-1))
  else           size=0.7+.3*(0:(n-1))

  if(n==3)
   if(s>1000)
    {
     size[1]=.4
     size[2]=.7
     size[3]=1.1
    }
   else if(s>400)
    {
     size[1]=.55
     size[2]=.8
     size[3]=1.2
    }
   else
    {
     size[1]=.7
     size[2]=1
     size[3]=1.2
    }

  return(size)
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# map2species
# </name>
# <description>
# Map 2 species to a 2-panel pdf. Two species names are passed to spp as a vector. The data
# must be a list of two split plot data objects, spplist is a list of two different species tables, and elev
# a list of two different elevation matrices. To make 2 maps from the same plot, each of the lists should
# repeated the same data twice. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
map2species=function(spp,data=list(bci3.spp,korup.spp),spplist=list(plotspp$bci,plotspp$korup),
                     elev=list(ctfs.elev$bci,ctfs.elev$korup),path="spp",
                     export=pdf,file=pst(spp[1],spp[2],"map.pdf"),ptsize=c(0.6,0.6),topo=c(2,4),
                     ht=11,wd=8.5,plotside=4)
{
 if(!is.null(export))
  {
   on.exit(graphics.off())
   export(height=ht,width=wd,file=pst(path,file))
  }
 else x11(height=ht,width=wd,xpos=725)

 oldpar=par(mfcol=c(2,1),mai=c(1,.85,.1,.35),mgp=c(2,1,0),cex=1.4)

 map(splitdatafile=data[[1]],species=spp[1],spplist=spplist[[1]],elevdata=elev[[1]]$mat,
     export="file",size=rep(ptsize[1],3),topoint=topo[1],plotside=plotside,legpos=c(500,585))
 map(splitdatafile=data[[2]],species=spp[2],spplist=spplist[[2]],elevdata=elev[[2]]$mat,
     export="file",size=rep(ptsize[2],3),topoint=topo[2],plotside=plotside,legpos=c(500,585))

}
# </source>
# </function>
# 
# 
