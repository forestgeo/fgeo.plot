# <function>
# <name>
# coldata.to.imagemat
# </name>
# <description>
# These functions create maps of response variables using the R functions contour, image, and filled.contour. Those functions are
# powerful and do the difficult work, but they are a bit tricky to learn and have many options. If you are expert with those 3 R functions,
# the functions below will not be especially useful. They simply rearrange various types of data into the correct format. 
# <br><br>
# The first, coldata.to.imagemat, converts a vector of response variables and converts it into a matrix of the correct form for use by contour, image,
# and filled.contour. There must be one value of the response z for every position in a grid overlain on the map. In the case of CTFS Plots,
# this means a value for z at every quadrat, though quadrat can be any size.  The function abundanceperquad (abundance.r) produces 
# exactly the correct kind of vector for use by coldata.to.imagemat. The function gxgy.to.index (quadfunc.r) produces quadrat numbers that  
# are the correct vector for use by coldata.to.imagemat.
# <br><br>
# For example, consider a 50 ha plot of 1000 x 500 meters. If gridsize=20, the standard quadrat, then there are 50 columns x 25 rows in the plot.
# In that case, the length of vector z must be exactly 1250, length(x) must be exactly 50, and length(y) exactly 25. Alternatively, x and y can be set
# NULL and it will be calculated from plotmax=c(1000,500) and plotmin=c(0,0). The simplest application of this function for CTFS plots is to
# set x and y NULL, and plotmax to the correct plot dimensions. Then make sure z is the right size for the number of quadrats. 
# <br><br>
# The common error with image and contour maps is getting the proper dimensions for z and the proper sizes for plotmax, x, and y. If you get an error
# 'data length is not a sub-multiple or multiple of the number of rows' then one of those inputs is wrong. <br><br>
# Return value is a list designed to match exactly the format of the RGDAL raster object. It has components
# <ul>
# <li> x: same as the argument x; if x is submitted as NULL, this is the vector calculated by using plotmax;
# <li> y: likewise for y;
# <li> z: exactly as submitted;
# <li> columns: number of columns in the grid, same as length(x);
# <li> rows: Number of rows in the grid, same as length(y);
# <li> mat: The matrix needed for graphing, exactly as needed for instance by imageGraph().
# </description>
# <arguments>
# <ul>
# <li> z: A numeric vector of responses, which means an attribute which has map coordinates.
# <li> x: The map coordinates of each column in the matrix, and the values with which the x-axis of the map will be numbered.
# The number of columns of z, dim(z)[2], must be exactly length(x). If x is NULL, then it is calculated using plotmax.
# <li> y: The map coordinates of each row in the matrix, and the values with which the y-axis of the map will be numbered; 
# dim(z)[1] must be exactly length(y). If y is NULL, then it is calculated using plotmax.
# <li> gridsize: Size of quadrats into which the map is divided; must be square quadrats. This cannot be NULL.  
# <li> plotmin: The minimum plot x and y coordinates; in CTFS plots, these are always 0,0. Ignored if x and y are submitted.
# <li> plotmax: Maximum plot x and y coordinates. The default, 1000,500, is typical CTFS 50-ha plot. Ignored if x and y are submitted.
# </ul>
# </arguments>
# <sample>
# CTFSplot('bci',6,'full')<br>
# BA=abundanceperquad(bci.full6,gridsize=20,plotdim=c(1000,500),type='ba')<br>
# totalBAperquad=colSums(BA$ba)<br>
# summary(totalBAperquad)<br>
# matrixdata=coldata.to.imagemat(z=totalBAperquad,x=NULL,y=NULL,gridsize=20,plotmax=c(1000,500))<br>
# length(totalBAperquad)<br>
# dim(matrixdata$mat)<br>
# length(matrixdata$x)
# </sample>
# <source>
coldata.to.imagemat=function(z,x=NULL,y=NULL,gridsize=20,plotmin=c(0,0),plotmax=c(1000,500))
{
  if(length(gridsize)==1) gridsize=c(gridsize,gridsize)
  
  if(!is.null(x)) 
   {
    columns=1+round(diff(range(x))/gridsize[1],0)
    rows=1+round(diff(range(y))/gridsize[2],0)
   }
  else if(!is.null(plotmin)) 
   {
    columns=round((plotmax[1]-plotmin[1])/gridsize[1],0)
    rows=round((plotmax[2]-plotmin[2])/gridsize[2],0)
    x=seq(plotmin[1],plotmax[1],len=columns)
    y=seq(plotmin[2],plotmax[2],len=rows)
   }
  else return('Submit separate vectors x, y, or z plus plotmin and plotmax')

 datamat=matrix(z,nrow=rows,ncol=columns,byrow=FALSE)

 return(list(x=x,y=y,z=z,cols=columns,rows=rows,mat=datamat))
}
# </source>
# </function>
# 


# <function>
# <name>
# imageGraph
# </name>
# <description>
# Maps a response variable with R's contour or image functions, or both, using the output of coldata.to.imagemat.
# Also can return contours as vector data, using R's contourLines. The graph can be sent to the screen or exported.
# There are many options for tweaking the graph, explained with the arguments.
# </description>
# <arguments>
# <ul>
# <li> matrixdata: a list having components x, y, and mat, exactly the output of coldata.to.imagemat
# <li> levels: if set, then contours are drawn (or colors assigned) to breaks are at levels equally-spaced divisions of the response variable; 
# can be NULL to let breaks control
# <li> breaks: if set, a vector of breaks for the image colors and the contour lines; if NULL, levels controls the breaks
# <li> xname and yname: names for the axes
# <li> xrange: if NULL, all x are included, otherwise, the graph will only include x inside xrange
# <li> yrange: likewise for y
# <li> axisdiv: the division between tick marks; must be a vector of 2, first for x ticks then for y ticks
# <li> clrs: colors for the image; if NULL, they are chosen by default, otherwise, must be 1 + the number of breaks
# <li> returnline: TRUE or FALSE, whether to return the contour lines; see R's base function contourLines for details
# <li> img: TRUE or FALSE, whether to draw the color image
# <li> cntrs: TRUE or FALSE, whether to add contour lines; both img and cntrs can be TRUE
# <li> filled; TRUE or FALSE, whether to draw filled contours; if TRUE, neither image or standard contours are included
# <li> newgraph: if FALSE, just graph to the screen and export is ignored; use TRUE to create a new plot window or export to a file
# <li> plotsize: size of graph's x-dimension in inches; ignored if newgraph=FALSE
# <li> h, w: height and width of graph window in units which depend on the export device chosen (some are pixels, some are inches); 
# ignored if newgraph=FALSE
# <li> export: if newgraph=TRUE, this defines the graphics device; options include X11, quartz, win.graph, win.metafile, png, bmp, jpeg, pdf (not quoted!)
# <li> graphfile: the file name if export is pdf, png, jpeg, win.metafile, etc.
# </ul>
# </arguments>
# <sample>
# CTFSplot('bci',6,'full')
# N=colSums(abundanceperquad(bci.full6,plotdim=c(1000,500),grid=20)$abund)
# matdata=coldata.to.imagemat(z=N,gridsize=20,plotmax=c(1000,500))
# imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=FALSE,newgraph=FALSE,plotsize=6,h=11,w=11)
# imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=TRUE,newgraph=FALSE,plotsize=6,h=11,w=11)
# imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=FALSE,newgraph=TRUE,export=win.graph,h=11,w=11,plotsize=8)
# </sample>
# <source>
imageGraph=function(matrixdata,levels=NULL,breaks=c(-33000,0,150,250,500,1000,2000),xname="x",yname="y",xrange=NULL,yrange=NULL,axisdiv=c(100,100),
                    img=TRUE,cntrs=FALSE,filled=TRUE,drawgrid=FALSE,addaxes=1:4,clrs=NULL,cntcolor='black',lwidth=1,returnline=FALSE,
                    newgraph=FALSE,add=FALSE,plotsize=6,h=11,w=11,export=X11,graphfile=NULL)
{
 xax=matrixdata$x
 yax=matrixdata$y

 if(is.null(xrange)) xrange=range(xax)
 if(is.null(yrange)) yrange=range(yax)
 incx=xax>=xrange[1] & xax<=xrange[2]
 incy=yax>=yrange[1] & yax<=yrange[2]
 mat=matrixdata$mat[incy,incx]
 xax=xax[incx]
 yax=yax[incy]

 dim=dim(mat)
 aspect=dim[1]/dim[2]

 if(is.null(levels)) levels=10
 if(is.null(breaks)) breaks=seq(min(mat),max(mat),length=levels)

 if(is.null(clrs)) clrs=c("blue",terrain.colors(length(breaks)-2))

 if(newgraph) 
  {
   if(is.null(graphfile)) export(height=h,width=w)
   else export(height=h,width=w,file=graphfile)
  }
 oldpar=par(pin=c(plotsize,plotsize*aspect))

 xgrid=diff(xrange)/dim(mat)[2]
 ygrid=diff(yrange)/dim(mat)[1]

 if(img)
  {
   imagex=seq(xrange[1]+xgrid/2,xrange[2]-xgrid/2,len=length(xax))
   imagey=seq(yrange[1]+ygrid/2,yrange[2]-ygrid/2,len=length(yax))
  }

 if(filled) 
   filled.contour(x=xax,y=yax,z=t(mat),levels=breaks,asp=1,col=clrs,xlab=xname,ylab=yname,axes=FALSE,lwd=lwidth,
                  plot.axes={draw.axes(whichaxs=1:2,xrange=xrange,yrange=yrange,div=axisdiv)},key.axes=axis(4,round(breaks,0)))
 else if(img) image(x=imagex,y=imagey,z=t(mat),breaks=breaks,col=clrs,xlab=xname,ylab=yname,axes=FALSE,add=TRUE)
 else contour(x=xax,y=yax,z=t(mat),levels=breaks,xlab=xname,ylab=yname,axes=FALSE,lwd=lwidth,add=add,col=cntcolor)
 
 if(img&cntrs&!filled) contour(x=xax,y=yax,z=t(mat),levels=breaks,axes=FALSE,lwd=lwidth,add=TRUE,col=cntcolor)
 
 if(!filled & !is.null(addaxes)) draw.axes(addaxes,xrange=xrange,yrange=yrange,div=axisdiv)

 if(drawgrid)
   {
    abline(h=seq(yrange[1],yrange[2],by=ygrid))
    abline(v=seq(xrange[1],xrange[2],by=xgrid))
   }

 if(!is.null(graphfile)) graphics.off()

 par(oldpar)
 if(returnline) return(contourLines(x=xax,y=yax,z=t(mat),levels=breaks))
}
# </source>
# </function>
#  
# <function>
# <name>
# draw.axes
# </name>
# <description>
# Add axes to a graph, with ticks spaced by the vector div. This is a subroutine used in imageGraph().
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
draw.axes=function(whichaxs=1:4,xrange,yrange,div,whichbox=1:4)
{
 xtick=seq(xrange[1],xrange[2],by=div[1])
 ytick=seq(yrange[1],yrange[2],by=div[2])
          
 if(1 %in% whichaxs) axis(1,at=xtick,pos=yrange[1])
 if(1 %in% whichbox) segments(x0=xrange[1],x1=xrange[2],y0=yrange[1],y1=yrange[1])
 if(2 %in% whichaxs) axis(2,at=ytick,pos=xrange[1])
 if(2 %in% whichbox) segments(x0=xrange[1],x1=xrange[1],y0=yrange[1],y1=yrange[2])
 if(3 %in% whichaxs) axis(3,at=xtick,pos=yrange[2])
 if(3 %in% whichbox) segments(x0=xrange[1],x1=xrange[2],y0=yrange[2],y1=yrange[2])
 if(4 %in% whichaxs) axis(4,at=ytick,pos=xrange[2])
 if(4 %in% whichbox) segments(x0=xrange[2],x1=xrange[2],y0=yrange[1],y1=yrange[2])
}
# </source>
# </function>

# 
# <function>
# <name>
# image.dataframe
# </name> 
# <description>
# Creates a color response map based on columnar data, not a matrix as used by R's image function. It is used in cases where a full matrix
# of responses, over a complete grid, is not available. Data are submitted as a table with x and y coordinates, and a response z at each.
# The points are mapped on an x-y plot, then colored according to the value of z. One typical use would be where soil data are available
# at many locations, coloring points by the concentration of one nutrient. Another, illustrated below, would be to color tree locations according to dbh. 
# </description>
# <arguments>
# <ul>
# <li> data: A data frame with at least 3 columns, 2 of which are coordinates and one a numerical response at those coordinates
# <li> xcol, ycol, zcol: These allow the columns of data to carry non-standard names
# <li> ptsize: Size of points to be drawn
# </ul>
# Other arguments are the same as imageGraph().
# </arguments>
# <sample>
# CTFSplot('bci',6,'full')<br>
# image.dataframe(data=subset(bci.full6,status=='A'),xcol='gx',ycol='gy',zcol='dbh',breaks=c(10,20,50,100,500,10000),xrange=c(0,100),yrange=c(0,100), colors=c('orange','yellow','lightgreen','green','blue'),newgraph=TRUE,h=9,w=12,plotsize=7)
# </sample>
# <source>
image.dataframe=function(data,xcol='x',ycol='y',zcol='z',breaks=NULL,div=NULL,colors=NULL,xname='x',yname='y',xrange=NULL,yrange=NULL,
                         ptsize=1,graphfile=NULL,export=X11,newgraph=FALSE,h=11,w=11,plotsize=6)
{
 x=data[,xcol]
 y=data[,ycol]
 z=data[,zcol]
 
 if(is.null(xrange)) xrange=range(x,na.rm=TRUE)
 if(is.null(yrange)) yrange=range(y,na.rm=TRUE)
 if(is.null(div)) div=10
 if(is.null(breaks)) breaks=seq(min(z,na.rm=TRUE),max(z,na.rm=TRUE),length=div)
 nocol=length(breaks)-2
 if(is.null(colors)) colors=c('white',terrain.colors(nocol)[nocol:1])
 
 aspect=diff(yrange)/diff(xrange)
 if(newgraph) 
  {
   if(is.null(graphfile)) export(height=h,width=w)
   else export(height=h,width=w,file=graphfile)
  }
 oldpar=par(fin=c(plotsize+1,plotsize+1),pin=c(plotsize,plotsize*aspect))

 plot(x,y,pch='.',xlim=xrange,ylim=yrange)
 for(i in 2:length(breaks))
  {
   includepts=(z>=breaks[i-1] & z<=breaks[i])
   points(x[includepts],y[includepts],pch=16,cex=ptsize,col=colors[i-1])
  }
  
 par(oldpar)
}
# </source>
# </function>
# 
# 
