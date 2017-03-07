# <function>
# <name>
# xydist
# </name>
# <description>
# Distance between two pairs of x-y coordinates. Input can be atomic or vector. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydist=function(x1,y1,x2,y2) 
  return( sqrt( (x1-x2)^2 + (y1-y2)^2 ) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# xydistvect
# </name>
# <description>
# Distance between two x-y coordinates, but accepts each set of coordinates as a vector of length 2,
# with the first element the x coordinates, the second y. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydistvect=function(pt1,pt2)
  return( xydist(pt1[,1],pt1[,2],pt2[,1],pt2[,2]) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# xydistmat
# </name>
# <description>
# Distance between two x-y coordinates, but accepts two sets of coordinates in a single matrix (4 columns ordered x1, y1, x2, y2). 
# For use with apply.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydistmat=function(pts)
  return( xydist(pts[,1],pts[,2],pts[,3],pts[,4]) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# perpendicular.distance
# </name>
# <description>
# Distance from a point to a line (so it's the perpendicular distance);
# m and b are slope and intercept; x and y are coordinates. If both b,m and x,y are vectors, they must all be same length.
# Note check for infinite slope, meaning that the intercept b is the x-intercept.

# </description>
# <display>true</display>
# <update>true</update>
# <arguments>
# b: y-intercept
# m: line slope
# </arguments>
# <sample>
# 
# </sample>
# <source>
perpendicular.distance=function(b,m,x,y) 
 {
  len=ifelse(length(b)>length(x),length(b),length(x))
  minf=is.infinite(m)
  result=rep(NA,len)
  
  result=sqrt((y-m*x-b)^2/(1+m^2))
  result[minf]=abs(x-b)[minf]
  
  return(result)
 }
# </source>
# </function>
# 

# Find distance from a pt (x then y coords) to a line segment given as start and end points. Either x, y or end points can be vectors, but not both.
# It first finds perpendicular distance, then distance to each end point, and returns the minimum. 
pt.to.segment=function(x,y,x1,y1,x2,y2)
{
 bm=pts.to.interceptslope(pt1=data.frame(x1,y1),pt2=data.frame(x2,y2))
 
 perpline=perpendicular.line(x=x,y=y,b=bm$b,m=bm$m)
 intersect=intersection.of.lines(b1=bm$b,m1=bm$m,b2=perpline$b,m2=perpline$m)
 
 perpcheck=is.between(intersect$x,intersect$y,x1,y1,x2,y2)
 noperp=length(which(perpcheck))
 
 perp=perpendicular.distance(x=x,y=y,b=bm$b,m=bm$m)
 pt1dist=xydist(x,y,x1,y1)
 pt2dist=xydist(x,y,x2,y2)
 # browser()
 
 # alldist=data.frame(pt1dist,pt2dist)
 closest=c(min(pt1dist),min(pt2dist),min(pt1dist)+1)
 perp[!perpcheck]=max(pt1dist)+1
 if(noperp>0) closest[3]=min(perp) 
 choice=which.min(closest)
 
 if(choice==1) { closeind=which.min(pt1dist); closex=x1[closeind]; closey=y1[closeind]; mindist=closest[1] }
 else if(choice==2) { closeind=which.min(pt2dist); closex=x2[closeind]; closey=y2[closeind]; mindist=closest[2] }
 else { closeind=which.min(perp); closex=intersect[closeind,'x']; closey=intersect[closeind,'y']; mindist=closest[3] }
 
 return(c(x=closex,y=closey,index=closeind,distance=mindist))
 # return(data.frame(mindist,perpcheck)) 
}
# 
# 
# <function>
# <name>
# perpendicular.line
# </name>
# <description>
# Finds the slope and intercept of the line perpendicular to a line whose slope and intercept are given, 
# through the points x,y. Either x and y can be vectors, or b and m can be vectors, or if all are vectors, must be same length. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
perpendicular.line=function(b,m,x,y)
{
 perpm=tan(pi/2+atan(m))
 
 intercept=y-x*perpm
 result=data.frame(b=intercept,m=perpm)
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# parallel.line
# </name>
# <description>
# Finds the slope and intercept of the line parallel to a line whose slope and intercept are given, 
# through the points x,y. Note that the intercept is not needed. For any m that are infinite, the intercept is x.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
parallel.line=function(b,m,x,y)
{
 minf=is.infinite(m)
 
 len=IfElse(length(b)>length(x),length(b),length(x))

 if(length(m)==1) slope=rep(m,len)
 else slope=m
 
 inter=y-m*x 
 inter[minf]=x
 
 result=data.frame(b=inter,m=slope)
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# intersection.of.lines
# </name>
# <description>
# Finds the point where 2 lines intersect, given lines as 2 parameters each (intercept b then slope m). 
# If the two lines are identical, it returns NAs. Note the check for both slopes being infinite (vertical lines).
# This is vectorized: either pair can be vector if other pair is atomic, or both can be same length vectors.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
intersection.of.lines=function(b1,m1,b2,m2)
{
 len=IfElse(length(b2)>length(b1),length(b2),length(b1))
 
 m1inf=(is.infinite(m1)&!is.infinite(m2))
 m2inf=(is.infinite(m2)&!is.infinite(m1))
 mfinite=(!m1inf&!m2inf)
 
 exact=(m1==m2 & b1==b2) | (is.infinite(m1) & is.infinite(m2))
 
 x=(b2-b1)/(m1-m2)
 y=m1*x+b1
 
 x[m1inf]=b1[m1inf]
 y[m1inf]=(m2*b1+b2)[m1inf]
 
 x[m2inf]=b2[m2inf]
 y[m2inf]=(m1*b2+b1)[m2inf]
 
 x[exact]=NA
 y[exact]=NA
 
 return(data.frame(x,y))
}
# </source>
# </function>
# <function>
# <name>
# intersection.line.curve
# </name>
# <description>
# Finds the points where a straight line, given lines as intercept b then slope m, intersects with a curve defined by a sequence of segments. Intersections must be found for the line with every segment, then every one checked with is.between. The intersections are returned as a dataframe of x, y coordinates. The b and m must be atomic. The curve must be a dataframe with columns x, y (or capital X, Y). 
# having any number of segments.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
intersection.line.curve=function(b,m,curve)
{
 cname=colnames(curve)
 colnames(curve)[cname=='x']='X'
 colnames(curve)[cname=='y']='Y'
 
 nopt=dim(curve)[1]
 segments=pts.to.interceptslope(pt1=curve[-nopt,c('X','Y')],pt2=curve[-1,c('X','Y')])
 
 intersect=intersection.of.lines(b1=b,m1=m,b2=segments$b,m2=segments$m)
 test=is.between(x=intersect$x,y=intersect$y,x1=curve$X[-nopt],y1=curve$Y[-nopt],x2=curve$X[-1],y2=curve$Y[-1])
 
 return(intersect[test,])
}
#
#
# </source>
# </function># 
# <function>
# <name>
# is.between
# </name>
# <description>
# Check whether a point x,y falls between two other points. The typical use is where the first point falls on the segment connecting the next two points. 
# All arguments can be vector, but must be identical in length. An x falls between when it lies in [x1,x2). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
is.between=function(x,y,x1,y1,x2,y2)
{
 xout=ifelse(x2>x1,x<x1|x>=x2,x<=x2|x>x1)
 yout=ifelse(y2>y1,y<y1|y>=y2,y<=y2|y>y1)

 result=rep(TRUE,length(x))
 result[xout|yout]=FALSE
 
 # Arrangements where end points have precisely matching coordinates.
 exactX=(x1==x2)
 result[exactX & x==x1 & y1<y2 & y>=y1 & y<y2]=TRUE
 result[exactX & x==x1 & y1>y2 & y>=y2 & y<y1]=TRUE

 exactY=(y1==y2)
 result[exactY & y==y1 & x1<x2 & x>=x1 & x<x2]=TRUE
 result[exactY & y==y1 & x1>x2 & x>=x2 & x<x1]=TRUE
 
 return(result)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# line.intersection.pts
# </name>
# <description>
# Finds the point where 2 lines intersect, given each line as 2 pairs of points on the line
# </description>
# <arguments>
# Both arguments must have columns x, y, with two rows, one row per point. 
# </arguments>
# <sample>
# 
# </sample>
# <source>
line.intersection.pts=function(pts1,pts2)
{
 line1=pts.to.interceptslope(pts1[1,],pts1[2,])
 line2=pts.to.interceptslope(pts2[1,],pts2[2,])

 return(line.intersection(m1=line1[1],b1=line1[2],m2=line2[1],b2=line2[2]))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# pts.to.interceptslope
# </name>
# <description>
# Returns intercept and slope of a line given two pairs of coordinates on the line. Arguments can be vectors; if both are vectors, must be same size. 
# If the x's are exactly equal, so slope is infinite, it returns the x as the first argument. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
pts.to.interceptslope=function(pt1,pt2)
{
 if(is.null(dim(pt1))) { x1=pt1[1]; y1=pt1[2] }
 else { x1=pt1[,1]; y1=pt1[,2] }

 if(is.null(dim(pt2))) { x2=pt2[1]; y2=pt2[2] }
 else { x2=pt2[,1]; y2=pt2[,2] }

 len=IfElse(length(x2)>length(x1),length(x2),length(x1))
 exact=(x1==x2)
 
 slope=(y2-y1)/(x2-x1)
 inter=y1-slope*x1
 slope[exact]=Inf
 inter[exact]=x1[exact]

 result=data.frame(b=inter,m=slope)
}
# </source>
# </function>
# 

# 
# <function>
# <name>
# segmentPt
# </name>
# <description>
# Draw a line segment between two points, where each point is a vector of x then y coordinates
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
segmentPt=function(pt1,pt2,clr='black',lwidth=1) 
{
 if(is.null(dim(pt1))) { x1=pt1[1]; y1=pt1[2] }
 else { x1=pt1[,1]; y1=pt1[,2] }

 if(is.null(dim(pt2))) { x2=pt2[1]; y2=pt2[2] }
 else { x2=pt2[,1]; y2=pt2[,2] }

 segments(x0=x1,x1=x2,y0=y1,y1=y2,col=clr,lwd=lwidth)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# drawrectangle
# </name>
# <description>
# Draw a rectangle given a matrix or dataframe of 4 x-y coordinates. The columns must have x first then y coordinates. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
drawrectangle=function(corners,add=TRUE,clr='black',lwidth=1)
{
 if(!add)
  {
   xrange=range(corners$x)
   yrange=range(corners$y)
   plot(xrange[1],yrange[1],xlim=xrange,ylim=yrange,xlab="",ylab="",col="white",axes=FALSE)
  }

 segmentPt(corners[1,],corners[2,],clr=clr,lwidth=lwidth)
 segmentPt(corners[2,],corners[3,],clr=clr,lwidth=lwidth)
 segmentPt(corners[3,],corners[4,],clr=clr,lwidth=lwidth)
 segmentPt(corners[1,],corners[4,],clr=clr,lwidth=lwidth)
}
# </source>
# </function>
#
#
# 
# <function>
# <name>
# angleBisector
# </name>
# <description>
# Given 3 sets of coordinates defining two line segments (middle point is intersection), find line bisecting the angle through middle point. This is not vectorized. It only works with 3 sets of coordinates. Note that it is necessary to correctly find the compass direction theta, which the slope alone does not establish:
# 1) If slope is negative and x2>=x1 (travel is toward Quadrant IV), then theta=atan(slope)
# 2) If slope is negative with x2<x1 (travel is toward Quadrant II), then theta=atan(slope)+pi
# 3) If slope is positive with x2>=x1 (travel is toward Quadrant I), then theta=atan(slope)
# 4) If slope is positive with x2<x1 (travel is toward Quadrant III), then theta=atan(slope)-pi
# This returns theta in (-pi,pi). Then the turn is calculated, ie the change in direction between the two segments as the difference in theta, but it is reset to be on (-pi,pi) as well in order to correctly estimate the mean direction of travel (half the turn added to theta[1]).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
angleBisector=function(corners)
{
 bm=pts.to.interceptslope(pt1=corners[1:2,],pt2=corners[2:3,])
 
 left=(corners[1:2,1]>=corners[2:3,1])
 right=!left
 up=(bm[,2]>=0 & right) | (bm[,2]<0 & left)
 down=!up
 QI=right&up
 QII=left&up
 QIII=left&down
 QIV=right&down
 
 theta=atan(bm[,2])
 theta[QII]=atan(bm[QII,2])+pi
 theta[QIII]=atan(bm[QIII,2])-pi
 
 fullturn=theta[2]-theta[1]
 if(fullturn>pi) turn=fullturn-2*pi
 else if(-fullturn>pi) turn=fullturn+2*pi
 else turn=fullturn
 
 slope=theta[1]+turn/2
 # slope=mean(theta)
 
 perpm=tan(pi/2+slope)
 result=parallel.line(b=NA,m=perpm,x=corners[2,1],y=corners[2,2])
 # avgtravel=parallel.line(b=NA,m=tan(slope),x=corners[2,1],y=corners[2,2])
 # if(length(which(QIII))>0 & length(which(QII))>0) browser()
 
 return(drp(as.matrix(result)))
}
# </source>
# </function>

# <function>
# <name>
# insideRectangle
# </name>
# <description>
# Checks a vector of coordinates x, y to return which are inside a rectangle. For a much more general function for checking whether
# points are inside polygons, use the function inout() in the package splancs.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
insideRectangle=function(x,y,xrange,yrange)
{
 inX=x>=xrange[1] & x<xrange[2] & !is.na(x)
 inY=y>=yrange[1] & y<yrange[2] & !is.na(y)
 return(inX&inY)
}
# </source>
# </function>
# 


# <function>
# <name>
## are.ptsinside
# </name>
# <description>
# Checks many points (dataframe pt with x and y) against a single quadrat whose corners are given by as xlo, ylo, xhi, yhi.
# It returns a logical vector, TRUE for the points inside. This is same as insideRectange, but accepting input as a matrix pts
# and a single vector of the four corners of the rectange. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
are.ptsinside=function(pts,coord)
{
 return(insideRectange(x=pts[,1],y=pts[,2],xrange=coord[,c(1,3)],yrange=coord[,c(2,4)]))
}

# </source>
# </function>

# <function>
# <name>
# ispt.inside
# </name>
# <description>
# Check a single pt (x and y) against a large number of quadrats whose corners are given by the rows of coord, xlo, ylo, xhi, yhi.
# It returns the fraction of quadrats which the point falls inside. This is exactly like are.ptsinside() but allows there to be many
# rectangles, defined by a dataframe coord.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
ispt.inside=function(pt,coord)
{
 norect=dim(coord)[1]
 inside=logical()
 
 for(i in 1:norect) logical[i]=are.ptsinside(pt,drp(coord[i,]))
 
 return(inside)
}
# </source>
# </function>


# 
# <function>
# <name>
# inside.rect
# </name>
# <description>
# Determines whether any of the 4 corners of one rectangle are within a second rectangle. Both rectangles are submitted as c(x0,x1,y0,y1). If just one
# of the corners is inside, it returns true. See insideRectange(), which has a similar name but does something different.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
inside.rect=function(rect1,rect2)
{
 if(rect1[2]<rect2[1]) return(FALSE)
 if(rect1[1]>rect2[2]) return(FALSE)
 if(rect1[4]<rect2[3]) return(FALSE)
 if(rect1[3]>rect2[4]) return(FALSE)

 return(TRUE)
}
# </source>
# </function>
# 

# 
# <function>
# <name>
# circle
# </name>
# <description>
# Calculates points on a 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
circle=function(x,center=c(0,0),radius=5,half="top")
 {
  xprime=x-center[1]
  
  yprimesq=radius^2-xprime^2
  yprime=numeric()
  yprime[yprimesq>=0]=sqrt(yprimesq[yprimesq>=0])
  
  if(half=="bottom") yprime=(-1)*yprime
  
  y=yprime+center[2]
  return(y)
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fullcircle
# </name>
# <description>
# Create a dataframe for a full circle, with x values repeated to get top then bottom. NA is inserted
# so this can be passed directly to graphing functions.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
fullcircle=function(x,center=c(0,0),radius=5)
{
 tophalf=circle(x,center,radius,half='top')
 bottomhalf=circle(x,center,radius,half='bottom')
 len=length(x)
 result=data.frame(x=c(x,NA,x[len:1]),y=c(tophalf,NA,bottomhalf[len:1]))
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# ellipse
# </name>
# <description>
# Equation for (half) a canonical 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
ellipse=function(x,center=c(0,0),radius=c(7,5),half='top')
{
  xprime=x-center[1]
  inc=abs(xprime)<=radius[1]
  yprimesq=yprime=t=numeric()

  t[inc]=acos(xprime[inc]/radius[1])
  yprime[inc]=radius[2]*sin(t[inc])
  
  if(half=="bottom") yprime=(-1)*yprime
  
  y=yprime+center[2]
  return(y)
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fullellipse
# </name>
# <description>
# Creates a dataframe for a full ellipse. 
# </description>
# <arguments>
# <ul>
# <li> x = input x coordinates
# <li> center = x-y coordinates of ellipse center (vector of 2)
# </ul>
# </arguments>
# <sample>
# 
# </sample>
# <source>
fullellipse=function(x,center=c(0,0),radius=c(7,5))
{
 tophalf=ellipse(x,center,radius,half='top')
 bottomhalf=ellipse(x,center,radius,half='bottom')
 
 result=data.frame(x=c(x,NA,x),y=c(tophalf,NA,bottomhalf))
 return(result)
}
   

# </source>
# </function>
# 
# 

# <function>
# <name>
# cartesian.to.polar
# </name>
# <description>
# Convert Cartesian coordinates to polar. Returns a dataframe of two columns named r and theta. This always
# returns a theta between -pi/2 and pi/2. Note that polar.to.cartesian may not return the starting x, y submitted
# to cartesian.to.polar due to problems with signs. It will work if theta is kept positive (ie, quadrant 1).
# </description>
# <arguments>
# <ul>
# <li> x = input vector of x coordinates
# <li> y = input vector of y coordinates (same length as x)
# </ul>
# </arguments>
# <sample>
# cartesian.to.polar(2,4)
# </sample>
# <source>
cartesian.to.polar=function(x,y)
{
 r=sqrt(x^2+y^2)
 theta=atan(y/x)
 zeroes=(x==0)
 
 theta[zeroes&y>0]=pi/2
 theta[zeroes&y<0]=(-1)*pi/2
 
 return(data.frame(r,theta))
}
# </source>
# </function>
# 


# <function>
# <name>
# polar.to.cartesian
# </name>
# <description>
# Convert polar coordinates to Cartesian. Returns a dataframe of two columns named x and y.
# </description>
# <arguments>
# <ul>
# <li> r = input vector of radii (distance from origin)
# <li> theta = input vector of angle from horizontal (radians), same length as r
# </ul>
# </arguments>
# <sample>
# polar.to.cartesian(2,pi/3)
# </sample>
# <source>
polar.to.cartesian=function(r,theta)
{
 x=r*cos(theta)
 y=r*sin(theta)
 return(data.frame(x,y))
}
# </source>
# </function>



### Should be deleted ... 
# 
# <function>
# <name>
# slope.intercept.frompts
# </name>
# <description>
# This is an old version of pts.to.interceptslope, returns slope than intercept of a line given two points. The alternate function, pts.to.interceptslope, 
# returns intercept then slope, which is more standard. This is kept for compatibility with old functions. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#slope.intercept.frompts=function(pt1,pt2)
#{
# line=pts.to.interceptslope(pt1,pt2)
 
# return(c(line[2:1]))
#}
# </source>
# </function>
# 
# 
