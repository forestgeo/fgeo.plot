
# <function>
# <name>
# NeighborDensities
# </name>

# <description>
# Calculates the total number or basal area of conspecific and heterospecific neighbors at a radius r 
# with edge effect correction. Most arguments aim to describe the type of neighbors to count <br> 
# Dependencies: CalcRingArea and circlearea from the CTFS package, and splancs package.<br> 
# Output: a data.frame with two columns listing the density of consp and hetsp, and rows equal to the size of censdata if censdata2 is absent, or censdata2 if it is present. 
# Author: Tania Brenes<br> 
# </description>

# <arguments>
# <ul>
#   <li>  censdata: should have the coordinates (gx, gy), species id (sp), status, tag and dbh. It counts neighbors for each of the individuals in censdata.
#   <li>  censdata2 (NULL): an optional dataset for focal individuals, for example a subset of only one species or a list of seedling coordinates that are not part of census data. It must contain coordinates (gx,gy), species id (sp) and tag. It will count the neighbors in censdata but only for these focal coordinates.
#   <li>  r (20): radius to count neighbors
#   <li>  plotdim  (c(1000,500)):  vector with two numbers indicating the xy-plot size;
#   <li>  mindbh (10): minimum size of neighbors to be counted
#   <li>  type (count): calculates count of stems ('count') or sum of basal areas ('basal'). Note that for the sum of basal areas is better to use the plot stem table, otherwise only one stem per tree will be included .
#   <li>  include (c("A")): a vector of status to include in the neighbors, by default it counts only alive individuals, for all status use c("A","P","D","M").
# </ul>
# </arguments>

# <sample>
# > load("bci.full7.rdata")<br>
# > attach("CTFSRPackage.rdata")<br>
# > library(splancs)<br>
# ## sum consp and hetsp neighbors for all stems in the plot <br> 
# > neighbor.counts <- NeighborDensities(bci.full7, r=20, type='count')<br> 
# ## sum consp and hetsp neighbors for only one species:<br> 
# > one.sp = subset(bci.full7, sp=="quaras")<br> 
# > neighbor.counts <- NeighborDensities(bci.full7, one.sp, type='count')<br> 
# </sample>

# <source>
NeighborDensities=function(censdata, censdata2=NULL, r=20, plotdim=c(1000,500), mindbh=10, type='count', include=c("A"))
{
ptm <- proc.time()

if (is.null(censdata2)) censdata2 <- censdata
n = nrow(censdata2)

output = matrix(NA, ncol=2, nrow=n)
dimnames(output)[[2]] <- c("con.sp","het.sp") 

if (type =='count') {

spd = subset( censdata, !is.na(gx) & !is.na(gy) & !duplicated(tag) & dbh>=mindbh & status %in% include)

	for (i in 1:n) {

	focal = censdata2[i,]
	if (is.na(focal$gx) | is.na(focal$gy) | duplicated(focal$tag)) output[i,1:2] = NA  else {

	poly= with(focal, spoints(c(gx-r, gy-r, gx-r, gy+r, gx+r, gy+r, gx+r, gy-r)))
	use = inpip(spoints(rbind(spd$gx,spd$gy)), poly)

	if (length(use)==0) output[i,1:2]=0  else {

	incircle = sqrt(dsquare(spoints(rbind(spd$gx[use],spd$gy[use])), spoints(rbind(focal$gx,focal$gy)) )) <= r
	nn = use[incircle]  #  & spd$tag[use] != focal$tag[i]

	consp <- spd$sp[nn]==focal$sp
	area = CalcRingArea(data.frame(gx=focal$gx,gy=focal$gy), r, plotdim)$each 

	output[i,1] = length(nn[consp]) * (pi*r^2 /area)
	output[i,2] = length(nn[!consp]) * (pi*r^2 /area)  
	} } 

	if (i %in% seq(5000,n+5000,5000))  cat( i, "of", n, " elapsed time = ", (proc.time()-ptm)[3], "seconds" , "\n" ) 
}

}  # end of count subrutine 
  
if (type =='basal') {

spd = subset( censdata, !is.na(gx) & !is.na(gy) & !is.na(dbh) & dbh>=mindbh & status %in% include)

	for (i in 1:n) 
	{

		focal = censdata2[i,]

		if(is.na(focal$gx) | is.na(focal$gy) ) output[i,1:2] = NA  else 
		{

			poly= with(focal, spoints(c(gx-r, gy-r, gx-r, gy+r, gx+r, gy+r, gx+r, gy-r)))
			use = inpip(spoints(rbind(spd$gx,spd$gy)), poly)

			if (length(use)==0) output[i,1:2]=0 else 
			{

				incircle = sqrt(dsquare(spoints(rbind(spd$gx[use],spd$gy[use])), spoints(rbind(focal$gx,focal$gy)) )) <= r
				nn = use[incircle]  #   & spd$tag[use] != focal$tag[i]

				area = CalcRingArea(data.frame(gx=focal$gx,gy=focal$gy), r, plotdim)$each 
				BA = circlearea(spd$dbh[nn]/20) # div by 2 for radius * 10 to convert mm to cm
				consp <- spd$sp[nn]==focal$sp

				output[i,1] = sum(BA[consp], na.rm=TRUE) * (pi*r^2 /area)
				output[i,2] = sum(BA[!consp], na.rm=TRUE) * (pi*r^2 /area)
			} 
		} 

		if (i %in% seq(5000,n+5000,5000))  cat( i, "of", n, " elapsed time = ", (proc.time()-ptm)[3], "seconds" , "\n") 

	}

}  # end of basal subrutine

cat( "Total elapsed time = ", (proc.time()-ptm)[3], "seconds" , "\n")
return(output)
}

# </source>
# </function>


# <function>
# <name>
# NDcount
# </name>

# <description>
# Calculates the count of neighbors within radius r with edge effect correction. Is a quick version of NeighborDensities with limited capabilities. <br>
# Dependencies: CalcRingArea from the CTFS package and splancs. <br>
# Output: a single vector with neighbor counts within radius r for each individual in censdata. <br>
# Author: Tania Brenes
# </description>

# <arguments>
# <ul>
#  <li> censdata: Is the coordinates dataset, should have the coordinates (gx, gy)
#  <li> r (20): radius 
#   <li> plotdim  (c(1000,500)):  vector with two numbers indicating the xy-plot size;
# </ul>
# </arguments>

# <sample>
# ## count all the consp neighbors of one species <br>
# > one.sp = subset(bci.full7, sp=="ingasa" & status=="A")<br>
# > neighbor.counts <- NDcount(one.sp)<br>
# </sample>

# <source>
NDcount=function(censdata, r=20, plotdim=c(1000,500))
{

sppi.pts <- with(censdata, data.frame(gx,gy))
areas <- CalcRingArea(sppi.pts, r, plotdim)$each

poly=spoints(c(0,0,plotdim[1],0,plotdim[1],plotdim[2],0,plotdim[2]))
counts <- khat(sppi.pts, poly=poly,r, newstyle=TRUE)$counts
output = counts * pi * r^2 /areas

return(as.vector(output)) 
}
# </source>
# </function>


