# <function>
# <name>
# wavelet.allsp
# </name>

# <description>
# Function to calculate the wavelet variance curves for all species in one plot using quadrat indexation from CTFS package.
#
# Author: Matteo Detto and Tania Brenes <br> 
#
# Output is a matrix with:  <br> 
# scale: vector with scale of the analysis in meters;<br> 
# variance: matrix with the normalized variance at each scale (columns) for each species (rows);<br> 
# density: matrix with the density per area and abundance for each species in the plot;<br> 
# plotdim: plot dimentions parameter used;<br> 
# gridsize: grid size parameter used;<br> 
# UCL: vector with the upper confidence limit for the null hypothesis;<br> 
# LCL: vector with the lower confidence limit for the null hypothesis.<br> 
# </source>

# </description>

# <arguments>
#   censdata (): census data for the plot containing the variables gx, gy, dbh, status, and sp code;<br> 
#   plotdim c(1000,500):  vector with two numbers indicating the plot size;<br> 
#   gridsize (2.5): gives the size of the quadrats for the rasterization
#   mindbh (NULL): if analysis is to be done at different size  classes
# </arguments>

# <sample>
# load("bci.full1.rdata")<br> 
# wavelet.variances = wavelet.allsp(censdata, plotdim=c(1000,500))
# </sample>

# <source>
wavelet.allsp = function(censdata, plotdim=c(1000,500), gridsize=2.5, mindbh=NULL)
{
ptm <- proc.time()

	if (is.null(mindbh)) censdata = subset(censdata, status=="A" & !is.na(gx) & !is.na(gy) & !duplicated(tag))   
	else 	censdata = subset(censdata, status=="A" & !is.na(gx) & !is.na(gy) & !duplicated(tag) & dbh>=mindbh) 

censdata$sp = factor(censdata$sp)
splitdata = split(censdata, censdata$sp)

n = length(splitdata)

variance = numeric()				# matrix for normalized variance
sp.density = matrix(NA, ncol=2, nrow=n)		# matrix for species density
dimnames(sp.density) = list(names(splitdata), c("number", "density"))

plot(c(0,100), c(0,100), type='n')

		for (i in 1:n) 
		{
		coords = with( splitdata[[i]], data.frame(gx,gy) )
		x = wavelet.var(coords=coords, plotdim=plotdim, gridsize=gridsize, k0=8, dj=0.15, graph=FALSE)
		
		variance = rbind(variance, x$E_norm)  
		sp.density[i,1] = nrow(splitdata[[i]])
		sp.density[i,2] = nrow(splitdata[[i]])/(plotdim[1]*plotdim[2])
		lines(x$scale, x$E_norm)

		if (i %in% seq(10,n+10,10))  cat( i, "of", n, " elapsed time = ", 			(proc.time()-ptm)[3]/60, "minutes" , "\n") 
		}

dimnames(variance) <- list(names(splitdata), paste("scale",1:ncol(variance)))

cat( "Total elapsed time = ", (proc.time()-ptm)[3]/60, "minutes" , "\n")

return(list(scale = x$scale, variance = variance, density= sp.density, plotdim=plotdim, gridsize=gridsize, UCL=x$UCL, LCL=x$LCL)) }
# </source>
# </function>


# <function>
# <name>
# plot.wavelet
# </name>

# <description>
# Function to plot the wavelet variance from the output of the wavelet.allsp. 
# Author: Tania Brenes <br> 
# </description>
# <arguments>
#   x (): output for wavelet.allsp;<br> 
# </arguments>

# <sample>
# </sample>

# <source>
plot.wavelet = function(x) {
plot(c(2*x$gridsize, min(x$plotdim)), c(0.3,100), log='xy', type='n')
		for (i in 1:nrow(x$variance)) {
		lines(x$scale, x$variance[i,])
		}
}
# </source>
# </function>


# <function>
# <name>
# rasterize
# </name>

# <description>
# Function to calculate the count (type='point'), basal area or agb (type='marked') per quadrat 
# by selecting quadrats of variable sizes. <br> 
# Author: Matteo Detto and Tania Brenes <br> 
# Output is a list containing a matrix with the raster data.
# </description>

# <arguments>
#   x , y : x and y coordinates for the point pattern; <br> 
#   z : marks for the marked point process, can be basal area, agb, etc. Needed only for type='marked'; <br> 
#   gridsize  (20 m): size of the quadrats for the rasterization, ;<br>
#   plotdim c(1000,500):  vector with plot x-y size;<br> 
#   type ('point'): type of rasterization: 'point' runs a simple point pattern counting the number of individuals per quadrat; 'marked' does a marked point pattern, where a function (FUN) is applied to the variable z in each quadrat, for example a sum of basal areas;
#   FUN (sum): function to apply to the point pattern when z is provided. By defult it sums the values of z per quadrat;<br> 
#   graph (FALSE): logical, plot the heat map of raster data?;<br> 
# </arguments>

# <sample>
# load("bci.full1.rdata") <br>
# attach("/home/brenest/Documents/Windocs/WorkFiles/R/Functions/CTFSRPackage.rdata")  <br> 
# onesp = subset(bci.full1, sp=="rinosy") <br> 
# ## plots the density of the sp in the plot <br> 
# rast1 = rasterize(x= onesp$gx, y=onesp$gy, gridsize=5, plotdim=c(1000,500), type='point', graph=TRUE) <br> 
# </sample>

# <source>
rasterize = function(x, y, z=NULL, gridsize=20, plotdim=c(1000,500), FUN=sum, graph=FALSE)
{

XI= seq(0, plotdim[1], gridsize)
YI= seq(0, plotdim[2], gridsize)

K = length(XI)-1
J = length(YI)-1

quad.indices = gxgy.to.index(x, y, gridsize=gridsize, plotdim=plotdim)
quad.indices = factor(quad.indices, levels=1:(K*J))

## count of individuals in each block
if (is.null(z)) {
f = matrix(tapply(x, quad.indices, length), ncol=K, nrow=J)
} # end of of count rutine

else {
# marked point process
f = matrix(tapply(z, quad.indices, FUN), ncol=K, nrow=J)
} # end of marked loop

f[is.na(f)]  <- 0
dimnames(f) <- list(YI[1:J], XI[1:K])

if (graph==TRUE) image(XI[1:K]+gridsize/2, YI[1:J]+gridsize/2, t(f)) 

return(f)
}
# </source>
# </function>


# <function>
# <name>
# wavelet.univariate
# </name>

# <description>
# Function to calculate the univariate wavelet variance using furier transforms. 
# It accepts a raster data or a point pattern, which is the default if raster is not provided.  
# The wavelet variance describes the spatial autocorrelation or aggregation of tree distribution.
# A wavelet variance greater than 1 indicates scales at which individuals 
# are aggregated. A wavelet variance less than 1, indicates scales at which 
# individuals are dis-aggregated. A wavelet variance equal to 1, indicates scales 
# at which individuals are randomply distribuited (as Poisson process). <br> 
# A graphical test is implemented on the null hypothesis of complete randomness. 
# If the wavelet variance is out of the conf bounds the tree distribution 
# is significantly different from a random process. <br> 
#
# Dependencies: needs the package 'spatstat' and the CTFSRpackage<br>
#
# Authors: Matteo Detto and Tania Brenes <br> 
#
# Output: a list containing vectors for the wavelet variance, the scale of the wavelet variance, the normalized variance, and the confidence intervals.<br> 
# </description>

# <arguments>
#  raster (NULL): used if data is entered already as a raster matrix
#  coords  (): an alternate to a raster table, a table with two (or three) columns giving coordinates x, y (and an optional mark) in that order. This is used to calculate a raster; <br> 
#  gridsize : is the quadrat size of the rasterization;<br>
#  plotdim (c(1000,500)): the dimensions of the plot;<br>
#  FUN (NULL): function to apply to the marked point pattern, if function is not specified (default) it counts points. E.g. can be used to sum the basal areas or above graound biomass
#  k0 (8): numeric. smoothing parameter of the wavelet filter (k0 between 5.5-15), lower values of k0 produce a smoother wavelet variance;<br> 
#  dj  (0.15):  numeric. discretization of the scale axis;<br> 
#  graph (FALSE): logical. If TRUE, plots the wavelet variace as a function of scale <br>
# </arguments>

# <sample>
# load("bci.full1.rdata") <br>
# rast1 = rasterize(, gridsize=5, plotdim=c(100,500), graph=TRUE)<br>
# wv = wavelet.var(coords=bci.full1[,c("gx","gy")], k0=8, dj=0.15, graph=TRUE)<br>
# plots the scale of aggregation<br>
# </sample>

# <source>
wavelet.univariate=function(raster=NULL, coords, gridsize=2, plotdim=c(1000,500), FUN=NULL, k0=8, dj=0.15, graph=FALSE) {

if ( is.null(raster) ) 
{
	if ( is.null(FUN) )   
	{
	raster = rasterize(coords[,1], coords[,2], gridsize=gridsize, plotdim=plotdim)
	}
	else
	{
	raster = rasterize(coords2[,1], coords2[,2], coords2[,3], gridsize=gridsize, plotdim=plotdim, FUN=FUN)
	}
}

m = dim(raster)[1] ; n = dim(raster)[2]

fourier_factor=4*pi/(k0+sqrt(4+k0^2))
s0=2/fourier_factor

J1 = floor( (log(min(c(m/2,n/2))/s0)/log(2))/dj)-1
sc = s0*2^((0:J1)*dj)

S = J1+1

f11 = fft(raster) 
F11 = f11 * Conj(f11) 

npuls_2   = floor((n-1)/2)
pulsx     = 2*pi/n* c( 0:npuls_2 , (npuls_2-n+1):-1 )
npuls_2   = floor((m-1)/2)
pulsy     = 2*pi/m* c(0:npuls_2,  (npuls_2-m+1):-1 )

kx = t(matrix(pulsx, nrow=n, ncol=m))
ky = matrix(pulsy, nrow=m, ncol=n)

K = sqrt(kx^2 + ky^2)
dkx=kx[1,2]-kx[1,1]
dky=ky[2,1]-ky[1,1]

E11 =  norm = s11 = numeric(S)
		for (i in 1:S) {
		H = abs(-exp(-1/2*(sc[i]*K-k0)^2))^2
		s11[i]=sd(as.vector(F11*H))/sum(H)
		E11[i]=sum(F11*H)/sum(H)/m/n
		norm[i]=sum(H)*dkx*dky*sc[i]^2/(2*pi)^2
		}

sc=sc*fourier_factor*gridsize
N = sum(raster)/m/n
E_norm = Re(E11/N)
dof = norm*(m*n * gridsize^2) * fourier_factor^2 /sc^2 
SE = s11/sqrt(dof)
UCL = qchisq( 0.975, df= dof-1)/dof
LCL = qchisq( 0.025, df= dof-1)/dof
		
output=list(variance=E_norm, scale=sc, UCL=UCL, LCL=LCL)

		if (graph == TRUE) {
		plot(sc, E_norm, log = "xy", type='l', ylim=c(0.1,100))
		lines(sc, UCL, lty=3)
		lines(sc, LCL, lty=3)
		abline(h=1, lty=2)
		}

return(output)	}
# </source>
# </function>


# <function>
# <name>
# wavelet.bivariate
# </name>

# <description>
# Function to calculate the wavelet variance to evaluate the association between two point patterns using furier transforms. 
# It accepts a raster data or a point pattern, but the type of data entered has to be specified in the argument type.  
# The wavelet variance describes the spatial autocorrelation or aggregation of point distribution.
# A wavelet variance greater than 1 indicates scales at which individuals 
# are aggregated. A wavelet variance less than 1, indicates scales at which 
# individuals are dis-aggregated. A wavelet variance equal to 1, indicates scales 
# at which individuals are randomply distribuited (as Poisson process). <br> 
# A graphical test is implemented on the null hypothesis of comple randomness. 
# If the wavelet variance is out of the conf bounds the point distribution 
# is significantly different from a random process. <br> 
# Dependencies: needs the package 'spatstat' and the CTFSRpackage <br>
# Authors: Matteo Detto and Tania Brenes <br> 
# Output: a list containing vectors for the wavelet variance, the scale of the wavelet variance, and the normalized variance.<br> 
# </description>

# <arguments>
#  coords1  (): a matrix with raster data OR a table with two or three columns that can be used to calculate a raster, the first two columns are the coordinates and the third is the mark. Type must be specified ; <br> 
#  coords2 (): 2nd dataset for the bivariate analyisis;<br> 
#  type ('raster'): the type of data entered, 'raster' if a raster matrix, 'point' for a point pattern, or 'marked' for a marked point pattern;<br>
#  gridsize : is the quadrat size of the rasterization;<br>
#  plotdim (c(1000,500)): the dimensions of the plot;<br>
#  FUN ('sum'): function to apply to the marked point pattern, by default it sums the values as would be used for sum of basal areas or sum of above graound biomass
#  k0 (8): numeric. smoothing parameter of the wavelet filter (k0 between 5.5-15), 
# lower values of k0 produce a smoother wavelet variance;<br> 
#  dj  (0.15):  numeric. discretization of the scale axis;<br> 
#  graph (TRUE): logical. plot the wavelet variace ? <br>
# </arguments>

# <sample>
# load("bci.full1.rdata") <br>
# sp.one = subset(bci.full7, sp=="quaras")[,c("gx","gy")] <br>
# sp.two = subset(bci.full7, sp=="cordal")[,c("gx","gy")]<br>
# wv = wavelet.bivariate(coords=sp.one, coords2=sp.two, type='point', rk0=8, dj=0.15, graph=TRUE)<br>
# plots the scale of aggregation<br>
# </sample>

# <source>
wavelet.bivariate = function(raster1=NULL, raster2=NULL, coords1, coords2, gridsize=1, plotdim=c(1000,500), FUN=NULL, k0=8, dj=0.15, graph=TRUE) 
{
if ( is.null(raster1) ) 
{
	if ( is.null(FUN) )   
	{
	raster1 = rasterize(coords1[,1], coords1[,2], gridsize=gridsize, plotdim=plotdim)
	raster2 = rasterize(coords2[,1], coords2[,2], gridsize=gridsize, plotdim=plotdim)
	}
	else
	{
	raster1 = rasterize(coords1[,1], coords1[,2], coords1[,3], gridsize=gridsize, plotdim=plotdim, FUN=FUN)
	raster2 = rasterize(coords2[,1], coords2[,2], coords2[,3], gridsize=gridsize, plotdim=plotdim, FUN=FUN)
	}
}

m = dim(raster1)[1] ; n = dim(raster1)[2]

fourier_factor=4*pi/(k0+sqrt(4+k0^2))
s0=2/fourier_factor

J1 = floor( (log(min(c(m/2,n/2))/s0)/log(2))/dj)-1
sc = s0*2^((0:J1)*dj)

S = J1+1

f11 = fft(raster1) 
F11 = f11 * Conj(f11) 

		f22 = fft(raster2)
		F12 = f11 * Conj(f22)
		F22 = f22 * Conj(f22)

npuls_2   = floor((n-1)/2)
pulsx     = 2*pi/n* c( 0:npuls_2 , (npuls_2-n+1):-1 )
npuls_2   = floor((m-1)/2)
pulsy     = 2*pi/m* c(0:npuls_2,  (npuls_2-m+1):-1 )

kx = t(matrix(pulsx, nrow=n, ncol=m))
ky = matrix(pulsy, nrow=m, ncol=n)

K = sqrt(kx^2 + ky^2)
dkx=kx[1,2]-kx[1,1]
dky=ky[2,1]-ky[1,1]

E11 =  E12 =  E22 = norm = numeric(S)
		for (i in 1:S) {
		H = abs(-exp(-1/2*(sc[i]*K-k0)^2))^2
		E11[i]=sum(F11*H)/sum(H)/m/n
		E12[i]=sum(F12*H)/sum(H)/m/n
		E22[i]=sum(F22*H)/sum(H)/m/n
		norm[i]=sum(H)*dkx*dky*sc[i]^2/(2*pi)^2
		}

sc=sc*fourier_factor*gridsize
N1 = sum(raster1)/m/n
N2 = sum(raster2)/m/n
r = Re(E12/sqrt(E11*E22))
coh = r/sqrt(1-r^2)
dof = norm*(m*n * gridsize^2) * fourier_factor^2 /sc^2 
UCL = qt( 0.975, df= dof-2)/sqrt(dof-2)
LCL = qt( 0.025, df= dof-2)/sqrt(dof-2)

output=list(variance=coh, scale=sc, UCL=UCL, LCL=LCL)

if (graph == TRUE) 
	{
	plot(sc, coh, log = "x", type='l', ylim=c(-1,1)) 
	lines(sc, UCL, lty=3)
	lines(sc, LCL, lty=3)
	abline(h=0, lty=2)  
	}

output=list(E_norm=coh, UCL=UCL, LCL=LCL, var11= as.real(E11), var12=as.real(E12), var22=as.real(E22), norm=norm, scale=sc)

return(output)	}
# </source>
# </function>

