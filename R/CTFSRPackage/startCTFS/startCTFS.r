# <function>
# <name>
# startCTFS
# </name>
# <description>
# Function to source all components of CTFS R package. Load all CTFS packages into a folder,
# then when opening R, execute this function, passing the folder name.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
startCTFS=function(folder)
{
	source(paste(folder,'/abundance/abundance.r',sep=''))
	source(paste(folder,'/biomass/biomass.CTFSdb.r',sep=''))
	source(paste(folder,'/demogchange/abund.fit.CTFS.r',sep=''))
	source(paste(folder,'/demogchange/demogChange.r',sep=''))
	source(paste(folder,'/growth/growth.r',sep=''))
	source(paste(folder,'/growth/growthfit.bin.r',sep=''))
	source(paste(folder,'/growth/growthfit.graph.r',sep=''))
	source(paste(folder,'/map/map.r',sep=''))
	source(paste(folder,'/map/mapresponse.r',sep=''))
	source(paste(folder,'/mortality/mortality.r',sep=''))
	source(paste(folder,'/recruitment/recruitment.r',sep=''))
	source(paste(folder,'/spatial/NeighborDensityFun.r',sep=''))
	source(paste(folder,'/spatial/RipUvK.r',sep=''))
	source(paste(folder,'/spatial/block.analysis.r',sep=''))
	source(paste(folder,'/spatial/quadfunc.r',sep=''))
	source(paste(folder,'/speciesarea/spparea.r',sep=''))
	source(paste(folder,'/startCTFS/startCTFS.r',sep=''))
	source(paste(folder,'/topography/imageJ.r',sep=''))
	source(paste(folder,'/topography/slope.r',sep=''))
	source(paste(folder,'/topography/solvetopo.r',sep=''))
	source(paste(folder,'/utilities/calcalpha.r',sep=''))
	source(paste(folder,'/utilities/distributions.r',sep=''))
	source(paste(folder,'/utilities/geometry.r',sep=''))
	source(paste(folder,'/utilities/lmerBayes.r',sep=''))
	source(paste(folder,'/utilities/modelBayes.r',sep=''))
	source(paste(folder,'/utilities/statistics.r',sep=''))
	source(paste(folder,'/utilities/utilities.r',sep=''))
	source(paste(folder,'/utilities/utilitiesCTFS.r',sep=''))
}
# </source>
# </function>
