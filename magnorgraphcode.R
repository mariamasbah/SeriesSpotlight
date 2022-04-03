
library(sp)
library(scales)
library(aqp)
library(soilDB)
library(sharpshootR)
library(dendextend)
library(latticeExtra)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(plyr)
library(igraph)

# comparing moist and dry colors
soils <- c('magnor')

s <- fetchOSD(soils)

s.dry <- fetchOSD(soils, colorState = 'dry')

par(mar=c(1,0,2,1), mfrow=c(1,2))

plotSPC(s, cex.names = 1, plot.depth.axis=FALSE) ; title('Moist Colors')

plotSPC(s.dry, cex.names = 1) ; title('Dry Colors')


# climate summaries
soils <- c('magnor')
s <- fetchOSD(soils, extended = TRUE)

cols <- brewer.pal(9, 'Set1') 
cols <- cols[c(1:5,7,9)]

trellis.par.set(plot.line = list(col = 'RoyalBlue'))

res <- vizAnnualClimate(s$climate.annual, IQR.cex = 1.25, cex=1.1, pch=18)

print(res$fig)

# hillslope position

res <- vizHillslopePosition(s$hillpos)
print(res$fig)

res <- vizGeomorphicComponent(s$geomcomp)
print(res$fig)

#siblings

s <- siblings('magnor', component.data = TRUE)
kable_styling(kable(s$sib, format = 'html'), full_width = FALSE)

s.list <- unique(c(s$sib$series[1], s$sib$sibling))
h <- fetchOSD(s.list)

par(mar = c (0,0,0,0), mfrow=c(1,1))
SoilTaxonomyDendrogram(h, y.offset = 0.4, width = 0.2)

#cousins 

s <- siblings('magnor', component.data = TRUE, cousins = TRUE)

d <- unique(rbind(s$sib.data, s$cousin.data))

d <- d[which(d$compkind == 'Series'), ]

m <- component.adj.matrix(d, mu = 'mukey', co = 'compname', wt = 'comppct_r')

par(mar=c(1,1,1,1))
plotSoilRelationGraph(m, s='magnor', vertex.scaling.factor=1, edge.transparency=0.25, edge.col=grey(0.5), edge.highlight.col='black', vertex.label.family='sans', spanning.tree='max')

#soil component relations

magnor_mu <- SDA_query("SELECT DISTINCT mapunit.mukey FROM component
INNER JOIN mapunit ON component.mukey = mapunit.mukey
WHERE compname LIKE '%Magnor%'")

magnor <- SDA_query(paste0("SELECT mapunit.mukey, compname, comppct_r FROM component
LEFT JOIN mapunit ON component.mukey = mapunit.mukey
WHERE mapunit.mukey IN ", format_SQL_in_statement(magnor_mu$mukey)))

m.1 <- component.adj.matrix(magnor, method='occurrence')
print(m.1)

idx <- order(m.1[,which(colnames(m.1) == "Magnor")], decreasing = TRUE)[1:10]
magnor.idx <- which(rownames(m.1) == "Magnor")[1]
if (!magnor.idx %in% idx) {
  idx <- c(idx, magnor.idx)
}
m.small <- m.1[idx, idx]

par(mfcol=c(1,1), mar=c(0,0,1,0))
plotSoilRelationGraph(m.small, s='Magnor')

m.2 <- component.adj.matrix(magnor)
print(round(m.2, 2))

#interactive map

magnor <- seriesExtent('magnor')

s <- c('magnor')

cols <- brewer.pal('Set1', n = length(s))

s.extent <- lapply(s, seriesExtent, timeout=120)

s.extent <- do.call('rbind', s.extent)


s.extent <- st_as_sf(s.extent)

library(mapview)
mapview(s.extent, zcol = 'series', legend = TRUE)

#soil extent map

cols <- brewer.pal('Set1', n=3)


magnor <- seriesExtent('magnor')

par(mar=c(1,0,1,0))
map(database='county', regions='wisconsin')
plot(magnor, col=cols[2], border=cols[2], add=TRUE)
box()
title(main='Magnor Series Extent', line=1.25)
mtext(side=1, text=as.character(Sys.Date()), line=0)

res <- SDA_query("SELECT component.mukey, muname, compname, comppct_r from laoverlap JOIN muaoverlap ON laoverlap.lareaovkey = muaoverlap.lareaovkey JOIN mapunit ON muaoverlap.mukey = mapunit.mukey JOIN component ON muaoverlap.mukey = component.mukey WHERE compname = 'magnor' AND areatypename = 'MLRA' AND areasymbol = '31'")
kable(res, row.names = FALSE)

#soil extent table
res <- SDA_query("SELECT compname, areasymbol as mlra, areaname as mlra_name, count(compname) as n_components from laoverlap JOIN muaoverlap ON laoverlap.lareaovkey = muaoverlap.lareaovkey JOIN component ON muaoverlap.mukey = component.mukey WHERE compname = 'magnor' AND areatypename = 'MLRA' GROUP BY compname, areasymbol, areaname ORDER BY count(compname) DESC")
kable(res, row.names = FALSE)


