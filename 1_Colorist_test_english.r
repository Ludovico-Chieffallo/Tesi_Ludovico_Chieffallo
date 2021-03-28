#The basic workflow for colorist is as follows:

#-Metrics: Users calculate metrics to describe their distributions.
#-Color palette: Users choose a color palette to enable visualization of metrics.
#-Map: Users combine metrics and a palette to map distributions in a series of small multiples or in a single map.
#-Legend: Users generate a legend to accompany their map


#The principal difference between these two classes is that a RasterBrick can only be linked to a single (multi-layer) file. In contrast, a RasterStack can be formed from separate files and/or from a few layers (‘bands’) from a single file.


install.packages("colorist")
library(colorist)

#load example data, field sparrow occurence probability

data("fiespa_occ")
fiespa_occ
plot(fiespa_occ)
dev.off()

#calculate distribution metrics
met<- metrics_distill(fiespa_occ)

#generate hcl color palette

pal<- palette_timecycle(fiespa_occ)

#map
map_single(met,pal,lambda_i = -2)

#legend
legend_timecycle(pal)



