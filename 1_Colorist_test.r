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

