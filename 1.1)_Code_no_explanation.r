install.packages("colorist")
library(colorist)

#EXAMPLE 1: MAPPING A DISTRIBUTION OF SPECIES IN THE ANNUAL CYCLE

data("fiespa_occ")
fiespa_occ
met1<-metrics_pull(fiespa_occ)  
print(met1)
pal<- palette_timecycle(fiespa_occ)
head(pal)
map_multiples(met1, pal, ncol = 3, labels = names (fiespa_occ))
map_single(met1, pal, layer = 6)
met1_distill<-metrics_distill(fiespa_occ)  
map_single(met1_distill,pal)               
legend_timecycle(pal, origin_label = "jan 1")



#EXAMPLE 2: MAPPING INDIVIDUAL BEHAVIOR OVER TIME

data("fisher_ud")   
fisher_ud
m2<-metrics_pull(fisher_ud)
m2
pal2<-palette_timeline(fisher_ud)
head(pal2)
map_multiples(m2,ncol = 3, pal2)
map_multiples(m2,ncol = 3, pal2, lambda_i = -5)
m2_distill<-metrics_distill(fisher_ud)
map_single(m2_distill,pal2,lambda_i = -5)
legend_timeline(pal2,time_labels = c("2 aprile", "11 aprile"))



#EXAMPLE 3: MAPPING THE DISTRIBUTIONS OF MULTIPLE INDIVIDUALS DURING THE SAME TIME PERIOD

data("elephant_ud")
elephant_ud
met3<-metrics_pull((elephant_ud))
pal3<-palette_set(elephant_ud)
map_multiples(met3, pal3, ncol = 2,lambda_i = -5,labels = names(elephant_ud))
met3_distt<-metrics_distill(elephant_ud)
map_single(met3_distt,pal2,lambda_i = -5)
legend_set(pal3, group_labels = names(elephant_ud))
