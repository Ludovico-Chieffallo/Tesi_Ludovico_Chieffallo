#The "colorist" (R) package was created to provide additional methodologies and options for studying and communicating information on the distribution of wildlife in space and time.
#To do this "colorist" uses Rasterstack images that describe wildlife distributions, processes and links them to HCL palettes in specific ways.
#Resulting visualizations allow viewers to meaningfully compare values of occurrence, abundance, or density over space and time.


#What is the HCL palette? The Hue-Chroma-Luminance (HCL) color space is an alternative to other color spaces such as RGB, HSV, and so on.
#Each color within the HCL color space is defined by a triplet of values. 
#The dimensions are:

   #Hue: defines the color (hue)
   #Chroma: defines the color (saturation or intensity of the color)
   #Luminance: defines the brightness
   #(https://hclwizard.org/images//hclscheme_pic0.png)

#The basic workflow for colorist is:

   # 1.) Metrics: Users calculate metrics to describe their distributions.
   # 2.) Color palette: Users choose a color palette to enable the display of metrics.
   # 3.) Map: Users combine metrics and palette to map distributions into a single map or more.
   # 4.) Legend: Users generate a legend to accompany their map.

#First, we can define the difference between RasterBrick and Rasterstack.
#The main difference between RasterBrick and Rasterstack is that RasterBrick can only be linked to a single file (multilayer, such as satellite images).
#Conversely, a RasterStack can consist of separate files and / or a few layers ("bands") from a single file.

install.packages("colorist")
library(colorist)



#EXAMPLE 1: MAPPING A DISTRIBUTION OF SPECIES IN THE ANNUAL CYCLE

#Here, we use eBird status and trend aggregate data for Field Sparrow (Spizella pusilla) to illustrate a different strategy for creating annual cycle maps, which leverages continuous occurrence data to describe where and when viewers might be able to find a species.

#Load the example using the data function

data("fiespa_occ")
fiespa_occ
#class      : RasterStack 
#dimensions : 193, 225, 43425, 12  (nrow, ncol, ncell, nlayers)
#resolution : 14814.03, 14814.04  (x, y)
#extent     : -1482551, 1850606, -1453281, 1405830  (xmin, xmax, ymin, ymax)
#crs        : +proj=laea +lat_0=38.7476367322638 +lon_0=-90.2379515912106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
#names      :       jan,       feb,       mar,       apr,       may,       jun,       jul,       aug,       sep,       oct,       nov,       dec 
#min values :         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0 
#max values : 0.8538026, 0.8272926, 0.7993844, 0.7805922, 0.7799550, 0.7745436, 0.7626938, 0.7867995, 0.7790458, 0.7896419, 0.8158410, 0.8681034


1) We can Calculate the metrics

met1<-metrics_pull(fiespa_occ) #This function transforms rasterstack values describing individual distributions or species distributions into standardized intensity values.
print(met1)
#class      : RasterBrick 
#dimensions : 193, 225, 43425, 12  (nrow, ncol, ncell, nlayers)
#resolution : 14814.03, 14814.04  (x, y)
#extent     : -1482551, 1850606, -1453281, 1405830  (xmin, xmax, ymin, ymax)
#crs        : +proj=laea +lat_0=38.7476367322638 +lon_0=-90.2379515912106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
#source     : memory
#names      :       jan,       feb,       mar,       apr,       may,       jun,       jul,       aug,       sep,       oct,       nov,       dec 
#min values :         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0 
#max values : 0.9835264, 0.9529886, 0.9208400, 0.8991927, 0.8984586, 0.8922251, 0.8785749, 0.9063431, 0.8974113, 0.9096173, 0.9397971, 1.0000000


#2.)We can create a palette (HCL).

#We want to choose a palette that helps communicate temporal information about the occurrence of Field Sparrow.
#We will use the palette_timecycle () function because our data represent an ordered and cyclic sequence (all months of the year)


pal<- palette_timecycle(fiespa_occ)
head(pal)   #we use head () to return the first values. "Pal" has 1212 objects, with head for example we take the first 6

#  specificity layer_id   color
#1           0        1 #6A6A6A
#2           0        2 #6A6A6A
#3           0        3 #6A6A6A
#4           0        4 #6A6A6A
#5           0        5 #6A6A6A
#6           0        6 #6A6A6A   


# We have seen how the palette_timecycle function returns us a data frame with three fields: specificy layer_id and color
#The specificity and layer_id fields will be used to assign colors to specific raster cells


#3.) Now we can create a map.

#With the map_multiples () function we will be able to see the map for each layer, therefore for each month of the year
#Inside the parenthesis we will write 1.) The metric / 2.) The palette used / 3.) ncol = x to define the number of columns to display / 4.) Labels = names (filename)

map_multiples(met1, pal, ncol = 3, labels = names (fiespa_occ))


#If we want to extract a month of data for deeper analysis, we can use map_single () and specify which month of data we would like to see using the layer argument.

map_single(met1, pal, layer = 6)

# To generate a single annual cycle map that summarizes space-time information on the occurrence of Field Sparrow, we need to "distill" the distributive information into our RasterStack.
We can use metrics_distill ()

#The metrics_distill () function calculates distribution metrics on all levels in each raster cell and returns three metrics for later viewing:

# -Maximum intensity (that is, the maximum value of occurrence, abundance, or density).
# -Maximum intensity layer (hence the identity of the layer containing the maximum intensity value)
# -Specificity of the maximum intensity value for the maximum intensity layer (hence the degree to which the intensity values are unevenly distributed between the layers).

met1_distill<-metrics_distill(fiespa_occ) # we can distill the information
map_single(met1_distill,pal) #We then display the information in the single image with the "distilled" images and the palette created previously


# 4.) Finally we create the legend.
# previously we used the palette_timecycle () function and consequently we will use the legend_timecycle () function to display the legend
#R will return a legend with circles.
Information about when the time cycle begins (and ends) can also be provided in the "origin_label" argument.

legend_timecycle(pal, origin_label = "jan 1")


#CONCLUSIONS
#With all the information described, with the map and with the legend, we are able to understand where and when to find this species.
#The more colored parts will indicate high specificity but high probability of occurrence only at certain times of the year
#The gray parts, on the other hand, will indicate low specificity but high occurrence throughout the year
# In this case, low specificity indicates seasonality where 0 indicates presence all year round




#EXAMPLE 2: MAPPING INDIVIDUAL BEHAVIOR OVER TIME


#Here we explore how a Fisher individual (Pekania pennanti), who lives in upstate New York, moved into his local environment over a period of nine sequential nights in 2011.


#Load the data

data("fisher_ud")   
fisher_ud
#class      : RasterStack 
#dimensions : 176, 177, 31152, 9  (nrow, ncol, ncell, nlayers)
#resolution : 25, 25  (x, y)
#extent     : -2282.343, 2142.657, 5100266, 5104666  (xmin, xmax, ymin, ymax)
#crs        : +proj=moll +lon_0=-73.4137066015374 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs 
#names      :      night1,      night2,      night3,      night4,      night5,      night6,      night7,      night8,      night9 
#min values :           0,           0,           0,           0,           0,           0,           0,           0,           0 
#max values : 0.004695207, 0.007591029, 0.006749434, 0.002891691, 0.002833876, 0.004711692, 0.002017082, 0.002700729, 0.005282948 



#Extract the information from the Rasterstack

m2<-metrics_pull(fisher_ud)
m2

#Create the palette, this time we use the palette_timeline function because unlike the previous example, they describe a linear and non-cyclic sequence

pal2<-palette_timeline(fisher_ud)
head(pal2)

#We create maps (9, one for each day) that will describe the position of the individual in each day

map_multiples(m2,ncol = 3, pal2)

#It often happens that maps are faded.
#The lambda_i parameter in map_multiples () and map_single () allows users to visually adjust the relative weights of high and low intensity values.
#In this case we want to reduce the disparity between high and low intensity values so we set the value to -5.
#We will see that the lower the lambda_i value, the more intense the color will be

map_multiples(m2,ncol = 3, pal2, lambda_i = -5)


#At this point we can distill the information
#It is important to remember that specificity values (and the resulting display of specificity values) for Fisher must be interpreted differently than for Field Sparrow.
#Here, specificity values indicate the degree to which the individual used the same locations within the landscape for nine nights. Low specificity suggests consistent use over time.

m2_distill<-metrics_distill(fisher_ud)
map_single(m2_distill,pal2,lambda_i = -5)


#We used linear values so we will use legend_timeline and not legend_timecycle
#In this case we can use time_labels to determine the investigation period.

legend_timeline(pal2,time_labels = c("2 aprile", "11 aprile"))



#EXAMPLE 3: MAPPING THE DISTRIBUTIONS OF MULTIPLE INDIVIDUALS DURING THE SAME TIME PERIOD

# In the previous examples, we explored the distributions of a single species and a single individual over multiple time periods.
#colorist can also be used to explore the distribution of multiple species or individuals in a single period of time.

#Here, we use GPS tracking data collected from African elephants (Loxodonta africana) in Etosha National Park (Namibia) during 2011 to explore how two individuals used the landscape over the course of a year.


#Load the data

data("elephant_ud")
elephant_ud
#class      : RasterBrick 
#dimensions : 208, 193, 40144, 2  (nrow, ncol, ncell, nlayers)
#resolution : 500, 500  (x, y)
#extent     : -58642.18, 37857.82, -2376938, -2272938  (xmin, xmax, ymin, ymax)
#crs        : +proj=moll +lon_0=15.8231920275931 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs 
#source     : memory
#names      :      LA11,      LA14 
#min values :         0,         0 
#max values : 0.8525854, 1.0000000 


#Extract the data from the Rasterstack

met3<-metrics_pull(elephant_ud)


#Here we encounter another function, palette_set ()
#We use this function when we have an unordered set (they don't fit into linear or cyclic sequences)

pal3<-palette_set(elephant_ud)


#We can Create the multiple maps and then we will see the two samples, we use "labels" to extract the names present in the Rasterstack

map_multiples(met3, pal3, ncol = 2,lambda_i = -5,labels = names(elephant_ud))



#The usage distributions for LA11 and LA14 look remarkably similar at first glance, which is not surprising given that the two elephants belong to the same herd.
#However, we can more clearly understand the similarities and differences in how they used space in 2011 by “distilling” the distributional information from the two raster layers with metrics_distill () and visualizing the resulting metrics with map_single ().

#WE SHOULD RE-CALIBRATE OUR UNDERSTANDING OF THE MEANING OF SPECIFICITY

#We stand in contrast to the Field Sparrow and Fisher examples, where specificity indicated the degree to which intensity values were inconsistent (or consistent) over time.
#HERE WE CAN DEFINE IT AS A DIFFERENTIAL LANDSCAPE MEASURE.
#THEREFORE LOW SPECIFICITY IS EQUAL TO COMMON USE OF THE LANDSCAPE, HIGH SPECIFICITY INDICATES A DIFFERENTIAL USE OF THE LANDSCAPE

met3_distt<-metrics_distill(elephant_ud)
map_single(met3_distt,pal2,lambda_i = -5)

#Here we will use a different legend once again.
#We will use legend_set and not legend_timeline or legend_timecycle 

legend_set(pal3, group_labels = names(elephant_ud))
