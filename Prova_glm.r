install.packages("dismo")
install.packages("dplyr")
install.packages("ggplot2")

library(dismo)
library(dplyr)
library(ggplot2)

hoodeed_warb_data <- read.csv2 ("hooded_warb_location.csv")
env_data_current <-stack ("env_current.grd")
env_dataforecast <-stack ("env_forecast.grd")
plot(env_data_currenti$precip)

hooded_warb_location <- select (hooded_warb_data, lon, lat)
hooded_warb_env <-extract( env_data_current, hooded_warb_location)
hooded_warb_data <- cbind (hooded_warb_data, hood_warbenv)


ggplot(hooded_warb_data,
       mapping=aes(x=tmin, y=precip, color = present))+
   geom_pont()


logistic_regr_model= glm (present ~ tmin + precip,
                          family=binomial(link="logit"),
                          data=hooded_warb_data)


summary(logistic_regr_model)


presence_data <- filter(hooded_warb_data, present == 1)
absence_data <- filter(hooded_warb_data, present== 0)

evaluation = evaluate (presence_data, absence_data, logistic_regr_model)

plot (evaluation, "ROC")

predictions <- predict (env_data_current,
                        logistic_regr_model,
                        type="response")
                        
plot(predictions, ext=extent (-140, -50, 25, 60))
points (presence_data[c("lon","lat")], pch="+", cex=0.5)

plot(predictions > 0.5, ext (-140,-50,25,60))

tr <- threshold(evaluation, stat="prevalence")
plot(predictions > tr, ext=extent (-140,-50,25,60))
points(presence_data[c("lon","lat")], pch="+", cex=0.5)

forecast =predict(env_data_forecast,
                  logistic_regr_model,
                  type ="response")

plot(forecast, ext=extent(-140,-50,-25,60))
plot(forecast > tr, ext=extent(-140,-50,-25,60))
plot(forecast - predictions, ext=extent(-140,-50,-25,60))
