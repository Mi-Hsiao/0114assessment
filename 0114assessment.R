setwd("/Users/Micheal/Desktop/UCL SGDS/GEOG0114/assessment/data")

# Load the packages with library()
library("tidyverse")
library("sf")
library("tmap")
library("nngeo")
library("spdep")
library("sp")
library("data.table")
library("spatialreg")
library("olsrr")

# load London borough data
boroughshp <- read_sf("London Borough Areas.shp")

# load data file
datafile <- read.csv(file = "merged_data.csv", header = TRUE, sep = ",")
child_obesity_data <- read.csv(file = "child_obesity_data.csv", header = TRUE, sep = ",")

# As it's mentioned in the report, the original data which is offered by london data store; the data provider combined data for city of london
#with Hackney to avoid disclosure of small numbers in ovese estimates
# so, here first we need to drop the data for city of london out from the borough shapefile
# delete the first row for city of london
borough <- boroughshp[-1,]

# inspect
tm_shape(borough) +
  tm_polygons()  +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) 

# join data together
# here we have created two new data frames, one is merged data, another is shp with only children obesity data, which is a smaller dataset,
# and this is for preliminary global moran's i test, by doing so, a smaller file can save some computing time, improve efficiency
obesity_data <- left_join(borough, datafile, by = c("BOROUGHC" = "Bo_code"))
london_children_obesity <- left_join(borough, child_obesity_data, by = c("BOROUGHC" = "Bo_code"))

# map for obesity
tm_shape(obesity_data) + tm_fill("obese_rate", style = "quantile", n = 5, palette = "Oranges") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_text("BOROUGHN", size = "AREA") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Quintile Map of London Year 6 Children Obesity Rate", title.position = c("left","top"), title.size = 1, frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# Global Moran's I test to see whether there is an evidance for clustering
# convert to sp
london_obesity_sp <- as_Spatial(london_children_obesity, IDs=london_children_obesity$BOROUGHC)

# create an nb object
london_obesity_nb <- poly2nb(london_obesity_sp, row.names=london_obesity_sp$OA11CD)

# inspect the first 10 rows 
str(london_obesity_nb,list.len=10)

# create the list weights object
obesity_nb_weights_list <- nb2listw(london_obesity_nb, style='W')

# inspect
class(obesity_nb_weights_list)

# Moran's I
obesity_mi_value <- moran(london_obesity_sp$obese_rate,obesity_nb_weights_list,n=length(obesity_nb_weights_list$neighbours),S0=Szero(obesity_nb_weights_list))

# inspect global moran's i
obesity_mi_value

# run a Monte Carlo simulation 599 times
obesity_mc_model <- moran.mc(london_obesity_sp$obese_rate, obesity_nb_weights_list, nsim=599)

# inspect
obesity_mc_model

# Local Moran's I to get the clustered boroughs

# Local Moran's I
local_moran_london_children_obesity <- localmoran(london_obesity_sp$obese_rate, obesity_nb_weights_list)

# rescale
london_obesity_sp$scaled_obese_rate <- scale(london_obesity_sp$obese_rate)

# create a spatial lag variable 
london_obesity_sp$lag_scaled_obese_rate <- lag.listw(obesity_nb_weights_list, london_obesity_sp$scaled_obese_rate)

# convert to sf
london_obesity_local_moran_stats <- st_as_sf(london_obesity_sp)

# set a significance value
sig_level <- 0.1

# classification with significance value
london_obesity_local_moran_stats$quad_sig <- ifelse(london_obesity_local_moran_stats$scaled_obese_rate > 0 & 
                                             london_obesity_local_moran_stats$lag_scaled_obese_rate > 0 & 
                                               local_moran_london_children_obesity[,5] <= sig_level, 
                                          'high-high', 
                                          ifelse(london_obesity_local_moran_stats$scaled_obese_rate <= 0 & 
                                                   london_obesity_local_moran_stats$lag_scaled_obese_rate <= 0 & 
                                                   local_moran_london_children_obesity[,5] <= sig_level, 
                                                 'low-low', 
                                                 ifelse(london_obesity_local_moran_stats$scaled_obese_rate > 0 & 
                                                          london_obesity_local_moran_stats$lag_scaled_obese_rate <= 0 & 
                                                          local_moran_london_children_obesity[,5] <= sig_level, 
                                                        'high-low', 
                                                        ifelse(london_obesity_local_moran_stats$scaled_obese_rate <= 0 & 
                                                                 london_obesity_local_moran_stats$lag_scaled_obese_rate > 0 & 
                                                                 local_moran_london_children_obesity[,5] <= sig_level, 
                                                               'low-high',
                                                               ifelse(local_moran_london_children_obesity[,5] > sig_level, 
                                                                      'not-significant', 
                                                                      'not-significant')))))


# plot the results with the statistical significance
ggplot(london_obesity_local_moran_stats, aes(x = scaled_obese_rate, 
                                   y = lag_scaled_obese_rate, 
                                   color = quad_sig)) +
  geom_vline(xintercept = 0) + # plot vertical line
  geom_hline(yintercept = 0) + # plot horizontal line
  xlab('Scaled Obese Rate (n)') +
  ylab('Lagged Scaled Obese Rate (n)') +
  labs(colour='Relative to neighbours') +
  geom_point()

# map only the statistically significant results here
tm_shape(london_obesity_local_moran_stats) +
  tm_fill(col = 'quad_sig', palette = c("#de2d26", "#fee0d2", "white")) +
  tm_borders(col = "grey")

# Multivariable Regression Model
# Reporting basic summary statistical measures
summary(datafile$obese_rate)

# plot to check non-alcholic_beverage_consumption in london
plot1 <- tm_shape(obesity_data) + tm_fill("non_alc_bev", style = "quantile", n = 7, palette = "Blues") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# plot the image object
plot1

# create 3 separate maps and store them in plot2, plot3 & plot4 objects
# map for children in poverty famlies
plot2 <- tm_shape(obesity_data) + tm_fill("children_in_poverty", style = "quantile", n = 7, palette = "Reds") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# map for physically inactive children rate
plot3 <- tm_shape(obesity_data) + tm_fill("inactive_children", style = "quantile", n = 7, palette = "Oranges") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# map for adult obese rate
plot4 <- tm_shape(obesity_data) + tm_fill("adult_obese", style = "quantile", n = 7, palette = "Greens") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# stitch the maps together using tmap_arrange() function
tmap_arrange(plot1, plot2, plot3, plot4, nrow = 2)

# Fitting a non-spatial multivariable regression model on spatial data and checking the residuals
# lm() function builds a regression model and stores model output into the object 'modelMLR'
modelMLR <- lm(obese_rate ~ log10(non_alc_bev) + log10(children_in_poverty) + log10(inactive_children) + log10(adult_obese), data = obesity_data)
# Include the 'scipen=7' argument in the summary() function remove those annoying scientific notation!
options(scipen = 7)
# summary() calls report the output stored in object 'modelMLR'
summary(modelMLR)

# The model have been fitted, extract the residuals
# Extract residuals from "modelLMR" object and dump into "spatialdatafile" and call the column "RESIDUALS"
obesity_data$RESIDUALS <- modelMLR$residuals

# Reporting basic summary measures to have an idea of its distribution before plotting them on map
summary(obesity_data$RESIDUALS)

# Let us generate a map to examine if these residuals show patterns of spatial autocorrelation
tm_shape(obesity_data) + tm_fill("RESIDUALS", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# let’s use the Moran’s I test to confirm the presence of spatial autocorrelation
# create the spatial adjacency matrix and apply the Moran’s I test on the modelMLR object using the lm.morantest() function.
#generate unique number for each row
obesity_data$ROWNUM <- 1:nrow(obesity_data)
# We need to coerce the sf spatialdatafile object into a new sp object
obesity_data2.0 <- as(obesity_data, "Spatial")
# Create spatial weights matrix for areas
Weights <- poly2nb(obesity_data2.0, row.names = obesity_data2.0$ROWNUM)
WeightsMatrix <- nb2mat(Weights, style='B')
Residual_WeightMatrix <- mat2listw(WeightsMatrix , style='W')
# Run the test on the regression model output object "modelMLR" using lm.morantest()
lm.morantest(modelMLR, Residual_WeightMatrix, alternative="two.sided")

# drop the variables that haven't passed the significant testing, non_alc_bev and inactive_children
# lm() function builds a regression model and stores model output into the object 'modelMLR'
modelMLR <- lm(log10(obese_rate) ~  + log10(children_in_poverty)  + adult_obese, data = obesity_data)
# Include the 'scipen=7' argument in the summary() function remove those annoying scientific notation!
options(scipen = 7)
# summary() calls report the output stored in object 'modelMLR'
summary(modelMLR)

# The model have been fitted, extract the residuals
# Extract residuals from "modelLMR" object and dump into "spatialdatafile" and call the column "RESIDUALS"
obesity_data$RESIDUALS <- modelMLR$residuals

# Reporting basic summary measures to have an idea of its distribution before plotting them on map
summary(obesity_data$RESIDUALS)

# Let us generate a map to examine if these residuals show patterns of spatial autocorrelation
tm_shape(obesity_data) + tm_fill("RESIDUALS", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# let’s use the Moran’s I test to confirm the presence of spatial autocorrelation
# create the spatial adjacency matrix and apply the Moran’s I test on the modelMLR object using the lm.morantest() function.
#generate unique number for each row
obesity_data$ROWNUM <- 1:nrow(obesity_data)
# We need to coerce the sf spatialdatafile object into a new sp object
obesity_data2.0 <- as(obesity_data, "Spatial")
# Create spatial weights matrix for areas
Weights <- poly2nb(obesity_data2.0, row.names = obesity_data2.0$ROWNUM)
WeightsMatrix <- nb2mat(Weights, style='B')
Residual_WeightMatrix <- mat2listw(WeightsMatrix , style='W')
# Run the test on the regression model output object "modelMLR" using lm.morantest()
lm.morantest(modelMLR, Residual_WeightMatrix, alternative="two.sided")








# Though Moran's I test is not significant hencr no need of spatial appraoach, given the aim of this project is to demonstrate the understanding of different linear regression models,
# some exploratory work on spatial regression model should be fine to conduct
# Construct spatial regression model
# spatial lag model, lagged on the dependent variable
# Fit model using lagsarlm()
# reuse spatial weight matrix created earlier as an object called "Residual_WeighMatrix" 
modelSLY <- lagsarlm(obese_rate ~ log10(non_alc_bev) + log10(children_in_poverty) + log10(inactive_children) + log10(adult_obese), data = obesity_data, Residual_WeightMatrix)

# Report results with summary()
# We are interested in the rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(modelSLY)

# run a moran's i test
# extract the residuals for modelSLY object and dump back to original sf obesity_data object
obesity_data$RESID_SLY <- modelSLY$residuals
# use Moran's I test using moran.mc() function
moran.mc(obesity_data$RESID_SLY, Residual_WeightMatrix, 1000, zero.policy = T)


# generate the map of residual
tm_shape(obesity_data) + tm_fill("RESID_SLY", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

# Interpretation of results using impacts
# impacts
Weights_2.0 <- as(Residual_WeightMatrix, "CsparseMatrix")
trMC <- trW(Weights_2.0, type="MC")
summary(impacts(modelSLY, tr = trMC, R=100), zstats=TRUE)



# Construct spatial error model
modelSER <- errorsarlm(obese_rate ~ log10(non_alc_bev) + log10(children_in_poverty) + log10(inactive_children) + log10(adult_obese), data = obesity_data, Residual_WeightMatrix)

# Report results with summary()
# We are interested in the rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(modelSER)

# run a moran's i test
# extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
obesity_data$RESID_SER <- modelSER$residuals
# use Moran's I test using moran.mc() function
moran.mc(obesity_data$RESID_SER, Residual_WeightMatrix, 1000, zero.policy = T)

# generate the map
tm_shape(obesity_data) + tm_fill("RESID_SER", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(borough) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)








