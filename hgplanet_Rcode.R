#load libraries
library(geosphere)
library(rgeos)
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)
library(devtools)
library(rgdal)
library(raster)
library(ggplot2)
library(caret)
library(readxl)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(treemap)
library(SHAPforxgboost)

#load basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

#read in data
setwd("C:/Users/.../file")
df <- read_excel("C:/Users/.../hgplanet_data.xlsx", guess_max = 10000)

#copy dataframe
r_df = df
summary(r_df)

#set breaks for map
r_df$cuts=cut(r_df$DENSITY, breaks=c(0, .001, 0.01, .1, 1,2,5), include.lowest = TRUE) 

#Create map of spatially-extended population densities
bb <- ggplot() + 
  geom_sf(data = world, alpha = 1, color = "gray70", fill = "gray70", size = .1, aes(geometry = geometry)) +
  coord_sf(ylim = c(-58,85), clip = "on", label_axes = list(bottom = "E", left = "N"), expand = FALSE) +
  geom_tile(data=r_df, aes(x=longitude,y=latitude,fill=cuts)) + 
  scale_fill_brewer("Population density",type = "seq", palette = "YlOrRd", na.value = "gray70") +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  ylim(c(-55,80)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue", color = "white"),
        plot.background = element_rect(fill = "white")
  )
bb

#save map
ggsave("bb_plot2.pdf", bb, dpi=600)

#Figure 7. generate treemap of biome contributions for cultural diversity
tmap <- treemap(df,
        index="name",
        vSize="abund",
        type="index",
        title="",
        palette="RdYlGn",
        border.col=c("black"),             
        border.lwds=1,                         
        fontsize.labels=1,
        fontcolor.labels="black",
        fontface.labels=1,            
        bg.labels="transparent",              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=T
        )
tmap

#save tree map
ggsave("tmap.pdf", tmap, dpi = 600)

#Prepare training data
names(df)
df=df[,-c(1:3)] 
df$DENSITY = log(df$DENSITY) #this is binford density applied to all points in blobs
summary(df)
df$biome = factor(df$biome)
df2 = df
df2$bc3 <- NULL
df2$bc7 <- NULL
df2$urban <- NULL
df2$agpot <- NULL
df2$areas <- NULL
df2$npp <- NULL
df2$abslat <- NULL
df2 <- df2[, -c(20, 29:55, 59:60, 63:66)] #remove soil variables and others
df2=na.omit(df2) #reduces down to 6188 cells

#prepare prediction data
df3 <- df[, -c(3:5,7,27, 38:66, 69:70, 73:75)]
df3$bc3 <- NULL
df3$bc7 <- NULL
df3$urban <- NULL
df3$agpot <- NULL
df3$continent <- NULL
df3$areas <- NULL
df3$npp <- NULL
df3$predictions <- NULL
df3=na.omit(df3)
df3$DENSITY <- NA
coordinates(df3) <- ~longitude+latitude

#Example run of xgboost algorithm
fitControl <- trainControl(method = "none")

#set hyper-parameters (trained from 10,000 iterations of random parameter searching)
xgboostGrid <-  expand.grid(nrounds = 646, 
                            max_depth = 8, 
                            eta = 0.09785586, 
                            gamma = 0.008289637, 
                            colsample_bytree = 0.585347, 
                            min_child_weight = 15,
                            subsample = 0.7173247)

#run xgboost training model
xgb <- train(DENSITY ~ ., method = "xgbTree", data = df2,
               tuneGrid = xgboostGrid, 
               trControl = fitControl, verbose = FALSE)
print(xgb)

#Variable importance
vi = varImp(xgb, scale=T)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw() +
  theme(aspect.ratio = 1, 
        axis.title.y = element_blank())
vi

#run same model in xgboost package to get SHAP values
dmy <- dummyVars("DENSITY ~ .", data = df2, fullRank = FALSE)
x_train <- predict(dmy, newdata = df2)
X1 = as.matrix(x_train)
set.seed(123)
mod1 = xgboost::xgboost(
  data = X1, label = df2$DENSITY,
  nrounds = 646, 
  max_depth = 8, 
  eta = 0.09785586, 
  gamma = 0.008289637, 
  colsample_bytree = 0.585347, 
  min_child_weight = 15,
  subsample = 0.7173247, verbose = FALSE)
mod1

importanceRaw <- xgb.importance(model = mod1, data = X1)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

# plot the first tree
xgb.plot.tree(model = xgb$finalModel, trees = 1)

# return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod1, X_train = X1) #x1 or 2

# The ranked features by mean |SHAP|
shap_values$mean_shap_score

#example SHAP dependence plot
g1 <- shap.plot.dependence(data_long = shap_long, size0 = 2, dilute = 1, smooth = F, 
                           x = 'min temperature of coldest month', y ='min temperature of coldest month', color_feature = 'pathogen load') + ggtitle("") +
                           geom_smooth(span = .2,  method = "loess") + ylab("SHAP min temp coldest month" ) +
                           geom_hline(yintercept = 0) +
                           guides(color = FALSE) +
                           theme(aspect.ratio = 1)
