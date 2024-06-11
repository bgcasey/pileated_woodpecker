# ---
# title: "6_summarize_results
# author: "Brendan Casey"
# created: "2023-12-01"
# ---

##////////////////////////////////////////////////////////////////

#Setup ----

##Load packages----
library(tidyverse)
library(dismo)
library(gbm)
##///////////

##Load BRTs
load("3_output/models/WT/brt_nfis_ls_hlc_terrain_canopy_29.rData")

##////////////////////////////////////////////////////////////////
# plot variable importance----

##////////////
## brt_nfis_ls_hlc_terrain_canopy_29----

data<-summary(brt_nfis_ls_hlc_terrain_canopy_29)

## Take the top 10 predictors
top_data <- head(data[order(-data$rel.inf), ], 15)

## Remove "mean_" from the "var" column
top_data$var <- gsub("mean_", "", top_data$var)
top_data$var <- gsub("lat", "latitude", top_data$var)
top_data$var <- gsub("FirstReturns.H150", "Height_150", top_data$var)
top_data$var <- gsub("Biomass.H150", "Biomass_150", top_data$var)
top_data$var <- gsub("`Exposed_Barren land_1000`", "Exposed_Barren land_1000", top_data$var)


# top_data<-top_data%>%dplyr::filter(var!="latitude")
top_data<-top_data%>%dplyr::filter(var!="lon")
top_data$var <- gsub("DRS", "NDRS", top_data$var)


## create a bar plot
bar_plot <- ggplot(top_data, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = "identity", fill = "#616a6b") +
  labs(
       y = "Predictor",
       x = "Relative influence")+
  theme_minimal()+
  theme(panel.grid = element_blank(),  panel.background = element_rect(color = "black", fill = "white"))

ggsave(filename = "3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_bar.png", plot = bar_plot, width=8, height=4, units="in", dpi=300)

##////////////////////////////////////////////////////////////////
### Partial dependence plots----

#### Individual plots----
png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_latitude.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 1, smooth=TRUE,  x.label= "latitude", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_TPI_5000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 81, smooth=TRUE, x.label= "TPI_5000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Water_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 28, smooth=TRUE, x.label= "Water_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()


png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Height_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 94, smooth=TRUE, x.label= "Height_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Wetland_treed_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 29, smooth=TRUE, x.label= "Wetland_treed_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Mixedwood_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 8, smooth=TRUE, x.label= "Mixedwood_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Mixedwood_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 26, smooth=TRUE, x.label= "Mixedwood_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Exposed_Barren_land_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 24, smooth=TRUE, x.label= "Exposed_Barren_land_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Biomass_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 84, smooth=TRUE, x.label= "Biomass_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()


png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_Biomass_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 84, smooth=TRUE, x.label= "Biomass_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()


png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_DRS_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 55, smooth=TRUE, x.label= "DRS_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()


png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_DRS_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 35, smooth=TRUE, x.label= "DRS_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_NDRS_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 36, smooth=TRUE, x.label= "NDRS_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()

png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_NDRS_1000.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 56, smooth=TRUE, x.label= "NDRS_1000", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()



png(file="3_output/model_summaries/brt_nfis_ls_hlc_terrain_canopy_29_TWI_150.png", width=10, height=8, units="in", res=300)
gbm.plot(brt_nfis_ls_hlc_terrain_canopy_29, variable.no = 61, smooth=TRUE, x.label= "TWI_150", plot.layout = c(1,1), write.title = FALSE,show.contrib = FALSE, cex.lab=1.5)
dev.off()


