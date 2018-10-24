library(mni2aal)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI")

PFC_ROI_coordiates <- read.csv("PFC_ROI_coordiates.csv", header = T)

Result <- t(mapply(FUN=mni_to_region_name,x=PFC_ROI_coordiates$x,y=PFC_ROI_coordiates$y,z=PFC_ROI_coordiates$z))

write.csv(Result, "aal_Result.csv")
