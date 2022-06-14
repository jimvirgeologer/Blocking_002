library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

############ INPUT FACE MAPPING SHEETS ###############  

# 
# setwd("~/current work/01_R_Projects/02_Blocking/Blocking/Face_Maps")
# file.list_gis <- list.files(getwd(), pattern = '.xlsx', recursive = TRUE)

###########GIS DATA BASE###############
setwd("~/current work/01_R_Projects/Blocking_002")
file.list_gis <- list.files(path = './FACE_MAPPING_GIS/SDN4', pattern = '.xlsx', recursive = TRUE, full.names = TRUE)
file.list_gis <- file.list_gis[!grepl("~", file.list_gis)]


############# Face Map GIS Function ###############
face_map_gis<- function(i) {
  x = read_xlsx(i,sheet = 1)
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11"
    )
  
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
    LOCATIONX = as.numeric(c2),
    LOCATIONY = as.numeric(c3),
    LOCATIONZ = as.numeric(c4),
    LENGTH = as.numeric(c5),
    LEVEL = as.numeric(c6),
    AREA = as.character(c7),
    ROCKCODE = as.character(c8),
    SAMP_BY = as.character(c9),
    DATE_SAMP = as.Date(c10),
    TENEMENT = as.character(c11)) %>%
    filter(!is.na(HOLE_ID))
} 


############# Applying Function to the file list gis ###############
df_gis <- lapply(file.list_gis, face_map_gis) %>%
  bind_rows %>%
  as.data.frame()

########### joining table of the mine geo face mapping and gis face mapping ###########
df_joined <- full_join(df,df_gis,by = c("SHEET" = "HOLE_ID"))
vis_miss(df_joined)

df_joined <- df_joined %>% filter(!is.na(LOCATIONX))


############## Plotting face mapping of both mine geo and gis #############
face_map_plot<- st_as_sf(df_joined , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)
ggplot(data = face_map_plot) + geom_sf()



############ INPUT SHAPEFILE POSITION LINES ###############  

setwd("~/current work/01_R_Projects/02_Blocking/Blocking/Shapefiles")

POS_LINES <- st_read(
  "./N_S_Positions.shp")
POS_LINES<- POS_LINES[,-c(1:2)]
POS_LINES_PLOT <- ggplot() + 
  geom_sf(data = POS_LINES, size = 0.1, color = "cyan") + 
  ggtitle("POS_LINES_PLOT") + 
  coord_sf()

ggplotly(POS_LINES_PLOT) 

######## PLOTTING ##############
face_map_names <- st_centroid(face_map_plot) ############### creating centroids on the data
face_map_names <- cbind(face_map_names, st_coordinates(st_centroid(face_map_plot$geometry)))############### addng X and Y to points

############# Intersection of the position lines and the face mapping plots #############




POS_FACE_MAP <- st_intersection(POS_LINES,face_map_plot)   
# %>% filter(LEVEL.x== 530)
POS_FACE_MAP_PLOT <- ggplot(data = POS_FACE_MAP, aes(color = SOURCE, text = SHEET)) +
  geom_sf()

ggplotly(POS_FACE_MAP_PLOT,tooltip = "text")

############## Compositing per block ##############

POS_FACE_MAP_AVERAGE <- POS_FACE_MAP %>%
  mutate(LEN_AU = LENGTH.x * AU_gpt) %>%
  group_by(POS_N_S, AREA, LEVEL.x) %>% 
  summarize(AVE= sum(LEN_AU)/sum(LENGTH.x)) %>% mutate(AVE = signif(AVE,3))

POS_FACE_MAP_PLOT_BLOCK <- ggplot(data = POS_FACE_MAP_AVERAGE , aes(color = AVE, text = POS_N_S)) +
  geom_sf()


ggplotly(POS_FACE_MAP_PLOT_BLOCK,tooltip = "text")



########################## Plotting the SDN4 ###############################
SDN4_PLOT <- POS_FACE_MAP_AVERAGE %>% filter(AREA == "SDN4") %>% ggplot(aes(x = POS_N_S, y = LEVEL.x, label = AVE)) + geom_text(hjust = 0, vjust = 0)
ggplotly(SDN4_PLOT)



#################### Acquiring Coordinates / Joining Mine Geology and GIS Data  #############

# df_joined <- full_join(df,df_gis,by = c("SHEET" = "HOLE_ID"))
# vis_miss(df_joined)



