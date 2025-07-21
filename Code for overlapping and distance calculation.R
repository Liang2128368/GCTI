library(readxl)
library(writexl)
library(sf)



# Read GCTD database
table_database <- read_xlsx("Table Database.xlsx")
shp_database <- st_read("Shp Database.shp")
point_database <- st_read("Point Database.shp")
multipoint_database <- st_read("Multipoint Database.shp")
# Buffer point and multipoint database
point_database_buffer <- st_buffer(point_database,1000)
multipoint_database_buffer <- st_buffer(multipoint_database,1000)
all_database <- bind_rows(shp_database,point_database_buffer,multipoint_database_buffer)

corrected_name <- read.csv("corrected name.csv",header = T)# Read corrected name table
species_distribution <- st_read("IUCN_database.shp")# Read IUCN database


x <- which(corrected_name$CorrectedName %in% species_distribution$sci_name)# Extract species names that match IUCN data
y <- which(table_database$Species %in% corrected_name$OriginalName[x])# Extract the species names used for overlapping from the GCTD database


result <- data.frame("number" = NA,"sub_number" = NA,"species_original" = NA,
                     "species_corrected" =NA ,"area_intersected"= NA,"distance" = NA)# Creat result dataframe

# Overlapping
for (i in y) {
  result[i,1] <- table_database$Number[i]# Extract "Number"
  result[i,2] <- table_database$`Sub-number`[i]# Extract "Sub-number"
  result[i,3] <- table_database$Species[i]# Extract "Species"
  result[i,4] <- corrected_name$CorrectedName[which(corrected_name$OriginalName == table_database$Species[i])]# Extract corrected species name
  GCTD <- all_database[which(all_database$Sub_number == result[i,2]),]# Extract the corresponding shapefile from the GCTD database
  GCTD <- st_union(GCTD)# Union the polygons
  IUCN <- species_distribution[which(species_distribution$sci_name == result[i,4]),]
  if (!all(st_is_valid(k))){# Check and fix the validity of the polygons
    IUCN <- st_make_valid(IUCN)
  }
  IUCN <- st_union(IUCN)# Union the polygons
  intersection <- st_intersection(GCTD,IUCN)# Extract the intersection between GCTD and IUCN databases
  if (length(st_area(intersection)) == 0){# Fill in the intersection area and distance between GCTD and IUCN databases
    result[i,5] <- 0 # If the intersection is empty, fill in 0 for the intersection area
    result[i,6] <- st_distance(GCTD,IUCN) # If the intersection is empty, calculate the distance
  }else{
    result[i,5] <- st_area(intersection)/st_area(GCTD) 
    result[i,6] <- 0 # If the intersection is not empty, fill in 0 for the distance
  }
}

write_xlsx(result,"result.xlsx")



