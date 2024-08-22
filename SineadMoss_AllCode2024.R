# Draft code


library(devtools)
library(aomisc)
library(nlme)
library(tidyverse)
library(readr)
library(Hmisc)
library(cowplot)
library(dplyr)
library(minpack.lm)
library(stringr)
library(convertr)

library(sp)
library(raster)
library(sf)
library(terra)
library(geodata)
library(rworldmap)

library(modelr)
library(purrr)

library(broom)

library(ggplot2)

library(tidyverse)
library(broom)
library(ggplot2)
library(dplyr)
library(data.table)

#Read in data (NOT standardised)
data<-read.csv("BEF_DATA_COMBINED_NOT STANDARDISED.csv")

#Data manipulation#
#remove any strange characters in location
data$Location <- as.character(data$Location)
data$Location <- gsub("[[:punct:]]","",data$Location)



#add group numbers/ labels
data <- setDT(data)[,label:=.GRP, by = c("DOI", "Location", "Taxon", "Biodiversity_x_axis_description","Ecosystem_function_y_axis_description", "Biodiversity_metric")]
unique(data$Ecosystem_function_metric)

###################################
# Drop labels where we have no/little data * >5
###################################
entries <- table(data$label)
entries <- entries[entries > 5]
data <- data[data$label %in% names(entries),]

#biodiv/ eco funt. needs to have at least 3 distinct values
data <- data %>%
  group_by(label) %>%
  mutate(count = n_distinct(Biodiversity_value_x)) %>% 
  filter(count !=2) %>% 
  filter(count !=1) %>% 
  dplyr::select(-count)

data <- data %>%
  group_by(label) %>%
  mutate(count = n_distinct(Ecosystem_function_value_y)) %>% 
  filter(count !=2) %>% 
  filter(count !=1) %>% 
  dplyr::select(-count)

data = subset(data, !is.na(Ecosystem_function_value_y))
data = subset(data, !is.na(Biodiversity_value_x))

#WRITE AS UNSTANDARDISED CSV
write.csv(data, "data_NOT_STANDARDISED.csv")

########################################################


########### Load the combined data following "Example BEF plots + map.R"
data <- read_csv("data_NOT_STANDARDISED.csv", 
                 col_types = cols(Biodiversity_value_x = col_number(), 
                                  Biodiversiy_value_SD = col_number(), 
                                  Ecosystem_function_value_y = col_number()))

# Standardise these categorical variables to make sure they are consistent

# Arrange year columns to follow 4 digit year format
# Start year
data$Sample_start_earliest <- str_sub(data$Sample_start_earliest,-4,-1)
data$Sample_start_earliest <- str_remove_all(data$Sample_start_earliest, "[a-zA-Z]")
data$Sample_start_earliest <- str_remove_all(data$Sample_start_earliest, "-")
add_20_to_string <- function(x) {
  if (nchar(x) == 2 && substr(x, 1, 1) %in% c("0", "1", "2")) {
    return(paste0("20", x))
  } else {
    return(x)
  }
}
data$Sample_start_earliest <- sapply(data$Sample_start_earliest, add_20_to_string)
add_19_to_string <- function(x) {
  if (nchar(x) == 2 && substr(x, 1, 1) %in% c("9", "8")) {
    return(paste0("19", x))
  } else {
    return(x)
  }
}
data$Sample_start_earliest <- sapply(data$Sample_start_earliest, add_19_to_string)
unique(data$Sample_start_earliest)
data$Sample_start_earliest[data$Sample_start_earliest == ""] <- NA
data$Sample_start_earliest <- as.numeric(data$Sample_start_earliest)

# End year
data$Sample_end_latest <- str_sub(data$Sample_end_latest,-4,-1)
data$Sample_end_latest <- str_remove_all(data$Sample_end_latest, "[a-zA-Z]")
data$Sample_end_latest <- str_remove_all(data$Sample_end_latest, "-")
data$Sample_end_latest <- sapply(data$Sample_end_latest, add_20_to_string)
data$Sample_end_latest <- sapply(data$Sample_end_latest, add_19_to_string)
unique(data$Sample_end_latest)
data$Sample_end_latest[data$Sample_end_latest == ""] <- NA
data$Sample_end_latest <- as.numeric(data$Sample_end_latest)

# standardise EF_cat --> first letter capital, words separated by _, acronyms capitalised, no spaces
total_EFs <- unique(data$Ecosystem_function_metric)
total_EFs

# Terrestrial_NPP
wrong_tnpp <- "terrestrial_NPP"
data$Ecosystem_function_metric <- replace(data$Ecosystem_function_metric, data$Ecosystem_function_metric
                                          %in% wrong_tnpp, "Terrestrial_NPP")

# Terrestrial_C_sequestration
wrong_tcs <- c("terrestrial_C_sequestration", "Terrestrial _C_sequestration")
data$Ecosystem_function_metric <- replace(data$Ecosystem_function_metric,
                                          data$Ecosystem_function_metric %in% wrong_tcs,
                                          "Terrestrial_C_sequestration")

# Marine_NPP
wrong_mnpp <- "marine_NPP"
data$Ecosystem_function_metric <- replace(data$Ecosystem_function_metric, data$Ecosystem_function_metric
                                          %in% wrong_mnpp, "Marine_NPP")

# Evapotranspiration
wrong_et <- "evapotranspiration"
data$Ecosystem_function_metric <- replace(data$Ecosystem_function_metric, data$Ecosystem_function_metric 
                                          %in% wrong_et, "Evapotranspiration")

# Freshwater_NPP
wrong_fnpp <- "freshwater_NPP"
data$Ecosystem_function_metric <- replace(data$Ecosystem_function_metric, data$Ecosystem_function_metric 
                                          %in% wrong_fnpp, "Freshwater_NPP")


## pull out relevant data for the code  - data are NOT scaled!
x <- data$Biodiversity_value_x
x_axis <- data$Biodiversity_x_axis_description
y <- data$Ecosystem_function_value_y
y_axis <- data$Ecosystem_function_y_axis_description
y_unit <- data$Ecosystem_function_unit_y_axis
label <- data$label
EF_cat <- data$Ecosystem_function_metric
lat <- as.numeric(data$GPS_latitude_centre)
long <- as.numeric(data$GPS_longitude_centre)
biome <- data$Biome_general
landuse <- data$Predominant_landuse
landuse_intensity <- data$Predominant_landuse_intensity
study_type <- data$Experiment_or_Observation
start_year <- data$Sample_start_earliest
end_year <- data$Sample_end_latest
spatial_scale <- data$spatial_extent_m2

unique(end_year)
# Create dataframe with each variable in it
dat <- data.frame(label, EF_cat,long, lat, study_type,start_year, end_year, 
                  x, x_axis, y, y_axis,  y_unit,biome,landuse,landuse_intensity, 
                  spatial_scale) 
unique(dat$start_year)


# fix group that has start and end years the wrong way around
rows <- dat$label == 625
temporary <- dat$start_year[rows]
dat$start_year[rows] <- dat$end_year[rows]
dat$end_year[rows] <- temporary

# fix group that has lat and long the wrong way around
rows <- dat$label == 1387
temporary <- dat$lat[rows]
dat$lat[rows] <- dat$long[rows]
dat$long[rows] <- temporary


# Add duration of study column and filter to years after 1990
dat$duration_yrs <- dat$end_year - dat$start_year
dat <- dat %>% filter(start_year >= 1990)


# Define the wrong units and replacement units
# Define the replacement lists for each unit
# %
wrong_perc <- c(
  "% delta13C",
  "% year-1",
  "log(%)",
  "(%)",
  "%"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_perc, "%")
# mg per ha
wrong_mg_per_ha <- c(
  "(Mg ha^-1 yr^-1)",
  "(Mg C ha^-1)",
  "(Mg C ha^-1 yr^-1)",
  "(Mg ha^-1)",
  "Mg C/ha",
  "Mg ha-1 year-1",
  "Mg ha-1",
  "Mg ha?1",
  "MgC ha-1 yr-1",
  "MgC ha-1 year-1",
  "Mg ha-1 y-1",
  "Mg C ha-1",
  "Mg/ha",
  "Mg ha^(-1) yr^(-1)",
  "Mg ha^-1",
  "Mg ha^-1 yr^-1",
  "MgC ha^-1 yr^-1",
  "Mg C ha^-1",
  "(Mg/ha)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_ha, "mg_per_ha")
# mg per kg
wrong_mg_per_kg <- c(
  "(mg kg^-1 soil)",
  "(mg kg^-1)",
  "mg kg-1 soil",
  "mg kg-1",
  "mg kg soil-1",
  "mg/kg soil",
  "mg/kg"
  
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_kg, "mg_per_kg")
# mg per m2
wrong_mg_per_m2 <- c(
  "(mg m^-2)",
  "mg m-2",
  "mg/m2",
  "mg m^-2 day^-1",
  "mg m^(-2) d^(-1)",
  "mg/m^2",
  "Pn (Mg C m^-2 d^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_m2, "mg_per_m2")
# mg
wrong_mg <- c(
  "(Mg)",
  "(mg)",
  "mg",
  "mg d.w."
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg, "mg")
# mg per g
wrong_mg_per_g <- c(
  "(mg g^-1 leaf dry mass)",
  "mg O2 g-1 min-1",
  "mg g-1 soil",
  "mg* (g dry soil)-1",
  "(mg g^-1)",
  "(mg C g^-1)",
  "(mg g^-1 d^-1)",
  "mg/g soil dry weight (DW)",
  "mg necromass carbon/g soil DW",
  "(m g^-1 soil)",
  "m g-1 soil"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_g, "mg_per_g")
# mg per L
wrong_mg_per_L <- c(
  "mg L-1  min-1",
  "mg L-1",
  "(mg/L)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_L, "mg_per_L")
# mg per m3
wrong_mg_per_m3 <- c(
  "mgCm-3 h-1",
  "(mg C m^-3)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_m3, "mg_per_m3")
# mg per dm3
wrong_mg_per_dm3 <- c(
  "mg dm^(-3) d^(-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_dm3, "mg_per_dm3")
# mg per cm3
wrong_mg_per_cm3 <- c(
  "mg cm-3"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mg_per_cm3, "mg_per_cm3")
# g
wrong_g <- c(
  "(g)",
  "g",
  "g DW core^(-1)",
  "g plot^-1",
  "g/unit",
  "g/49m2/year",
  "(g m^-1), control (2009)",
  "(g m^-1), low warming (2009)",
  "(g m^-1), high warming (2009)",
  "(g m^-1), control (2012)",
  "(g m^-1), low warming (2012)",
  "(g m^-1), high warming (2012)",
  "(g m^-1), control (2014)",
  "(g m^-1), low warming (2014)",
  "(g m^-1), high warming (2014)",
  "(g/0.1 m^2)",
  "(g/individual)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_g, "g")
# g per m2
wrong_g_per_m2 <- c(
  "(g ww m^-2)",
  "(gC/m^2)",
  "(g*m^-2)",
  "(g y^-1 m^-2)",
  "g m-2",
  "V g/m2",
  "g m-2 year-1",
  "g C m-2 year-1",
  "gC m-2",
  "g m-2 yr-1",
  "g C m-2 y-1",
  "g year-1 m-2",
  "gm^(-2)",
  "g/m^2",
  "g/m2",
  "g m^(-2)",
  "g/(m^2)",
  "g m^-2",
  "g m^-2 year^-1",
  "g m^-2 year-1",
  "g N/m^2",
  "gm^-2",
  "g m^-2 yr^-1",
  "g m^-2 day^-1",
  "gC/m2/y",
  "(g m^-2)",
  "g/m2 soil",
  "(g/m^2)",
  "Above-ground productivity (gdwm-2)",                                     
  "Below-ground productivity (gdwm-2)" 
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_g_per_m2, "g_per_m2")
# n individuals
wrong_n_individuals <- c(
  "(Individuals/20g dws)",
  "individuals m-2",
  "individual"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_n_individuals, "n_individuals")
# g per kg
wrong_g_per_kg <- c(
  "g kg-1",
  "g C m-2 yr-1",
  "(g kg^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_g_per_kg, "g_per_kg")
# g per m3
wrong_g_per_m3 <- c(
  "g m^(-3) y^(-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_g_per_m3, "g_per_m3")
# g per L
wrong_g_per_L <- c(
  "g biomass L^-1 water"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_g_per_L, "g_per_L")
# kg
wrong_kg <- c(
  "(kg)",
  "kg 400m-2 year-1",
  "kg",
  "G, kgC yr-1",
  "kg/100m^2",
  "kg/22 plants"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_kg, "kg")
# kg per ha
wrong_kg_per_ha <- c(
  "kg ha-1",
  "kg ha^-1 yr^-1",
  "(kg/ha)",
  "(Kg/ha)",
  "(kg ha^-1 yr^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_kg_per_ha, "kg_per_ha")
# kg per m2
wrong_kg_per_m2 <- c(
  "kgC m-2 year-1 (x 10000)",
  "kg m-2 year-1",
  "kg m^(-2) y^/9-1)",
  "kg/m2",
  "kg/m^2"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_kg_per_m2, "kg_per_m2")
# kg per km2
wrong_kg_per_km2 <- c(
  "kg km^-2 yr^-1",
  "kg km^-2"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_kg_per_km2, "kg_per_km2")
# micromol per kg
wrong_micromol_per_kg <- c(
  "micromol kg-1 h-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_micromol_per_kg, "micromol_per_kg")
# microlitre per g
wrong_microlitre_per_g <- c(
  "(?l O2 h^-1 g soil dw^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_micromol_per_kg, "microlitre_per_g")
# microgram per L
wrong_microgram_per_L <- c(
  "micrograms L^-1",
  "micrograms L-1",
  "(?g C L^-1)",
  "(?g L^-1)",
  "(?g/L)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_microgram_per_L, "microgram_per_L")
# micromol per m2
wrong_micromol_per_m2 <- c(
  "micromol m-2 s-1",
  "?mol CO2 m-2 S-1",
  "(umole CO2 m^-2 s^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_micromol_per_m2, "micromol_per_m2")
# micromol per g
wrong_microgram_per_g <- c(
  "micrograms CO2-C g-1 h-1",
  "micrograms N g^-1 day^-1",
  "micrograms g-1 day-1",
  "micrograms g-1",
  "(ug microbial C-[g soil]^-1[g dry mass]^-1)",
  "ug g-1",
  "?g CO2-C g-1 h-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_microgram_per_g, "microgram_per_g")
# nanogram_per microgram
wrong_nanogram_per_microgram <- c(
  "ng micrograms-1 day-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_nanogram_per_microgram, "nanogram_per_microgram")
# cm
wrong_cm <- c(
  "(cm)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_cm, "cm")
# t per ha
wrong_t_per_ha <- c(
  "t ha-1",
  "t*ha^(-1)",
  "t ha^(-1) yr^(-1)",
  "t/ha",
  "t BM*ha^-1*yr^-1",
  "ton ha-1",
  "(t ha^-1)",
  "(T. ha^-1)",
  "(tC ha^-1)"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_t_per_ha, "t_per_ha")
# mm
wrong_mm <- c(
  "(mm/yr)",
  "(mm)",
  "mm year-1",
  "mm",
  "mm/day",
  "mm month-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mm, "mm")
# mol per g
wrong_mol_per_g <- c(
  "mo g^-1",
  "mol g^-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mol_per_g, "mol_per_g")
# mol per m2
wrong_mol_per_m2 <- c(
  "mol m^-2"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_mol_per_m2, "mol_per_m2")
# nmol per g
wrong_nmol_per_g <- c(
  "nmol g^-1"
)
dat$y_unit <- replace(dat$y_unit, dat$y_unit %in% wrong_nmol_per_g, "nmol_per_g")


unique(dat$y_unit)


######################################################################
# Z standardise biodiversity and ecosystem function columns
#######


mean_x <- mean(dat$x, na.rm=TRUE)
sd_x <- sd(dat$x, na.rm = TRUE)
mean_y <- mean(dat$y, na.rm = TRUE)
sd_y <- sd(dat$y, na.rm = TRUE)


str(dat)
dat$x.z <-  (dat$x - mean_x)/sd_x
dat$y.z <-  (dat$y - mean_y)/sd_y
mean(dat$x.z)
sd(dat$x.z)
mean(dat$y.z)
sd(dat$y.z)


#######################################
library(terra)

### Prep coordinate data for worldclim
# create data frame of coordinates with EF
labelcoords <- data.frame(label,EF_cat, long,lat)
labelcoords <- labelcoords %>%
  filter(!is.na(lat), !is.na(long))
labelcoords <- labelcoords %>% distinct(label,EF_cat,long,lat)

# create data frame of coordinates without EF
coords <- data.frame(long,lat, label)
coords <- coords %>%
  filter(!is.na(lat), !is.na(long))



# create function to extract continent and country from coordinates
coords2continent <- function(coords)
{
  continentSP <- getMap(resolution="high")
  coordsSP <- SpatialPoints(coords, proj4string=CRS(proj4string(continentSP)))
  indices <- over(coordsSP, continentSP)
  indices$REGION
}
coords2country <- function(coords)
{
  countriesSP <- getMap(resolution="high")
  coordsSP <- SpatialPoints(coords, proj4string=CRS(proj4string(countriesSP)))
  indices <- over(coordsSP, countriesSP)
  indices$ADMIN
}

# Merge data
continents <- coords2continent(coords)
countries <- coords2country(coords)
geography <- data.frame(label = coords$label,
                        country = countries,
                        continent = continents)
geography <- data.frame(distinct(geography))

dat <- merge(geography, dat, by = c("label"))


####### download worldclim data

r <- worldclim_global(var="bio",res=10, path = 'data')

# Extract Bio1 (temp) and Bio12 (precipitation)
r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")
# Assign a CRS
crs(r) <- "EPSG:4326"  

# Convert the CRS string to a CRS object
crs_string <- crs(r, proj=TRUE)  
crs_object <- CRS(crs_string)  

# Convert to matrix
coords <- as.matrix(coords)


# Get points from coordinates
points <- SpatialPoints(coords, proj4string = crs_object)
# Convert SpatialPoints to SpatVector
points_vect <- vect(points)
values <- extract(r,points_vect)

# Extract coordinates from the SpatVector object
coords_extracted <- geom(points_vect)[, c("x", "y")]


# Rename to fit rest of data
bioclim <- cbind.data.frame(coords_extracted,values)
names(bioclim)[names(bioclim) == "x"] <- "long"
names(bioclim)[names(bioclim) == "y"] <- "lat"

# Merge with rest of data
bioclimlabel <- merge(bioclim, labelcoords, by=c("long","lat"))
bioclimlabel <- bioclimlabel[, -3]
bioclimlabel <- distinct(bioclimlabel)
dat <- merge(bioclimlabel,dat, by=c("label"))
# reorder dataframe
dat <- dat[, -c(9,10,11)]
colnames(dat) <- gsub(".x","", colnames(dat))
names(dat)[names(dat) == "x_is"] <- "x_axis"
names(dat)[names(dat) == "y_is"] <- "y_axis"


unique(filt10_dat$country)
nrow(unique(filt10_dat))
#####################################################################################

# Filter data to remove studies with fewer than 10 rows of data
filt10_dat <- dat %>%
  group_by(label) %>%
  filter(n() >= 10)%>%
  ungroup()

# Conduct nls with Michaelis-Menten formula for each EF
############################################ TERRESTRIAL C SEQUESTRATION
carbon_data <- filt10_dat %>% filter(EF_cat == "Terrestrial_C_sequestration" & x.z <=15)

carbon_data <- carbon_data %>% group_by(label)

# plot to check for outliers
plot(y.z~x.z, carbon_data)

# Nest data by label
by_label <- carbon_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(carbon_data$y.z, na.rm = TRUE)
initial_Km  <- carbon_data$x.z[which.min(abs(carbon_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function using initial values
carbon_function <- function(carbon_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = carbon_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", carbon_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
carbon_models <- by_label %>%
  mutate(model = map(data, carbon_function))


## Extract the parameters for each model
# make blank list to put parameters in
carbon_parameters <- list(Vmax = numeric(),
                          Km = numeric(),
                          label= character(),
                          EF_cat = character())

for (i in seq_along(carbon_models$model)) {
  model <- carbon_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    carbon_parameters$Vmax <- c(carbon_parameters$Vmax, coef["Vmax"])
    carbon_parameters$Km <- c(carbon_parameters$Km, coef["Km"])
    carbon_parameters$label <- c(carbon_parameters$label, carbon_models$label[[i]])
    carbon_parameters$EF_cat <- c(carbon_parameters$EF_cat, carbon_models$data[[i]]$EF_cat[1]) } 
  else {
    carbon_parameters$Vmax <- c(carbon_parameters$Vmax, NA)
    carbon_parameters$Km <- c(carbon_parameters$Km, NA)
    carbon_parameters$label <- c(carbon_parameters$label, carbon_models$label[[i]])
    carbon_parameters$EF_cat <- c(carbon_parameters$EF_cat, carbon_models$data[[i]]$EF_cat[1]) }
}

# Convert to data frame
carbon_parameters_df <- as.data.frame(carbon_parameters)

sum(is.na(carbon_parameters_df))
nrow(carbon_parameters_df)


############################################ BIOLOGICAL PUMP EFFICIENCIES
bpe_data <- filt10_dat %>% filter(EF_cat == "Biological_pump_efficiencies", x.z<(-0.17),y.z<0)

bpe_data <- bpe_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, bpe_data)



by_label <- bpe_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(bpe_data$y.z, na.rm = TRUE)
initial_Km  <- bpe_data$x.z[which.min(abs(bpe_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function
bpe_function <- function(bpe_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = bpe_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", bpe_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
bpe_models <- by_label %>%
  mutate(model = map(data, bpe_function))


## Now need to extract the parameters for each model
# make blank list to put parameters in
bpe_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(bpe_models$model)) {
  model <- bpe_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    bpe_parameters$Vmax <- c(bpe_parameters$Vmax, coef["Vmax"])
    bpe_parameters$Km <- c(bpe_parameters$Km, coef["Km"])
    bpe_parameters$label <- c(bpe_parameters$label, bpe_models$label[[i]])
    bpe_parameters$EF_cat <- c(bpe_parameters$EF_cat, bpe_models$data[[i]]$EF_cat[1]) } 
  else {
    bpe_parameters$Vmax <- c(bpe_parameters$Vmax, NA)
    bpe_parameters$Km <- c(bpe_parameters$Km, NA)
    bpe_parameters$label <- c(bpe_parameters$label, bpe_models$label[[i]])
    bpe_parameters$EF_cat <- c(bpe_parameters$EF_cat, bpe_models$data[[i]]$EF_cat[1]) }
}


bpe_parameters_df <- as.data.frame(bpe_parameters)

sum(is.na(bpe_parameters_df))
nrow(bpe_parameters_df)

############################################ BIOMASS TURNOVER RATE
btr_data <- filt10_dat %>% filter(EF_cat == "Biomass_turnover_rate", x.z <8.5, y.z<65)

btr_data <- btr_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, btr_data)


by_label <- btr_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(btr_data$y.z, na.rm = TRUE)
initial_Km  <- btr_data$x.z[which.min(abs(btr_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
btr_function <- function(btr_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = btr_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", btr_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
btr_models <- by_label %>%
  mutate(model = map(data, btr_function))


## Now need to extract the parameters for each model
# make blank list to put parameters in
btr_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(btr_models$model)) {
  model <- btr_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    btr_parameters$Vmax <- c(btr_parameters$Vmax, coef["Vmax"])
    btr_parameters$Km <- c(btr_parameters$Km, coef["Km"])
    btr_parameters$label <- c(btr_parameters$label, btr_models$label[[i]])
    btr_parameters$EF_cat <- c(btr_parameters$EF_cat, btr_models$data[[i]]$EF_cat[1]) } 
  else {
    btr_parameters$Vmax <- c(btr_parameters$Vmax, NA)
    btr_parameters$Km <- c(btr_parameters$Km, NA)
    btr_parameters$label <- c(btr_parameters$label, btr_models$label[[i]])
    btr_parameters$EF_cat <- c(btr_parameters$EF_cat, btr_models$data[[i]]$EF_cat[1]) }
}



btr_parameters_df <- as.data.frame(btr_parameters)

sum(is.na(btr_parameters_df))
nrow(btr_parameters_df)

############################################ Energy
e_data <- filt10_dat %>% filter(EF_cat == "Energy")

e_data <- e_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, e_data)


by_label <- e_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(e_data$y.z, na.rm = TRUE)
initial_Km  <- e_data$x.z[which.min(abs(e_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
e_function <- function(e_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = e_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", e_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
e_models <- by_label %>%
  mutate(model = map(data, e_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
e_parameters <- list(Vmax = numeric(),
                     Km = numeric(),
                     label= character(),
                     EF_cat = character())

for (i in seq_along(e_models$model)) {
  model <- e_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    e_parameters$Vmax <- c(e_parameters$Vmax, coef["Vmax"])
    e_parameters$Km <- c(e_parameters$Km, coef["Km"])
    e_parameters$label <- c(e_parameters$label, e_models$label[[i]])
    e_parameters$EF_cat <- c(e_parameters$EF_cat, e_models$data[[i]]$EF_cat[1]) } 
  else {
    e_parameters$Vmax <- c(e_parameters$Vmax, NA)
    e_parameters$Km <- c(e_parameters$Km, NA)
    e_parameters$label <- c(e_parameters$label, e_models$label[[i]])
    e_parameters$EF_cat <- c(e_parameters$EF_cat, e_models$data[[i]]$EF_cat[1]) }
}



e_parameters_df <- as.data.frame(e_parameters)

sum(is.na(e_parameters_df))
nrow(e_parameters_df)

############################################ Evapotranspiration
et_data <- filt10_dat %>% filter(EF_cat == "Evapotranspiration")
et_data <- et_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, et_data)



by_label <- et_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(et_data$y.z, na.rm = TRUE)
initial_Km  <- et_data$x.z[which.min(abs(et_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
et_function <- function(et_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = et_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", et_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
et_models <- by_label %>%
  mutate(model = map(data, et_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
et_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(et_models$model)) {
  model <- et_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    et_parameters$Vmax <- c(et_parameters$Vmax, coef["Vmax"])
    et_parameters$Km <- c(et_parameters$Km, coef["Km"])
    et_parameters$label <- c(et_parameters$label, et_models$label[[i]])
    et_parameters$EF_cat <- c(et_parameters$EF_cat, et_models$data[[i]]$EF_cat[1]) } 
  else {
    et_parameters$Vmax <- c(et_parameters$Vmax, NA)
    et_parameters$Km <- c(et_parameters$Km, NA)
    et_parameters$label <- c(et_parameters$label, et_models$label[[i]])
    et_parameters$EF_cat <- c(et_parameters$EF_cat, et_models$data[[i]]$EF_cat[1]) }
}



et_parameters_df <- as.data.frame(et_parameters)

sum(is.na(et_parameters_df))
nrow(et_parameters_df)

############################################ Food and feed
ff_data <- filt10_dat %>% filter(EF_cat == "Food_and_feed")
ff_data <- ff_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, ff_data)


by_label <- ff_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(ff_data$y.z, na.rm = TRUE)
initial_Km  <- ff_data$x.z[which.min(abs(ff_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
ff_function <- function(ff_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = ff_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", ff_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
ff_models <- by_label %>%
  mutate(model = map(data, ff_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
ff_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(ff_models$model)) {
  model <- ff_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    ff_parameters$Vmax <- c(ff_parameters$Vmax, coef["Vmax"])
    ff_parameters$Km <- c(ff_parameters$Km, coef["Km"])
    ff_parameters$label <- c(ff_parameters$label, ff_models$label[[i]])
    ff_parameters$EF_cat <- c(ff_parameters$EF_cat, ff_models$data[[i]]$EF_cat[1]) } 
  else {
    ff_parameters$Vmax <- c(ff_parameters$Vmax, NA)
    ff_parameters$Km <- c(ff_parameters$Km, NA)
    ff_parameters$label <- c(ff_parameters$label, ff_models$label[[i]])
    ff_parameters$EF_cat <- c(ff_parameters$EF_cat, ff_models$data[[i]]$EF_cat[1]) }
}



ff_parameters_df <- as.data.frame(ff_parameters)

sum(is.na(ff_parameters_df))
nrow(ff_parameters_df)

############################################ Formation protection and decontamination of soils and sediments
fpdss_data <- filt10_dat %>% filter(EF_cat == "Formation_protection_and_decontamination_of_soils_and_sediments")

fpdss_data <- fpdss_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, fpdss_data)


by_label <- fpdss_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(fpdss_data$y.z, na.rm = TRUE)
initial_Km  <- fpdss_data$x.z[which.min(abs(fpdss_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
fpdss_function <- function(fpdss_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = fpdss_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", fpdss_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
fpdss_models <- by_label %>%
  mutate(model = map(data, fpdss_function))


## Now need to extract the parameters for each model
# make blank list to put parameters in
fpdss_parameters <- list(Vmax = numeric(),
                         Km = numeric(),
                         label= character(),
                         EF_cat = character())

for (i in seq_along(fpdss_models$model)) {
  model <- fpdss_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    fpdss_parameters$Vmax <- c(fpdss_parameters$Vmax, coef["Vmax"])
    fpdss_parameters$Km <- c(fpdss_parameters$Km, coef["Km"])
    fpdss_parameters$label <- c(fpdss_parameters$label, fpdss_models$label[[i]])
    fpdss_parameters$EF_cat <- c(fpdss_parameters$EF_cat, fpdss_models$data[[i]]$EF_cat[1]) } 
  else {
    fpdss_parameters$Vmax <- c(fpdss_parameters$Vmax, NA)
    fpdss_parameters$Km <- c(fpdss_parameters$Km, NA)
    fpdss_parameters$label <- c(fpdss_parameters$label, fpdss_models$label[[i]])
    fpdss_parameters$EF_cat <- c(fpdss_parameters$EF_cat, fpdss_models$data[[i]]$EF_cat[1]) }
}



fpdss_parameters_df <- as.data.frame(fpdss_parameters)

sum(is.na(fpdss_parameters_df))
nrow(fpdss_parameters_df)

############################################ Habitat creation and maintenance
hcm_data <- filt10_dat %>% filter(EF_cat == "Habitat_creation_and_maintenance")
hcm_data <- hcm_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, hcm_data)


by_label <- hcm_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(hcm_data$y.z, na.rm = TRUE)
initial_Km  <- hcm_data$x.z[which.min(abs(hcm_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
hcm_function <- function(hcm_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = hcm_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", hcm_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
hcm_models <- by_label %>%
  mutate(model = map(data, hcm_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
hcm_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(hcm_models$model)) {
  model <- hcm_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    hcm_parameters$Vmax <- c(hcm_parameters$Vmax, coef["Vmax"])
    hcm_parameters$Km <- c(hcm_parameters$Km, coef["Km"])
    hcm_parameters$label <- c(hcm_parameters$label, hcm_models$label[[i]])
    hcm_parameters$EF_cat <- c(hcm_parameters$EF_cat, hcm_models$data[[i]]$EF_cat[1]) } 
  else {
    hcm_parameters$Vmax <- c(hcm_parameters$Vmax, NA)
    hcm_parameters$Km <- c(hcm_parameters$Km, NA)
    hcm_parameters$label <- c(hcm_parameters$label, hcm_models$label[[i]])
    hcm_parameters$EF_cat <- c(hcm_parameters$EF_cat, hcm_models$data[[i]]$EF_cat[1]) }
}



hcm_parameters_df <- as.data.frame(hcm_parameters)

sum(is.na(hcm_parameters_df))
nrow(hcm_parameters_df)

############################################ Maintenance of options
mo_data <- filt10_dat %>% filter(EF_cat == "Maintenance_of_options")
mo_data <- mo_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, mo_data)



by_label <- mo_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(mo_data$y.z, na.rm = TRUE)
initial_Km  <- mo_data$x.z[which.min(abs(mo_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
mo_function <- function(mo_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = mo_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", mo_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
mo_models <- by_label %>%
  mutate(model = map(data, mo_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
mo_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(mo_models$model)) {
  model <- mo_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    mo_parameters$Vmax <- c(mo_parameters$Vmax, coef["Vmax"])
    mo_parameters$Km <- c(mo_parameters$Km, coef["Km"])
    mo_parameters$label <- c(mo_parameters$label, mo_models$label[[i]])
    mo_parameters$EF_cat <- c(mo_parameters$EF_cat, mo_models$data[[i]]$EF_cat[1]) } 
  else {
    mo_parameters$Vmax <- c(mo_parameters$Vmax, NA)
    mo_parameters$Km <- c(mo_parameters$Km, NA)
    mo_parameters$label <- c(mo_parameters$label, mo_models$label[[i]])
    mo_parameters$EF_cat <- c(mo_parameters$EF_cat, mo_models$data[[i]]$EF_cat[1]) }
}



mo_parameters_df <- as.data.frame(mo_parameters)

sum(is.na(mo_parameters_df))
nrow(mo_parameters_df)

############################################ Materials companionship and labor
mcl_data <- filt10_dat %>% filter(EF_cat == "Materials_compansionship_and_labor",y.z<0)

mcl_data <- mcl_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, mcl_data)


by_label <- mcl_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(mcl_data$y.z, na.rm = TRUE)
initial_Km  <- mcl_data$x.z[which.min(abs(mcl_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function
mcl_function <- function(mcl_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = mcl_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", mcl_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
mcl_models <- by_label %>%
  mutate(model = map(data, mcl_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
mcl_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(mcl_models$model)) {
  model <- mcl_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    mcl_parameters$Vmax <- c(mcl_parameters$Vmax, coef["Vmax"])
    mcl_parameters$Km <- c(mcl_parameters$Km, coef["Km"])
    mcl_parameters$label <- c(mcl_parameters$label, mcl_models$label[[i]])
    mcl_parameters$EF_cat <- c(mcl_parameters$EF_cat, mcl_models$data[[i]]$EF_cat[1]) } 
  else {
    mcl_parameters$Vmax <- c(mcl_parameters$Vmax, NA)
    mcl_parameters$Km <- c(mcl_parameters$Km, NA)
    mcl_parameters$label <- c(mcl_parameters$label, mcl_models$label[[i]])
    mcl_parameters$EF_cat <- c(mcl_parameters$EF_cat, mcl_models$data[[i]]$EF_cat[1]) }
}



mcl_parameters_df <- as.data.frame(mcl_parameters)

sum(is.na(mcl_parameters_df))
nrow(mcl_parameters_df)

############################################ Multifunctionality
mf_data <- filt10_dat %>% filter(EF_cat == "Multifunctionality")

mf_data <- mf_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, mf_data)



by_label <- mf_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(mf_data$y.z, na.rm = TRUE)
initial_Km  <- mf_data$x.z[which.min(abs(mf_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
mf_function <- function(mf_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = mf_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", mf_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
mf_models <- by_label %>%
  mutate(model = map(data, mf_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
mf_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(mf_models$model)) {
  model <- mf_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    mf_parameters$Vmax <- c(mf_parameters$Vmax, coef["Vmax"])
    mf_parameters$Km <- c(mf_parameters$Km, coef["Km"])
    mf_parameters$label <- c(mf_parameters$label, mf_models$label[[i]])
    mf_parameters$EF_cat <- c(mf_parameters$EF_cat, mf_models$data[[i]]$EF_cat[1]) } 
  else {
    mf_parameters$Vmax <- c(mf_parameters$Vmax, NA)
    mf_parameters$Km <- c(mf_parameters$Km, NA)
    mf_parameters$label <- c(mf_parameters$label, mf_models$label[[i]])
    mf_parameters$EF_cat <- c(mf_parameters$EF_cat, mf_models$data[[i]]$EF_cat[1]) }
}



mf_parameters_df <- as.data.frame(mf_parameters)

sum(is.na(mf_parameters_df))
nrow(mf_parameters_df)
############################################ Freshwater NPP
fnpp_data <- filt10_dat %>% filter(EF_cat == "Freshwater_NPP")

fnpp_data <- fnpp_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, fnpp_data)



by_label <- fnpp_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(fnpp_data$y.z, na.rm = TRUE)
initial_Km  <- fnpp_data$x.z[which.min(abs(fnpp_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
fnpp_function <- function(fnpp_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = fnpp_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", fnpp_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
fnpp_models <- by_label %>%
  mutate(model = map(data, fnpp_function))


## Now need to extract the parameters for each model
# make blank list to put parameters in
fnpp_parameters <- list(Vmax = numeric(),
                        Km = numeric(),
                        label= character(),
                        EF_cat = character())

for (i in seq_along(fnpp_models$model)) {
  model <- fnpp_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    fnpp_parameters$Vmax <- c(fnpp_parameters$Vmax, coef["Vmax"])
    fnpp_parameters$Km <- c(fnpp_parameters$Km, coef["Km"])
    fnpp_parameters$label <- c(fnpp_parameters$label, fnpp_models$label[[i]])
    fnpp_parameters$EF_cat <- c(fnpp_parameters$EF_cat, fnpp_models$data[[i]]$EF_cat[1]) } 
  else {
    fnpp_parameters$Vmax <- c(fnpp_parameters$Vmax, NA)
    fnpp_parameters$Km <- c(fnpp_parameters$Km, NA)
    fnpp_parameters$label <- c(fnpp_parameters$label, fnpp_models$label[[i]])
    fnpp_parameters$EF_cat <- c(fnpp_parameters$EF_cat, fnpp_models$data[[i]]$EF_cat[1]) }
}



fnpp_parameters_df <- as.data.frame(fnpp_parameters)

sum(is.na(fnpp_parameters_df))
nrow(fnpp_parameters_df)
############################################ Marine npp
mnpp_data <- filt10_dat %>% filter(EF_cat == "Marine_NPP", y.z<1.5)

mnpp_data <- mnpp_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, mnpp_data)



by_label <- mnpp_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(mnpp_data$y.z, na.rm = TRUE)
initial_Km  <- mnpp_data$x.z[which.min(abs(mnpp_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
mnpp_function <- function(mnpp_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = mnpp_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", mnpp_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
mnpp_models <- by_label %>%
  mutate(model = map(data, mnpp_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
mnpp_parameters <- list(Vmax = numeric(),
                        Km = numeric(),
                        label= character(),
                        EF_cat = character())

for (i in seq_along(mnpp_models$model)) {
  model <- mnpp_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    mnpp_parameters$Vmax <- c(mnpp_parameters$Vmax, coef["Vmax"])
    mnpp_parameters$Km <- c(mnpp_parameters$Km, coef["Km"])
    mnpp_parameters$label <- c(mnpp_parameters$label, mnpp_models$label[[i]])
    mnpp_parameters$EF_cat <- c(mnpp_parameters$EF_cat, mnpp_models$data[[i]]$EF_cat[1]) } 
  else {
    mnpp_parameters$Vmax <- c(mnpp_parameters$Vmax, NA)
    mnpp_parameters$Km <- c(mnpp_parameters$Km, NA)
    mnpp_parameters$label <- c(mnpp_parameters$label, mnpp_models$label[[i]])
    mnpp_parameters$EF_cat <- c(mnpp_parameters$EF_cat, mnpp_models$data[[i]]$EF_cat[1]) }
}



mnpp_parameters_df <- as.data.frame(mnpp_parameters)

sum(is.na(mnpp_parameters_df))
nrow(mnpp_parameters_df)

############################################ Terrestrial NPP
tnpp_data <- filt10_dat %>% filter(EF_cat == "Terrestrial_NPP")

tnpp_data <- tnpp_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, tnpp_data)



by_label <- tnpp_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(tnpp_data$y.z, na.rm = TRUE)
initial_Km  <- tnpp_data$x.z[which.min(abs(tnpp_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
tnpp_function <- function(tnpp_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = tnpp_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", tnpp_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
tnpp_models <- by_label %>%
  mutate(model = map(data, tnpp_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
tnpp_parameters <- list(Vmax = numeric(),
                        Km = numeric(),
                        label= character(),
                        EF_cat = character())

for (i in seq_along(tnpp_models$model)) {
  model <- tnpp_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    tnpp_parameters$Vmax <- c(tnpp_parameters$Vmax, coef["Vmax"])
    tnpp_parameters$Km <- c(tnpp_parameters$Km, coef["Km"])
    tnpp_parameters$label <- c(tnpp_parameters$label, tnpp_models$label[[i]])
    tnpp_parameters$EF_cat <- c(tnpp_parameters$EF_cat, tnpp_models$data[[i]]$EF_cat[1]) } 
  else {
    tnpp_parameters$Vmax <- c(tnpp_parameters$Vmax, NA)
    tnpp_parameters$Km <- c(tnpp_parameters$Km, NA)
    tnpp_parameters$label <- c(tnpp_parameters$label, tnpp_models$label[[i]])
    tnpp_parameters$EF_cat <- c(tnpp_parameters$EF_cat, tnpp_models$data[[i]]$EF_cat[1]) }
}



tnpp_parameters_df <- as.data.frame(tnpp_parameters)

sum(is.na(tnpp_parameters_df))
nrow(tnpp_parameters_df)

############################################ Oceanic C sequestration
ocs_data <- filt10_dat %>% filter(EF_cat == "Oceanic_C_sequestration")

ocs_data <- ocs_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, ocs_data)




by_label <- ocs_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(ocs_data$y.z, na.rm = TRUE)
initial_Km  <- ocs_data$x.z[which.min(abs(ocs_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function
ocs_function <- function(ocs_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = ocs_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", ocs_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
ocs_models <- by_label %>%
  mutate(model = map(data, ocs_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
ocs_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(ocs_models$model)) {
  model <- ocs_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    ocs_parameters$Vmax <- c(ocs_parameters$Vmax, coef["Vmax"])
    ocs_parameters$Km <- c(ocs_parameters$Km, coef["Km"])
    ocs_parameters$label <- c(ocs_parameters$label, ocs_models$label[[i]])
    ocs_parameters$EF_cat <- c(ocs_parameters$EF_cat, ocs_models$data[[i]]$EF_cat[1]) } 
  else {
    ocs_parameters$Vmax <- c(ocs_parameters$Vmax, NA)
    ocs_parameters$Km <- c(ocs_parameters$Km, NA)
    ocs_parameters$label <- c(ocs_parameters$label, ocs_models$label[[i]])
    ocs_parameters$EF_cat <- c(ocs_parameters$EF_cat, ocs_models$data[[i]]$EF_cat[1]) }
}



ocs_parameters_df <- as.data.frame(ocs_parameters)

sum(is.na(ocs_parameters_df))
nrow(ocs_parameters_df)


############################################ Pollination and dispersal
pd_data <- filt10_dat %>% filter(EF_cat == "Pollination_and_dispersal_of_seeds_and_other_propagules", x.z <0.15)

pd_data <- pd_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, pd_data)


by_label <- pd_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(pd_data$y.z, na.rm = TRUE)
initial_Km  <- pd_data$x.z[which.min(abs(pd_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
pd_function <- function(pd_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = pd_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", pd_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
pd_models <- by_label %>%
  mutate(model = map(data, pd_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
pd_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(pd_models$model)) {
  model <- pd_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    pd_parameters$Vmax <- c(pd_parameters$Vmax, coef["Vmax"])
    pd_parameters$Km <- c(pd_parameters$Km, coef["Km"])
    pd_parameters$label <- c(pd_parameters$label, pd_models$label[[i]])
    pd_parameters$EF_cat <- c(pd_parameters$EF_cat, pd_models$data[[i]]$EF_cat[1]) } 
  else {
    pd_parameters$Vmax <- c(pd_parameters$Vmax, NA)
    pd_parameters$Km <- c(pd_parameters$Km, NA)
    pd_parameters$label <- c(pd_parameters$label, pd_models$label[[i]])
    pd_parameters$EF_cat <- c(pd_parameters$EF_cat, pd_models$data[[i]]$EF_cat[1]) }
}



pd_parameters_df <- as.data.frame(pd_parameters)

sum(is.na(pd_parameters_df))
nrow(pd_parameters_df)

############################################ Regulation of air quality
raq_data <- filt10_dat %>% filter(EF_cat == "Regulation_of_air_quality")
raq_data <- raq_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, raq_data)




by_label <- raq_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(raq_data$y.z, na.rm = TRUE)
initial_Km  <- raq_data$x.z[which.min(abs(raq_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
raq_function <- function(raq_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = raq_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", raq_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
raq_models <- by_label %>%
  mutate(model = map(data, raq_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
raq_parameters <- list(Vmax = numeric(),
                       Km = numeric(),
                       label= character(),
                       EF_cat = character())

for (i in seq_along(raq_models$model)) {
  model <- raq_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    raq_parameters$Vmax <- c(raq_parameters$Vmax, coef["Vmax"])
    raq_parameters$Km <- c(raq_parameters$Km, coef["Km"])
    raq_parameters$label <- c(raq_parameters$label, raq_models$label[[i]])
    raq_parameters$EF_cat <- c(raq_parameters$EF_cat, raq_models$data[[i]]$EF_cat[1]) } 
  else {
    raq_parameters$Vmax <- c(raq_parameters$Vmax, NA)
    raq_parameters$Km <- c(raq_parameters$Km, NA)
    raq_parameters$label <- c(raq_parameters$label, raq_models$label[[i]])
    raq_parameters$EF_cat <- c(raq_parameters$EF_cat, raq_models$data[[i]]$EF_cat[1]) }
}



raq_parameters_df <- as.data.frame(raq_parameters)

sum(is.na(raq_parameters_df))
nrow(raq_parameters_df)

############################################ Regulation of climate
rc_data <- filt10_dat %>% filter(EF_cat == "Regulation_of_climate")
rc_data <- rc_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, rc_data)



by_label <- rc_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(rc_data$y.z, na.rm = TRUE)
initial_Km  <- rc_data$x.z[which.min(abs(rc_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
rc_function <- function(rc_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = rc_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", rc_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
rc_models <- by_label %>%
  mutate(model = map(data, rc_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
rc_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(rc_models$model)) {
  model <- rc_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    rc_parameters$Vmax <- c(rc_parameters$Vmax, coef["Vmax"])
    rc_parameters$Km <- c(rc_parameters$Km, coef["Km"])
    rc_parameters$label <- c(rc_parameters$label, rc_models$label[[i]])
    rc_parameters$EF_cat <- c(rc_parameters$EF_cat, rc_models$data[[i]]$EF_cat[1]) } 
  else {
    rc_parameters$Vmax <- c(rc_parameters$Vmax, NA)
    rc_parameters$Km <- c(rc_parameters$Km, NA)
    rc_parameters$label <- c(rc_parameters$label, rc_models$label[[i]])
    rc_parameters$EF_cat <- c(rc_parameters$EF_cat, rc_models$data[[i]]$EF_cat[1]) }
}



rc_parameters_df <- as.data.frame(rc_parameters)

sum(is.na(rc_parameters_df))
nrow(rc_parameters_df)
############################################ Regulation of detrimental organisms and biological processes
rdobp_data <- filt10_dat %>% filter(EF_cat == "Regulation_of_detrimental_organisms_and_biological_processes")

rdobp_data <- rdobp_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, rdobp_data)




by_label <- rdobp_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(rdobp_data$y.z, na.rm = TRUE)
initial_Km  <- rdobp_data$x.z[which.min(abs(rdobp_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
rdobp_function <- function(rdobp_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = rdobp_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", rdobp_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
rdobp_models <- by_label %>%
  mutate(model = map(data, rdobp_function))


## Now need to extract the parameters for each model
# make blank list to put parameters in
rdobp_parameters <- list(Vmax = numeric(),
                         Km = numeric(),
                         label= character(),
                         EF_cat = character())

for (i in seq_along(rdobp_models$model)) {
  model <- rdobp_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    rdobp_parameters$Vmax <- c(rdobp_parameters$Vmax, coef["Vmax"])
    rdobp_parameters$Km <- c(rdobp_parameters$Km, coef["Km"])
    rdobp_parameters$label <- c(rdobp_parameters$label, rdobp_models$label[[i]])
    rdobp_parameters$EF_cat <- c(rdobp_parameters$EF_cat, rdobp_models$data[[i]]$EF_cat[1]) } 
  else {
    rdobp_parameters$Vmax <- c(rdobp_parameters$Vmax, NA)
    rdobp_parameters$Km <- c(rdobp_parameters$Km, NA)
    rdobp_parameters$label <- c(rdobp_parameters$label, rdobp_models$label[[i]])
    rdobp_parameters$EF_cat <- c(rdobp_parameters$EF_cat, rdobp_models$data[[i]]$EF_cat[1]) }
}



rdobp_parameters_df <- as.data.frame(rdobp_parameters)

sum(is.na(rdobp_parameters_df))
nrow(rdobp_parameters_df)

############################################ REgulation of freshwater and coastal water quality
rfcwq_data <- filt10_dat %>% filter(EF_cat == "Regulation_of_freshwater_and_coastal_water_quality")
rfcwq_data <- rfcwq_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, rfcwq_data)



by_label <- rfcwq_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(rfcwq_data$y.z, na.rm = TRUE)
initial_Km  <- rfcwq_data$x.z[which.min(abs(rfcwq_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
rfcwq_function <- function(rfcwq_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = rfcwq_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", rfcwq_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
rfcwq_models <- by_label %>%
  mutate(model = map(data, rfcwq_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
rfcwq_parameters <- list(Vmax = numeric(),
                         Km = numeric(),
                         label= character(),
                         EF_cat = character())

for (i in seq_along(rfcwq_models$model)) {
  model <- rfcwq_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    rfcwq_parameters$Vmax <- c(rfcwq_parameters$Vmax, coef["Vmax"])
    rfcwq_parameters$Km <- c(rfcwq_parameters$Km, coef["Km"])
    rfcwq_parameters$label <- c(rfcwq_parameters$label, rfcwq_models$label[[i]])
    rfcwq_parameters$EF_cat <- c(rfcwq_parameters$EF_cat, rfcwq_models$data[[i]]$EF_cat[1]) } 
  else {
    rfcwq_parameters$Vmax <- c(rfcwq_parameters$Vmax, NA)
    rfcwq_parameters$Km <- c(rfcwq_parameters$Km, NA)
    rfcwq_parameters$label <- c(rfcwq_parameters$label, rfcwq_models$label[[i]])
    rfcwq_parameters$EF_cat <- c(rfcwq_parameters$EF_cat, rfcwq_models$data[[i]]$EF_cat[1]) }
}



rfcwq_parameters_df <- as.data.frame(rfcwq_parameters)

sum(is.na(rfcwq_parameters_df))
nrow(rfcwq_parameters_df)

############################################ Regulation of hazards and extreme events
rhee_data <- filt10_dat %>% filter(EF_cat == "Regulation_of_hazards_and_extreme_events")
rhee_data <- rhee_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, rhee_data)



by_label <- rhee_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(rhee_data$y.z, na.rm = TRUE)
initial_Km  <- rhee_data$x.z[which.min(abs(rhee_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
rhee_function <- function(rhee_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = rhee_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", rhee_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
rhee_models <- by_label %>%
  mutate(model = map(data, rhee_function))



## Now need to extract the parameters for each model
# make blank list to put parameters in
rhee_parameters <- list(Vmax = numeric(),
                        Km = numeric(),
                        label= character(),
                        EF_cat = character())

for (i in seq_along(rhee_models$model)) {
  model <- rhee_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    rhee_parameters$Vmax <- c(rhee_parameters$Vmax, coef["Vmax"])
    rhee_parameters$Km <- c(rhee_parameters$Km, coef["Km"])
    rhee_parameters$label <- c(rhee_parameters$label, rhee_models$label[[i]])
    rhee_parameters$EF_cat <- c(rhee_parameters$EF_cat, rhee_models$data[[i]]$EF_cat[1]) } 
  else {
    rhee_parameters$Vmax <- c(rhee_parameters$Vmax, NA)
    rhee_parameters$Km <- c(rhee_parameters$Km, NA)
    rhee_parameters$label <- c(rhee_parameters$label, rhee_models$label[[i]])
    rhee_parameters$EF_cat <- c(rhee_parameters$EF_cat, rhee_models$data[[i]]$EF_cat[1]) }
}



rhee_parameters_df <- as.data.frame(rhee_parameters)

sum(is.na(rhee_parameters_df))
nrow(rhee_parameters_df)

############################################ Supporting identities
si_data <- filt10_dat %>% filter(EF_cat == "Supporting_identities")
si_data <- si_data %>% group_by(label)

# plot to check for outliers
dev.off()
plot(y.z~x.z, si_data)



by_label <- si_data %>% 
  group_by(label) %>% 
  nest()

# Make initial values for the nls
initial_Vmax <- max(si_data$y.z, na.rm = TRUE)
initial_Km  <- si_data$x.z[which.min(abs(si_data$y.z-(initial_Vmax/2)))]

# Define the model fitting function 
si_function <- function(si_data) {
  tryCatch({
    nls(y.z ~ (Vmax*x.z)/(Km+x.z), data = si_data, start = list(Vmax = initial_Vmax, Km = initial_Km), control = nls.lm.control(maxiter = 1024))
  }, error = function(e) {
    print(paste("Error fitting model for label:", si_data$label[1]))
    print(e)
    return(NULL)
  })
}

# Apply the model fitting function to each subset of data
si_models <- by_label %>%
  mutate(model = map(data, si_function))





## Now need to extract the parameters for each model
# make blank list to put parameters in
si_parameters <- list(Vmax = numeric(),
                      Km = numeric(),
                      label= character(),
                      EF_cat = character())

for (i in seq_along(si_models$model)) {
  model <- si_models$model[[i]]
  coef <- coefficients(model)
  
  if (!is.null(coef)) {
    si_parameters$Vmax <- c(si_parameters$Vmax, coef["Vmax"])
    si_parameters$Km <- c(si_parameters$Km, coef["Km"])
    si_parameters$label <- c(si_parameters$label, si_models$label[[i]])
    si_parameters$EF_cat <- c(si_parameters$EF_cat, si_models$data[[i]]$EF_cat[1]) } 
  else {
    si_parameters$Vmax <- c(si_parameters$Vmax, NA)
    si_parameters$Km <- c(si_parameters$Km, NA)
    si_parameters$label <- c(si_parameters$label, si_models$label[[i]])
    si_parameters$EF_cat <- c(si_parameters$EF_cat, si_models$data[[i]]$EF_cat[1]) }
}



si_parameters_df <- as.data.frame(si_parameters)

sum(is.na(si_parameters_df))
nrow(si_parameters_df)



######################################################################
# rbind the parameters_df for each 

parameters_df <- rbind(carbon_parameters_df, bpe_parameters_df, btr_parameters_df,
                       e_parameters_df, et_parameters_df,ff_parameters_df, fpdss_parameters_df,
                       hcm_parameters_df,mo_parameters_df,
                       mcl_parameters_df, mf_parameters_df,fnpp_parameters_df,
                       mnpp_parameters_df,mnpp_parameters_df,tnpp_parameters_df,
                       ocs_parameters_df,pd_parameters_df,raq_parameters_df,
                       rc_parameters_df,rdobp_parameters_df,rfcwq_parameters_df,
                       rhee_parameters_df, si_parameters_df)

# Merge parameters with the rest of the data
totaldata <- merge(parameters_df,filt10_dat , by = c("label","EF_cat"))

# get rid of raw data
totaldata <- totaldata[, -c(14,16,24,25)]
str(totaldata)
totaldata$continent <- as.character(totaldata$continent)

# Remove missing and repeated values
clean_data <- na.omit(totaldata)
uclean_data <- unique(clean_data)






# RWA 

###############################
# Function for producing rwa results 
###############################
library(boot)
library(purrr)
suppressPackageStartupMessages(library(tidyverse))
library(rwa)
library(broom)
library(knitr)
colnames(uclean_data_numeric)


############## ALL DATA VMAX

# select predicted variables to test for collinearity
predictors <- c("EF_cat","Prec","Temp", "continent","spatial_scale" , "long","lat",
                "landuse_intensity", "start_year","landuse","biome", "duration_yrs")


# test for collinearity following default settings by package creator
selected_variables <- collinear(
  uclean_data, 
  response = "Vmax",
  predictors,
  preference_order = NULL, 
  max_cor = 0.75,
  max_vif = 5, 
  encoding_method = 'mean',
)
selected_variables
head(uclean_data)


#rwa doesn't like categorical variables so make dummy ones
test_matrix <- model_matrix(~EF_cat + continent + biome + landuse + landuse_intensity -1, data = uclean_data)
head(test_matrix)
# add to uclean_data_numeric just so I don't have to rewrite it all
uclean_data_numeric <- data.frame(uclean_data,test_matrix)

write.csv(uclean_data_numeric, "another_clean_data_numeric.csv", row.names = FALSE)

# Set inputs
df <- uclean_data_numeric
outcome <- "Vmax"
predictors <- c("spatial_scale","lat", "duration_yrs", "start_year","long","Prec", "EF_catEvapotranspiration",
                "EF_catFood_and_feed","EF_catFormation_protection_and_decontamination_of_soils_and_sediments",
                "EF_catHabitat_creation_and_maintenance", "EF_catMaterials_compansionship_and_labor","EF_catMultifunctionality",
                "EF_catRegulation_of_detrimental_organisms_and_biological_processes","EF_catRegulation_of_freshwater_and_coastal_water_quality",
                "EF_catRegulation_of_hazards_and_extreme_events", "EF_catTerrestrial_C_sequestration","EF_catTerrestrial_NPP",
                "continentAntarctica","continentAsia","continentAustralia","continentEurope","continentNorth.America","continentSouth.America",
                "biomemarine","biometerrestrial","landuseintermediate_secondary_vegetation","landusemature_secondary_vegetation",
                "landusenot_relevant","landuseother..experimental.grassland.restoration",
                "landuseother..experimental.plots.established.using.species.native.to.regional.floodplains","landuseother..grassland.age.classified.as.0...10...20..and.30.year",
                "landuseother..mesocosm.experiment.mimicking.grassland","landuseother..unsure..university.campus...range.of.tree.species.planted.86.years.prior.to.study",
                "landuseother..Wetland","landusepasture","landuseplantation","landuseprimary_vegetation","landusesecondary_vegeation_age_indeterminate",
                "landuseunknown","landuseurban","landuseyoung_secondary_vegetation","landuse_intensitylight","landuse_intensityminimal","landuse_intensityunknown")


#Run rwa on the data
myrwa <- rwa(df, outcome = outcome, predictors = predictors)


#Print Results----
print(myrwa$result)
allvmaxresult <- myrwa$result

write.csv(allvmaxresult,"allvmaxresult1.csv",row.names = FALSE)


# model selected variables
model <- lm(Vmax~spatial_scale + EF_cat + landuse_intensity+biome+ landuse +
              continent + lat + duration_yrs + start_year + long + Prec,
            uclean_data)
summary(model)
# scaled model
formula <- Vmax~spatial_scale + EF_cat + landuse_intensity+ biome+landuse +
  continent + lat + duration_yrs + start_year + long + Prec
scaled_data <- lapply(uclean_data_numeric[, all.vars(formula)], scale) 
scaled_model <- lm(Vmax~spatial_scale + EF_cat + landuse_intensity+biome+landuse +
                     continent + lat + duration_yrs + start_year + long + Prec,
                   scaled_data)
summary(scaled_model)




########### ALL DATA KM

predictors <- c("EF_cat","Prec","Temp", "continent","spatial_scale" , "long","lat",
                "landuse_intensity", "start_year","landuse","biome", "duration_yrs")


# Km for all data
selected_variables <- collinear(
  uclean_data,
  response = "Km", 
  predictors,
  preference_order = NULL,
  max_cor = 0.75, 
  max_vif = 5, 
  encoding_method = 'mean', 
)
selected_variables


df <- uclean_data_numeric
outcome <- "Km"
predictors <- c("spatial_scale","lat", "duration_yrs", "start_year","long","Prec", "EF_catEvapotranspiration",
                "EF_catFood_and_feed","EF_catFormation_protection_and_decontamination_of_soils_and_sediments",
                "EF_catHabitat_creation_and_maintenance", "EF_catMaterials_compansionship_and_labor","EF_catMultifunctionality",
                "EF_catRegulation_of_detrimental_organisms_and_biological_processes","EF_catRegulation_of_freshwater_and_coastal_water_quality",
                "EF_catRegulation_of_hazards_and_extreme_events", "EF_catTerrestrial_C_sequestration","EF_catTerrestrial_NPP",
                "continentAntarctica","continentAsia","continentAustralia","continentEurope","continentNorth.America","continentSouth.America",
                "biomemarine","biometerrestrial","landuseintermediate_secondary_vegetation","landusemature_secondary_vegetation",
                "landusenot_relevant","landuseother..experimental.grassland.restoration",
                "landuseother..experimental.plots.established.using.species.native.to.regional.floodplains","landuseother..grassland.age.classified.as.0...10...20..and.30.year",
                "landuseother..mesocosm.experiment.mimicking.grassland","landuseother..unsure..university.campus...range.of.tree.species.planted.86.years.prior.to.study",
                "landuseother..Wetland","landusepasture","landuseplantation","landuseprimary_vegetation","landusesecondary_vegeation_age_indeterminate",
                "landuseunknown","landuseurban","landuseyoung_secondary_vegetation","landuse_intensitylight","landuse_intensityminimal","landuse_intensityunknown")


#Run rwa on the data
myrwa <- rwa(df, outcome = outcome, predictors = predictors)
allkmresult <- myrwa$result
print(myrwa$result)
write.csv(allkmresult, "allkmresult1.csv", row.names = FALSE)


# model selected variables
model <- lm(Km~spatial_scale + biome +landuse_intensity + start_year + duration_yrs +
              landuse +long+ EF_cat  + lat +continent+ Prec,
            uclean_data_numeric)
summary(model)
# scaled model
formula <- Km~spatial_scale + biome +landuse_intensity + start_year + duration_yrs +
  landuse +long+ EF_cat  + lat +continent+ Prec
scaled_data <- lapply(uclean_data_numeric[, all.vars(formula)], scale) 
scaled_model <- lm(Km~spatial_scale + biome +landuse_intensity + start_year + duration_yrs +
                     landuse +long+ EF_cat  + lat +continent+ Prec,
                   scaled_data)
summary(scaled_model)




########## CARBON VMAX
# Predictors for carbon no EF_cat or biome
c_predictors <- c("Temp","Prec","continent", "start_year", "landuse","lat", "long",
                  "landuse_intensity",  "spatial_scale", "duration_yrs")

# Make carbon data frame
u_carbon <- uclean_data %>% filter(EF_cat == "Terrestrial_C_sequestration")


#rwa doesn't like categorical variables so am going to make dummy ones
test_matrix <- model_matrix(~continent + landuse + landuse_intensity -1, data = u_carbon)
head(test_matrix)
u_carbon_numeric <- data.frame(u_carbon,test_matrix)
colnames(u_carbon_numeric)


# find non-collinear variables
selected_variables <- collinear(
  u_carbon,
  response = "Vmax",
  c_predictors, 
  preference_order = NULL,
  max_cor = 0.75,
  max_vif = 5, 
  encoding_method = 'mean', 
)
selected_variables

df <- u_carbon_numeric
outcome <- "Vmax"
predictors <- c("lat", "duration_yrs","Prec",
                "continentAsia","continentEurope","continentNorth.America","continentSouth.America",
                "landusemature_secondary_vegetation",
                "landusenot_relevant","landuseother..experimental.grassland.restoration",
                "landuseother..grassland.age.classified.as.0...10...20..and.30.year",
                "landusepasture","landuseprimary_vegetation","landusesecondary_vegeation_age_indeterminate",
                "landuse_intensityunknown")


#Run rwa on the data
myrwa <- rwa(df, outcome = outcome, predictors = predictors)
carbonvmaxresult <- myrwa$result
print(myrwa$result)
write.csv(carbonvmaxresult, "carbonvmaxresult1.csv", row.names = FALSE)


# model selected predictors
model <- lm(Vmax ~ landuse + duration_yrs + lat + continent + landuse_intensity +
              Prec + long, ucarbon_numeric)
summary(model)
# scaled model
formula <- Vmax ~ landuse + duration_yrs + lat + continent + landuse_intensity +
  Prec + long
scaled_data <- lapply(ucarbon_numeric[, all.vars(formula)], scale) 
scaled_model <- lm(Vmax ~ landuse + duration_yrs + lat + continent + landuse_intensity +
                     Prec + long,
                   scaled_data)
summary(scaled_model)



#### CARBON DATA KM
selected_variables <- collinear(
  u_carbon, 
  response = "Km", 
  c_predictors, 
  preference_order = NULL, 
  max_cor = 0.75, 
  max_vif = 5, 
  encoding_method = 'mean'
)
selected_variables
str(u_carbon_numeric)
df <- u_carbon_numeric
outcome <- "Km"
predictors <- c("lat", "duration_yrs","Prec", 
                "continentAsia","continentEurope","continentNorth.America","continentSouth.America",
                "landusemature_secondary_vegetation",
                "landusenot_relevant","landuseother..experimental.grassland.restoration",
                "landuseother..grassland.age.classified.as.0...10...20..and.30.year",
                "landusepasture","landuseprimary_vegetation","landusesecondary_vegeation_age_indeterminate",
                "landuse_intensityunknown")


#Run rwa on the data
myrwa <- rwa(df, outcome = outcome, predictors = predictors)
carbonkmresult <- myrwa$result
print(myrwa$result)
write.csv(carbonkmresult, "carbonkmresult1.csv", row.names = FALSE)

# model selected parameters
model <- lm(Km~ landuse + duration_yrs + landuse_intensity + lat + continent +
              Prec, ucarbon_numeric)
summary(model)
# scaled model
formula <- Km~ landuse + duration_yrs + landuse_intensity + lat + continent +
  Prec
scaled_data <- lapply(ucarbon_numeric[, all.vars(formula)], scale) 
scaled_model <- lm(Km~ landuse + duration_yrs + landuse_intensity + lat + continent +
                     Prec,
                   scaled_data)
summary(scaled_model)



### Plot results
rwa_result <- read.csv("Final_results1.csv", h=T)
all_result <- rwa_result %>% filter(Data == "All")
carbon_result <- rwa_result %>% filter(Data == "Carbon")


dev.off()
par(mfrow=c(1,2))
p1 <- ggplot(all_result, aes(x=reorder(Variables,Rescaled.RelWeight), y=Rescaled.RelWeight, fill=Parameter))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust=1, size = 14), 
        axis.title=element_text(size=14), 
        plot.title=element_text(size=14), 
        legend.text=element_text(size=14), 
        legend.title=element_text(size=14))+
  labs(x ="Variable",y ="Relative importance (%)")+
  scale_fill_brewer(palette = "Paired")

p2<-ggplot(carbon_result, aes(x=reorder(Variables,Rescaled.RelWeight), y=Rescaled.RelWeight, fill=Parameter))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14), 
        axis.title=element_text(size=14),
        plot.title=element_text(size=14), 
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))+
  labs(x ="Variable",y ="Relative importance (%)")+
  scale_fill_brewer(palette = "Paired")
p2

library(ggpubr)
ggarrange(p1, p2,
          ncol = 1, nrow = 2)


########################################################################
# LAND USE AND BIODIVERSITY ANALYSIS
########################################################################

install.packages(c("terra", "raster", "sf"), dependencies = TRUE)

library(ncf)
library(raster)
library(terra)
library(sf)
library(spdep)
library(SpatialPack)
library(spatialreg)
library(nlme)
library(spgwr)
library(spmoran)




# load land use data
tiff_path_g <- "lulc-human-modification-terrestrial-systems_geographic.tif"
land_g <- raster(tiff_path_g)
land_g
plot(land_g)
dev.off()
land_g_df <- as.data.frame(land_g, xy=TRUE)
# Reduce resolution by a factor of 10
agg_land <- aggregate(land_g, fact=10, fun=mean)
agg_land <- raster::readAll(agg_land)
agg_land_df <- as.data.frame(agg_land, xy = TRUE)

plot(agg_land)

# Convert to spatial pixels data frame
land_g_spdf <- as(agg_land, 'SpatialPixelsDataFrame')
land_g_spdf
summary(land_g_spdf)

# Convert to sf
land_g_sf <- st_as_sf(land_g_spdf)
plot(st_geometry(land_g_sf), pch = 20, cex = 0.5)
summary(land_g_sf)

# make all data an sf object
allEFs_sf <- st_as_sf(uclean_data, coords = c("long","lat"),crs = "+proj=longlat +datum=WGS84 +no_defs ")



#############################################################################
land_g_spdf$category <- cut(land_g_spdf$lulc.human.modification.terrestrial.systems_geographic, 
                            breaks = c(0, 0.33, 0.66, 1), 
                            labels = c("minimal", "light", "intense"), 
                            include.lowest = TRUE)
plot(land_g_spdf)


# Read in natural earth world map
ne110 <- st_read("practical_data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# Group by continent and summarize the data
continent_summary <- ne110 %>%
  group_by(CONTINENT) %>%  # Replace continent_column with the actual column name
  summarize(geometry = st_union(geometry), .groups=drop)


# make sure to only have unique values 
uallEFs_sf <- unique(allEFs_sf)
boxplot(uallEFs_sf$Vmax~uallEFs_sf$continent)
hist(allEFs_sf$Vmax)
head(uallEFs_sf)
# filter to just carbon sequestration
ucarbon_sf <- uallEFs_sf %>% filter(EF_cat =="Terrestrial_C_sequestration")
head(ucarbon_sf)
unique(allEFs_sf$EF_cat)
# test for normality
shapiro.test(uallEFs_sf$Vmax)
shapiro.test(uallEFs_sf$Km)
shapiro.test(ucarbon_sf$Vmax)
shapiro.test(uallEFs_sf$Km)

# test for differences between groups
kruskal.test(Vmax ~continent, data = uallEFs_sf)
kruskal.test(Km ~continent, data = uallEFs_sf)
kruskal.test(Vmax ~continent, data = ucarbon_sf)
kruskal.test(Km ~continent, data = ucarbon_sf)

# run post hoc
dunnTest(Vmax ~ continent, data = uallEFs_sf, method = "bh")
dunnTest(Km ~ continent, data = uallEFs_sf, method = "bh")
dunnTest(Vmax ~ continent, data = ucarbon_sf, method = "bh")
dunnTest(Km ~ continent, data = ucarbon_sf, method = "bh")


# filter by continent
africa <- uallEFs_sf %>% filter(continent == "Africa")
asia <- uallEFs_sf %>% filter(continent == "Asia")
australia <- uallEFs_sf %>% filter(continent == "Australia")
europe <- uallEFs_sf %>% filter(continent == "Europe")
northamerica <- uallEFs_sf %>% filter(continent == "North America")
southamerica <- uallEFs_sf %>% filter(continent == "South America")
africa <- ucarbon_sf %>% filter(continent == "Africa")
asia <- ucarbon_sf %>% filter(continent == "Asia")
australia <- ucarbon_sf %>% filter(continent == "Australia")
europe <- ucarbon_sf %>% filter(continent == "Europe")
northamerica <- ucarbon_sf %>% filter(continent == "North America")
southamerica <- ucarbon_sf %>% filter(continent == "South America")

# consider medians for each continent
median(africa$Km)
median(asia$Km)
median(australia$Km)
median(europe$Km)
median(northamerica$Km)
median(southamerica$Km)



############### What land use intensity has highest EFs


# categorise land use intensity
land_g_sf$category <- cut(land_g_sf$lulc.human.modification.terrestrial.systems_geographic, 
                          breaks = c(0, 0.33, 0.66, 1), 
                          labels = c("minimal", "light", "intense"), 
                          include.lowest = TRUE)
head(land_g_sf)

# join to main data
bef_data <- data.frame(EF = allEFs_sf$EF_cat, Vmax = allEFs_sf$Vmax, Km = allEFs_sf$Km, continent = allEFs_sf$continent, geometry = allEFs_sf$geometry)
bef_data <- st_as_sf(bef_data)
st_crs(allEFs_sf)
st_crs(land_g_sf)
joined_data <- st_join(bef_data, land_g_sf, join = st_nearest_feature, left = T)
ujoined_data <- unique(joined_data)
carbonjoined_data <- ujoined_data %>% filter(EF == "Terrestrial_C_sequestration")

# test for differences in land use for each parameter
kruskal.test(Vmax ~ category, data = ujoined_data)
kruskal.test(Km ~ category, data = ujoined_data)
kruskal.test(Vmax ~ category, data = carbonjoined_data)
kruskal.test(Km ~ category, data = carbonjoined_data)
# post hoc
dunnTest(Vmax ~ category, data = ujoined_data, method = "bh")
dunnTest(Km ~ category, data = ujoined_data, method = "bh")
dunnTest(Vmax ~ category, data = carbonjoined_data, method = "bh")
dunnTest(Km ~ category, data = carbonjoined_data, method = "bh")

# filter by land use
minimal <- ujoined_data %>% filter(category == "minimal")
light <- ujoined_data %>% filter(category == "light")
intense <- ujoined_data %>% filter(category == "intense")
minimal <- carbonjoined_data %>% filter(category == "minimal")
light <- carbonjoined_data %>% filter(category == "light")
intense <- carbonjoined_data %>% filter(category == "intense")

# compare medians
median(minimal$Km)
median(light$Km)
median(intense$Km)

head(allEFs_sf)


## intersection of continent and land use
# Not included in report
ideal_vmax <- ujoined_data %>% filter(category =="light" & continent == "Asia")
head(ideal_vmax)
nrow(ideal_vmax)
hist(ideal_vmax$Vmax, breaks = 56)


# find top 50% of vmax
top_50_percent <- ujoined_data %>%
  filter(percent_rank(Vmax) >= 0.5)
nrow(top_50_percent) # 118
head(top_50_percent)

yet_again <- st_intersects(top_50_percent, ideal_vmax, sparse = FALSE)
num_matches <- sum(rowSums(yet_again) > 0)
num_matches #30
# 30/118 --> 25% of the highest Vmax are both in asia and in light land use intensity
nrow(intersections$Vmax)
head(yet)

# carbon
ideal_Cvmax <- carbonjoined_data %>% filter(category =="light" & continent == "North America")
# find top 50% of vmax
top_50_percent <- carbonjoined_data %>%
  filter(percent_rank(Vmax) >= 0.5)
nrow(top_50_percent) # 21
yet_againC <- st_intersects(top_50_percent, ideal_Cvmax, sparse = FALSE)
num_matches <- sum(rowSums(yet_againC) > 0)
num_matches # 4
# 4 / 21 --> 19% of the highest Vmax for carbon are both in North america and light land use

########
# Biodiversity hotspots
# load biodiversity data
hotspots <- st_read("hotspots_2016_1.shp")
print(hotspots)

# make sure shape file is valid, filter to just information on land
hotspots <- hotspots %>% filter(Type == "hotspot area")
hotspots <- st_make_valid(hotspots)
plot(st_geometry(hotspots))
# filter corrupted polygon
plot(st_geometry(hotspots[26, ]))
hotspots <- hotspots[-26, ]



# Combine data
# top 50% Vmax in hotspots
allEFs_bio <-st_intersection(top_50_percent,hotspots)
# top 50% Vmax not in hotspots
allEFs_nbio <- st_difference(top_50_percent, st_union(hotspots))

# code them as 1 in or 0 out
allEFs_bio$in_hotspot <- 1
allEFs_nbio$in_hotspot <- 0
# add missing columns to nbio with NAs
allEFs_nbio$NAME <- NA
allEFs_nbio$Type <- NA

str(allEFs_bio)
colnames(allEFs_bio)
str(allEFs_nbio)
colnames(allEFs_nbio)

# combine rows
total_allEFs_bio <- rbind(allEFs_bio,allEFs_nbio)


# double check for normality
shapiro.test(total_allEFs_bio$Vmax)
# kruskal
kruskal.test(total_allEFs_bio$Vmax, total_allEFs_bio$in_hotspot)
# no significant difference H = 1.4429, df=1, p = 0.2297


# lets look at everything not just top 50%

# Combine data
# 
allEFs_bio2 <-st_intersection(ujoined_data,hotspots)
# Vmax not in hotspots
allEFs_nbio2 <- st_difference(ujoined_data, st_union(hotspots))

# code them as 1 in or 0 out
allEFs_bio2$in_hotspot <- 1
allEFs_nbio2$in_hotspot <- 0
# add missing columns to nbio with NAs
allEFs_nbio2$NAME <- NA
allEFs_nbio2$Type <- NA

str(allEFs_bio)
colnames(allEFs_bio2)
str(allEFs_nbio)
colnames(allEFs_nbio2)

# combine rows
total_allEFs_bio2 <- rbind(allEFs_bio2,allEFs_nbio2)

dev.off()
boxplot(total_allEFs_bio2$Vmax~total_allEFs_bio2$in_hotspot)

# double check for normality
shapiro.test(total_allEFs_bio2$Vmax)
# kruskal
kruskal.test(total_allEFs_bio2$Vmax, total_allEFs_bio2$in_hotspot)
# no significant difference between data in hotspot or not H = 2.6744, df = 1, p = 0.102

## Looking at Km
boxplot(total_allEFs_bio2$Km~total_allEFs_bio2$in_hotspot)

# double check for normality
shapiro.test(total_allEFs_bio2$Km)
# kruskal
kruskal.test(total_allEFs_bio2$Km, total_allEFs_bio2$in_hotspot)
# no significant difference between data in hotspot or not H = 0.486, df = 1, p = 0.4853



#### looking at carbon
# Combine data
# 
allEFs_bio3 <-st_intersection(carbonjoined_data,hotspots)
# Vmax not in hotspots
allEFs_nbio3 <- st_difference(carbonjoined_data, st_union(hotspots))

# code them as 1 in or 0 out
allEFs_bio3$in_hotspot <- 1
allEFs_nbio3$in_hotspot <- 0
# add missing columns to nbio with NAs
allEFs_nbio3$NAME <- NA
allEFs_nbio3$Type <- NA

str(allEFs_bio)
colnames(allEFs_bio3)
str(allEFs_nbio)
colnames(allEFs_nbio3)

# combine rows
total_allEFs_bio3 <- rbind(allEFs_bio3,allEFs_nbio3)

dev.off()
boxplot(total_allEFs_bio3$Vmax~total_allEFs_bio3$in_hotspot)

# double check for normality
shapiro.test(total_allEFs_bio3$Vmax)
# kruskal
kruskal.test(total_allEFs_bio3$Vmax, total_allEFs_bio3$in_hotspot)
# no significant difference between data in hotspot or not H = 0.907, df = 1, p = 0.3409

## Looking at Km
boxplot(total_allEFs_bio3$Km~total_allEFs_bio3$in_hotspot)
table(total_allEFs_bio3$in_hotspot)

total_allEFs_bio3$in_hotspot <- as.factor(total_allEFs_bio3$in_hotspot)
# double check for normality
shapiro.test(total_allEFs_bio3$Km)
# kruskal
kruskal.test(total_allEFs_bio3$Km, total_allEFs_bio3$in_hotspot)
# significant difference between data in hotspot or not H = 4.5134, df = 1, p = 0.03363
wilcox.test(Km~in_hotspot, data = total_allEFs_bio3)
# significant difference W = 102, p = 0.03031

# Compare medians
median(allEFs_bio3$Km)
median(allEFs_nbio3$Km)


unique(total_allEFs_bio2$EF)




##### Create plots of maps 


library(dplyr)
str(agg_land)
# convert land use data to data frame
agg_land_df <- as.data.frame(agg_land, xy= TRUE)
agg_land_df<- na.omit(agg_land_df)
head(agg_land_df)
# Categorise land use intensity
agg_land_df$category <- cut(agg_land_df$lulc.human.modification.terrestrial.systems_geographic, 
                            breaks = c(0, 0.33, 0.66, 1), 
                            labels = c("minimal", "light", "intense"), 
                            include.lowest = TRUE)

str(ne110)
library(ggplot2)
summary(agg_land)

# remove antarctica and ocean data
filt_ne110 <- ne110 %>% filter(CONTINENT != "Antarctica" & CONTINENT != "Seven seas (open ocean)")

# Get the colors from the "Paired" palette
palette_colors <- brewer.pal(n = 9, name = "Blues")

# Choose specific colors
chosen_colors <- palette_colors[c(2,6,8)]

m2 <-ggplot() +
  geom_sf(data = filt_ne110,fill = "aliceblue", color = "grey20") + 
  #scale_fill_brewer(palette = "Blues", direction = -1)+
  theme_grey() +
  #labs(fill = "Continent") +
  geom_sf(data=hotspots, fill = "deeppink", color = NA, alpha = 0.7)+
  geom_sf(data=allEFs_sf,aes(size=Vmax), alpha=0.5 )+
  scale_size_continuous(range = c(-2, 12))
m2


m <- ggplot() +
  geom_raster(data = agg_land_df, aes(x = x, y = y, fill = category)) +  # Raster layer
  scale_fill_manual(values = chosen_colors)+ 
  geom_sf(data = filt_ne110,fill = NA, color = "grey10", alpha = 0)+
 # geom_sf(data=hotspots, fill = "deeppink", color = NA, alpha = 0.7)+
  theme_grey()
m

ggarrange(m, m2,
          ncol = 1, nrow = 2)
