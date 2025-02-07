library(gstat)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)

#-------------------------------------------------------------------------------------------------#
#-- Settings
# setwd('C:/Users/lelan/Documents/CodeProjects/PhD_SV_AEM_T2P')
mdir <- './02_Models/Texture2Par_onlytexture_1layer'
classes <- c('FINE','MIXED_FINE','SAND','MIXED_COARSE','VERY_COARSE')
wellfile <- 'logs_and_AEM_5classes.dat'

output_file <- './05_Outputs/fitted_variograms.txt'
output_shapefile <- './01_Data/shapefiles/t2p_logs_and_AEM.shp'
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Read in wells file for x,y
locs <- read.table(file.path(mdir,wellfile), header=T, stringsAsFactors = F)
locs <- unique(locs[,c('Line','ID','X','Y')])

# wells_sf <- st_as_sf(locs, coords = c("X", "Y"), crs = 26910)
#
# # Write to a shapefile
# st_write(wells_sf, output_shapefile, delete_layer = TRUE, shape_restore_shx=T)
#
# # Print confirmation
# cat("Shapefile written to:", output_shapefile, "\n")

#-- Loop over classes
tex <- lapply(classes, function(class){
  fname <- paste0('t2p_',class,'_layavg.out')
  layavg <- read.table(file.path(mdir, fname),header = T)
  layavg <- merge(layavg, locs, by.x='Well', by.y='ID')
  layavg_long <- data.frame(
    X = rep(layavg$X, 2),
    Y = rep(layavg$Y, 2),
    texture = class,
    value = c(layavg$X1)
  )
  # Remove rows where value is -999 (missing data)
  layavg_long <- layavg_long[layavg_long$value >= 0.0,]
  coordinates(layavg_long) <- ~X+Y
  return(layavg_long)
})
tex <- setNames(tex, classes)

# Combine all into a single dataframe
#tex_df <- do.call(rbind, tex_list)
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# FINE Texture Class
vgm_fine_emp <- variogram(value ~ 1, data = tex[["FINE"]], width=600)
vgm_fine_model <- vgm(model = "Exp", nugget = 0.0, range = 6480, psill = 0.075)
vgm_fine_model <- fit.variogram(vgm_fine_emp, vgm_fine_model)
plot(vgm_fine_emp, vgm_fine_model, main = "FINE Texture Variogram")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# MIXED_FINE Texture Class
vgm_mf_emp <- variogram(value ~ 1, data = tex[["MIXED_FINE"]], width=600)
vgm_mf_model <- vgm(model = "Exp", nugget = 0.0, range = 700, psill = 0.15)
vgm_mf_model <- fit.variogram(vgm_mf_emp, vgm_mf_model)
plot(vgm_mf_emp, vgm_mf_model, main = "MIXED_FINE Texture Variogram")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# SAND Texture Class
vgm_sand_emp <- variogram(value ~ 1, data = tex[["SAND"]], width=300)
vgm_sand_model <- vgm(model = "Exp", nugget = 0, range = 1000, psill = 0.2)
vgm_sand_model <- fit.variogram(vgm_sand_emp, vgm_sand_model)
plot(vgm_sand_emp, vgm_sand_model, main = "SAND Texture Variogram")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# MIXED_COARSE Texture Class
vgm_mc_emp <- variogram(value ~ 1, data = tex[["MIXED_COARSE"]], width=500)
vgm_mc_model <- vgm(model = "Exp", nugget = 0.0, range = 1200, psill = 0.12)
vgm_mc_model <- fit.variogram(vgm_mc_emp, vgm_mc_model)
plot(vgm_mc_emp, vgm_mc_model, main = "MIXED_COARSE Texture Variogram")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# VERY_COARSE Texture Class
vgm_vc_emp <- variogram(value ~ 1, data = tex[["VERY_COARSE"]], width=600)
vgm_vc_model <- vgm(model = "Exp", nugget = 0.0, range = 800, psill = 0.08)
vgm_vc_model <- fit.variogram(vgm_vc_emp, vgm_vc_model)
plot(vgm_vc_emp, vgm_vc_model, main = "VERY_COARSE Texture Variogram")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Define function for grabbing parameters from variogram objects:
extract_vgm_params <- function(vgm_model, texture) {
  data.frame(
    texture = texture,
    model = as.character(vgm_model[2, 'model']),  # Model type (e.g., "Exp", "Sph")
    nugget = round(vgm_model[1, "psill"], 3),
    sill = round(vgm_model[2, "psill"], 3),
    range = round(vgm_model[2, "range"], 1)
  )
}
# Create dataframe with parameters
params_df <- bind_rows(
  extract_vgm_params(vgm_fine_model, "FINE"),
  extract_vgm_params(vgm_mf_model, "MIXED_FINE"),
  extract_vgm_params(vgm_sand_model, "SAND"),
  extract_vgm_params(vgm_mc_model, "MIXED_COARSE"),
  extract_vgm_params(vgm_vc_model, "VERY_COARSE")
)

# Format text labels for display
params_df$label <- with(params_df, paste0(
  "Model: ", model, "\n",
  "Nugget: ", nugget, "\n",
  "Sill: ", sill, "\n",
  "Range: ", range
))

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Get Plotting

# Define the intended order (because GGPLOT loves alphabetizing)
texture_order <- c("FINE", "MIXED_FINE", "SAND", "MIXED_COARSE", "VERY_COARSE")

# Combine empirical variogram data into a single dataframe
empirical_df <- bind_rows(
  mutate(vgm_fine_emp, texture = factor("FINE", levels = texture_order)),
  mutate(vgm_mf_emp, texture = factor("MIXED_FINE", levels = texture_order)),
  mutate(vgm_sand_emp, texture = factor("SAND", levels = texture_order)),
  mutate(vgm_mc_emp, texture = factor("MIXED_COARSE", levels = texture_order)),
  mutate(vgm_vc_emp, texture = factor("VERY_COARSE", levels = texture_order))
)

# Combine fitted variogram models into a single dataframe
fitted_df <- bind_rows(
  mutate(variogramLine(vgm_fine_model, maxdist = max(vgm_fine_emp$dist)), texture = factor("FINE", levels = texture_order)),
  mutate(variogramLine(vgm_mf_model, maxdist = max(vgm_mf_emp$dist)), texture = factor("MIXED_FINE", levels = texture_order)),
  mutate(variogramLine(vgm_sand_model, maxdist = max(vgm_sand_emp$dist)), texture = factor("SAND", levels = texture_order)),
  mutate(variogramLine(vgm_mc_model, maxdist = max(vgm_mc_emp$dist)), texture = factor("MIXED_COARSE", levels = texture_order)),
  mutate(variogramLine(vgm_vc_model, maxdist = max(vgm_vc_emp$dist)), texture = factor("VERY_COARSE", levels = texture_order))
)

params_df$texture <- factor(params_df$texture, levels = texture_order)

# Plot with annotated variogram parameters
ggplot(empirical_df, aes(x = dist, y = gamma, color = texture)) +
  geom_point(alpha = 0.7) +  # Empirical variogram points
  geom_line(data = fitted_df, aes(x = dist, y = gamma, color = texture), linetype = "dashed") +  # Fitted models
  facet_wrap(~texture, scales = "free_y", ncol = 1) +  # Arrange in a single column
  geom_text(data = params_df, aes(x = Inf, y = -Inf, label = label),
            hjust = 1.1, vjust = -0.5, inherit.aes = FALSE,
            size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Empirical Variograms and Fitted Models",
       x = "Lag Distance",
       y = "Semivariance",
       color = "Texture Class")
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Write out variogram model parameters for Texture2Par
# Define the number of nearest neighbors (nnear)
nnear <- 32

# Create a function to format each variogram entry
format_variogram_entry <- function(texture, vgm_model) {
  structure <- 1
  vtype <- as.character(vgm_model[2, 'model'])  # Extract variogram type (e.g., "Exp", "Sph")
  nugget <- sprintf("%.3f", vgm_model$psill[1])  # Format to 3 decimal places
  sill <- sprintf("%.3f", vgm_model$psill[2])  # Format to 3 decimal places
  range_min <- sprintf("%.2E", vgm_model$range[2])  # Scientific notation (e.g., 1.50E4)
  range_max <- range_min  # Assuming min/max range are the same
  ang1 <- "0.0"  # Default angle value

  # Format as a string
  entry <- paste0(
    "  CLASS ", texture, "\n",
    "           ", structure, "    ", vtype, "    ", nugget, "   ", sill,
    "    ", range_min, "       ", range_max, "  ", ang1, "    ", nnear
  )
  return(entry)
}

# Generate the variogram text block
variogram_text <- c(
  "BEGIN VARIOGRAMS",
  "  # Structure Vtype  Nugget  Sill  Range_min Range_max ang1  nnear"
)

# Append each variogram entry
variogram_text <- c(
  variogram_text,
  format_variogram_entry("Fine", vgm_fine_model),
  format_variogram_entry("Mixed_Fine", vgm_mf_model),
  format_variogram_entry("Sand", vgm_sand_model),
  format_variogram_entry("Mixed_Coarse", vgm_mc_model),
  format_variogram_entry("Very_Coarse", vgm_vc_model),
  "END VARIOGRAMS"
)

# Write to file
writeLines(variogram_text, output_file)

# Print confirmation
cat("Variogram file written to:", output_file, "\n")


#-------------------------------------------------------------------------------------------------#

