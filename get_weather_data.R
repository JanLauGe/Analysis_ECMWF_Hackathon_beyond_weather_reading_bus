
library(ncdf4)
library(raster)
library(MASS)

#setwd('/home/ma/majw/Notebooks/data_service_catalog/')

# open the netcdf file
setwd('C:/Users/Laurens/Dropbox/projects/ecmwf/data/')
ncdffile <- nc_open('temp.nc', write=FALSE)

# Get variable
variable <- ncdffile$var[[3]]
# Get values for the entire grid
valuegrid <- ncvar_get(ncdffile, varid = variable)

#retrieve latitude and longitude  information
lon <- which(tmp$dim$lon$vals == 358.50)
lat <- which(tmp$dim$lat$vals == 51.00)
# Get value of the specific cell we are interested in
valuegrid[lon, lat, 1]

# close the netcdf file
close(ncdffile)
