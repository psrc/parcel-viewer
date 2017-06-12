library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)

wrkdir <- '/home/shiny/apps/parcel-viewer' # shiny path
# wrkdir <- 'C:/Users/CLam/Desktop/'

data <- 'parcel-viewer/data'

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

