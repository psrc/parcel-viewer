library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)

# wrkdir <- '' #insert shiny path
wrkdir <- 'C:/Users/CLam/Desktop/'

data <- 'parcelviewer/data'

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

