########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Nov  6 21:04:34 2011
########################################################
#This program analyzes the discrepancy in the homicide
#data for the state of Sinaloa. In 2007 and 2008 there
#were more drug-war related murders than total murders
#which, of course, should be impossible. This program shows
#the discrepancy was due to an excess of deaths registered
#as accidents whose cause of injury was a firearm.

library(ggplot2)
library(stringr)
library(lubridate)
library(directlabels)
library(testthat)
library(ascii)
options(asciiType = "org")

theme_set(theme_bw())

########################################################
#Load the data
########################################################

deaths <- read.csv("data/sinaloa-deaths.csv.bz2", fileEncoding = "utf-8")
drug.homicides <- read.csv("data/drug-homicides.csv.bz2", fileEncoding = "utf-8")
municipality.heads <- read.csv("data/municipality-heads.csv", fileEncoding = "utf-8")

#recode the database with the injury intent mortality matrix
source("src/codeMM.R")
#Unit testing
source("tests/tests.R")
#Create the graphs
source("src/analyze.R")

