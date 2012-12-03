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


#If you run the classifiers it will take a long time to run


#libraries needed for the analysis
source(file.path("src", "load-libraries.R"))
#load all injury intent deaths in the state of Sinaloa
source(file.path("src", "load-data.R"))
#recode the database with the injury intent mortality matrix
source(file.path("src", "codeMM.R"))
#Unit testing
test_dir("tests")
#Create the graphs
source(file.path("src", "analyze.R"))

#Try and classify accidents and homicides
##source(file.path("src", "multiple-imputation.R"))
source(file.path("src", "classify.R"))


