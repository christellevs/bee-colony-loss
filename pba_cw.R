# **********************************************************************
# R Script for bee dataset analysis
# **********************************************************************
# University of Surrey
# Department of Computer Science
#
# COM3018: Practical Business Analytics Coursework
# Due date: Tuesday 3rd December 2019 - 16:00
#
# Group name: GiveMeFive(ppl)
#
# Group Members:
#
# Name                      URN
# Aneta Ivanova             6510974
# Christelle Van Sebroeck   6418824
# Emily Heather             6405720
# Lu Lu                     6494499
# Shanshan Wang             6473679
#
# **********************************************************************

#  clearing all objects in "global environment"
rm(list=ls())

# *********************************************************************
# Global Environment variables

DATASET_FILENAME_1       <- "pesticides.csv"
DATASET_FILENAME_2       <- "bee_loss.csv"
DATASET_FILENAME_3       <- "states.csv"

# Global variables for data preparation 
YEAR_MIN                  <- 2010
YEAR_MAX                  <- 2016

YEAR_PESTICIDES           <- 2003


# Data exploration global variables
TOTAL_COLONY_DIVISOR      <- 1000000
TOTAL_NEONIC_DIVISOR      <- 1000000
SUMMARY_COLONY_DIVISOR    <- 1000
SCIENTIFIC_VALUE          <- 100

# SOM global variables
SOM_GRIDSIZE              <- 5
SOM_EPOCHS                <- 300
SOM_TOPO_TYPE             <- "hexagonal"
SOM_LEARN_RATE            <- c(0.05, 0.01)
SOM_RADIUS                <- 1
SOM_KEEP_DATA             <- TRUE
SOM_DIST_FUNC             <- "euclidean"
SOM_K_COLUMNS             <- 10


# **************************************************************************************
# ------------------------------ Loading R Libraries -----------------------------------
#
# Library from CRAN     Version
#
# pacman	               0.5.1
# corrplot	             0.84
# factoextra             1.0.5
# formattable 	         0.2.0.1
# ggplot2                3.2.1
# gridExtra              2.3
# h2o                    3.26.0.2
# igraph                 1.2.4.1
# kohonen                3.0.8
# latex2exp              0.4.0
# MASS	                 7.3.51.4
# magrittr               1.5
# openintro              1.7.1
# outliers               0.14
# partykit               1.2.5
# PerformanceAnalytics   1.5.3
# pROC	                 1.15.3
# reshape2               1.4.3
# scales                 1.0.0
# stats                  3.6.1
# stringr                1.4.0
# usmap                  0.5.0
# dplyr                  0.8.3

MYLIBRARIES<-c("caret",
               "corrplot",
               "factoextra",
               "formattable",
               "ggplot2",
               "gridExtra",
               "h2o",
               "igraph",
               "kohonen",
               "latex2exp",
               "MASS",
               "magrittr",
               "openintro",
               "outliers",
               "partykit",
               "PerformanceAnalytics",
               "pROC",
               "reshape2",
               "scales",
               "stats",
               "stringr",
               "usmap",
               "dplyr")

# ************************************************************************************
# ----------------------------- User Defined Functions --------------------------------

LoadDataset <- function(dataset_csv) {
  return(na.omit(data.frame(read.csv(dataset_csv))))
}

RemoveNaRows <- function(dataset) {
  
  return(dataset[!apply(is.na(dataset) | dataset == "", 1, all),])
}

RenameColumn <- function(dataset, old_name, new_name) {
  names(dataset)[names(dataset) == old_name] <- new_name
}


# ************************************************************************
# -------------------------- Pre-Processing ------------------------------


# ----------------------------------------------------
# ----------------- Pesticide Dataset-----------------

# Loading main pesticide dataset
data_pesticides <- LoadDataset(DATASET_FILENAME_1)
data_pesticides <- data_pesticides[, !duplicated(colnames(data_pesticides))]
data_pesticides <- RemoveNaRows(data_pesticides)

# Rename columns in Pesticides
names(data_pesticides)[names(data_pesticides) == "numcol"] <- "bee_colony_numbers"
names(data_pesticides)[names(data_pesticides) == "yieldpercol"] <- "yield_per_col"
names(data_pesticides)[names(data_pesticides) == "totalprod"] <- "total_production"
names(data_pesticides)[names(data_pesticides) == "priceperlb"] <- "price_per_lb"
names(data_pesticides)[names(data_pesticides) == "prodvalue"] <- "production_value"
names(data_pesticides)[names(data_pesticides) == "StateName"] <- "state"
names(data_pesticides)[names(data_pesticides) == "Region"] <- "region"

# Converting state names from abbreviation to full name
data_pesticides$state <- openintro::abbr2state(data_pesticides$state)


# ----------------------------------------------------
# ----------------- Bee Loss Dataset------------------

data_bee_loss <- LoadDataset(DATASET_FILENAME_2)
data_bee_loss <- subset(data_bee_loss, select=-c(Season))

#Renameing column names for bee loss dataset
names(data_bee_loss)[names(data_bee_loss) == "Year"] <- "year"
names(data_bee_loss)[names(data_bee_loss) == "ÿState"] <- "state"
names(data_bee_loss)[names(data_bee_loss) == "ÿTotal.Annual.Loss"] <- "total_annual_loss"
names(data_bee_loss)[names(data_bee_loss) == "ÿBeekeepers"] <- "beekeepers"
names(data_bee_loss)[names(data_bee_loss) == "ÿBeekeepers.Exclusive.to.State"] <- "beekeepers_exclusive_to_state"
names(data_bee_loss)[names(data_bee_loss) == "ÿColonies"] <- "colonies"
names(data_bee_loss)[names(data_bee_loss) == "ÿColonies.Exclusive.to.State"] <- "colonies_exclusive_to_state"

# Re-formatting year value
data_bee_loss$year <- gsub("/..", "", data_bee_loss$year)

# # Removing empty / NA rows
data_bee_loss <- RemoveNaRows(data_bee_loss)


# ----------------------------------------------------
# ----------------- Merging Datasets -----------------

# Creating subset of pesticide dataset from years 2010 to 2016 inclusive
data_pest_bee_common <- subset(data_pesticides, year >= YEAR_MIN & year <= YEAR_MAX)
data_all_bee <- merge(data_pest_bee_common, data_bee_loss, by=c("year", "state"))
data_all_bee <- subset(data_all_bee, select = -c(state.1))


# Percentage conversions
# 2nd round of pre processing
# Converting 'total_annual_loss', 'colonies_exclusive_to_state', 'beekeepers_exclusive_to_state'
# from factor to numeric
# and adding a new variable 'total_annual_loss_colonies' to the all_bee_df
data_all_bee$total_annual_loss = t(data.frame(lapply(
  data_all_bee$total_annual_loss,function(x) as.numeric(gsub("\\%", "", x))/100)))

data_all_bee$total_annual_loss_colonies = data_all_bee$total_annual_loss*data_all_bee$colonies

data_all_bee$colonies_exclusive_to_state = t(data.frame(lapply(
  data_all_bee$colonies_exclusive_to_state, function(x) as.numeric(gsub("\\%", "", x))/100)))

data_all_bee$beekeepers_exclusive_to_state = t(data.frame(lapply(
  data_all_bee$beekeepers_exclusive_to_state, function(x) as.numeric(gsub("\\%", "", x))/100)))


# --------------------------------------------------------
# ------------ Visualisation data prep -------------------

# Loading list of states for appplying map visualisations
data_states = LoadDataset(DATASET_FILENAME_3)
data_states = subset(data_states, select = c(state, area_km_sq))

data_pre_pest <- subset(data_pesticides, year < YEAR_PESTICIDES,
                        select = c(state, bee_colony_numbers, yield_per_col))
data_post_pest <- subset(data_pesticides, year >= YEAR_PESTICIDES,
                         select = c(state, bee_colony_numbers, yield_per_col))

data_pre_pest <- aggregate(data_pre_pest, list(data_pre_pest$state), mean)
data_pre_pest <- subset(data_pre_pest, select = -c(state))

data_post_pest <- aggregate(data_post_pest, list(data_post_pest$state), mean)
data_post_pest <- subset(data_post_pest, select = -c(state))

names(data_pre_pest)[names(data_pre_pest) == 'Group.1'] <- 'state'
names(data_post_pest)[names(data_post_pest) == 'Group.1'] <- 'state'

data_pre_pest <- merge(data_pre_pest, data_states, by=c("state"))
data_post_pest <- merge(data_post_pest, data_states, by=c("state"))

data_pre_pest$colony_density = data_pre_pest$bee_colony_numbers/data_pre_pest$area_km_sq
data_post_pest$colony_density = data_post_pest$bee_colony_numbers/data_post_pest$area_km_sq


# Total Colonies
data_pesticide_per_year <- aggregate(data_pesticides$bee_colony_numbers,
                                     list(data_pesticides$year), sum)
names(data_pesticide_per_year)[names(data_pesticide_per_year) == "Group.1"] <- "year"
names(data_pesticide_per_year)[names(data_pesticide_per_year) == "x"] <- "total_colonies"


# Grouping by neonic
data_neonic_types <- subset(data_pesticides, select = c(year, nCLOTHIANIDIN, nIMIDACLOPRID,
                                                        nTHIAMETHOXAM, nACETAMIPRID, nTHIACLOPRID,
                                                        nAllNeonic))
data_neonic_types <- aggregate(data_neonic_types, list(data_neonic_types$year), sum)
data_neonic_types <- subset(data_neonic_types, select = -c(year))
names(data_neonic_types)[names(data_neonic_types) == "Group.1"] <- "year"


# ---------------------- End of data processing --------------------------
# ************************************************************************



# ***************************************************************************
# --------------------------------- Main ------------------------------------
# main() : Entry point to execute the bee data machine learning analysis
# INPUT  : None
# OUTPUT : None
# ***************************************************************************
main<-function() {
  
  print("Started bee data analysis")
  
  # View processed data
  NPREPROCESSING_prettyDataset(data_pesticides)
  NPREPROCESSING_prettyDataset(data_bee_loss)
  NPREPROCESSING_prettyDataset(data_all_bee)
  
  # Setting to minimise scientifically labelled axes in graphs
  options(scipen = 100)
  
  print("Finished pre-processing.")
  
  # ************************************************************************
  # --------------------- Data Modelling Preparation -----------------------
  
  print("Starting data modelling preparation...")
  
  
  # --------- Numeric Dataset Manipulation ----------
  
  # Creating dataframe with only numeric values from the bee dataset
  bee_nums <- unlist(lapply(data_all_bee, is.numeric))
  bees_numeric <- data.frame(data_all_bee[, bee_nums])
  
  NPREPROCESSING_prettyDataset(bees_numeric)
  
  print("Finished data modelling preparation.")
  
  
  # ************************************************************************
  # ------------------------ Data Exploration ------------------------------
  
  print("Starting data exploration...")
  #----------- Linear representations ------------
  LinearPlotTotalColonies()
  LinearPlotNeonicTypes()


  # ----------- USA map representations -----------

  # Normalising colony density scale
  max_col_density <- NormaliseGradientScale(
    data_pre_pest$colony_density, data_post_pest$colony_density)

  CreateUsaMap("states", data_pre_pest, "colony_density", 0,
               max_col_density, "Colony density", "Pre pesticides usage (pre 2003)")
  CreateUsaMap("states", data_post_pest, "colony_density", 0,
               max_col_density, "Colony density", "Post pesticides usage (post 2003)")


  # ------------- Main summary plot ---------------
  PlotBeeGraphSummary();
  
  print("Finished data exploration.")
  
  
  # ************************************************************************
  # ------------------------ Regression models -----------------------------

  print("Starting regression modelling...")

  Regression_lossPesticides(dataset = data_all_bee)
  Regression_multipleLinear(dataset = data_all_bee)
  Regression_productionValue(dataset = data_all_bee)
  Regression_stepwisePrediction(datase = data_all_bee)

  print("Finished regression modelling.")


  # ************************************************************************
  # ------------------------ Self Organising Maps --------------------------

  print("Started SOM modelling...")

  # Selecting dataset variables for SOM
  bee_som_vars <- subset(bees_numeric,
                         select = c(bee_colony_numbers, yield_per_col,
                                    total_production, nAllNeonic, total_annual_loss_colonies,
                                    nCLOTHIANIDIN, nIMIDACLOPRID, nTHIAMETHOXAM,
                                    nACETAMIPRID, nTHIACLOPRID))


  # Data preparation for SOM modelling - creating matrix
  bee_matrix <- SOM_unsupervised_dataPrep(bee_som_vars)

  # Creating SOM model for bees
  bee_som_model <- SOM_createModel(bee_matrix, SOM_GRIDSIZE, SOM_TOPO_TYPE,
                                   SOM_EPOCHS, SOM_LEARN_RATE,
                                   SOM_RADIUS, SOM_KEEP_DATA, SOM_DIST_FUNC)

  # Evaluating SOM model
  par(mfrow=c(1,1))
  SOM_evaluate(bee_som_model, "changes", "Training SOM")

  par(mfrow=c(1,3))
  SOM_evaluate(bee_som_model, "count", "Node Counts")
  SOM_evaluate(bee_som_model, "dist.neighbours", "SOM neighbour distances")
  SOM_evaluate(bee_som_model, "quality", "SOM quality")

  som_codes <- data.frame(bee_som_model$codes)

  # Modelling SOM variables
  par(mfrow=c(2,2))
  SOM_unsupervisedMain(bee_som_model, som_codes, 1)
  SOM_unsupervisedMain(bee_som_model, som_codes, 2)
  SOM_unsupervisedMain(bee_som_model, som_codes, 3)
  SOM_unsupervisedMain(bee_som_model, som_codes, 4)

  # Modelling SOM with different neonic variables
  par(mfrow=c(2,3))
  SOM_unsupervisedMain(bee_som_model, som_codes, 4)
  SOM_unsupervisedMain(bee_som_model, som_codes, 5)
  SOM_unsupervisedMain(bee_som_model, som_codes, 6)
  SOM_unsupervisedMain(bee_som_model, som_codes, 7)
  SOM_unsupervisedMain(bee_som_model, som_codes, 8)
  SOM_unsupervisedMain(bee_som_model, som_codes, 9)

  print("Finished SOM modelling.")

  print("Stopped bee data analysis")
} #endof main()

gc() # Garbage collection to automatically release memory

# Clearing plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clearing all warning messages
assign("last.warning", NULL, envir = baseenv())

# Set wroking directory
setwd(getwd())

print(paste("Working directory: ", getwd()))
print("START Practical Business Analytics coursework")

library(pacman)
pacman::p_load(char=MYLIBRARIES, install = TRUE, character.only = TRUE)

# Loading additional R script files
source("data_prep.R")
source("data_exploration.R")
source("models.R")

set.seed(123)

# ************************************************

main()

print("end")