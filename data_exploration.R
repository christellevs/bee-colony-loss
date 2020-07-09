# ***********************************************************************************
# R File to hold data exploration and visualisation plotting functions
# ***********************************************************************************

# *****************************************************************************
# ------------------- Gerneral Visualisation Functions ------------------------

# Centres title for a plot
CentrePlotTitle <- function() {
  return(theme(plot.title = element_text(hjust = 0.5)))
}

# ************************************************
# GetAxisLim() :
# INPUT:  Function   - limit function (max or min)
#         Datacolumn_var -
#         rounder    -
#         divisor    - 
# OUTPUT: 
# ************************************************
GetAxisLim <- function(extremity, column_var, rounder = 1, divisor = 1) {
  lim = 0
  if(extremity == "max") {
    lim = max(column_var)
  }
  else {
    lim = min(column_var)
  }
  
  if (rounder == 1) {
    lim = ceiling(lim/divisor)
  }
  else if (rounder == 0.5) {
    lim = ceiling(lim*2/divisor)/2
  }
  return (lim)
}


# *****************************************************************************
# ------------------------- Linear Visualisation ------------------------------

# Linear plotting of total colonies in USA over the years
LinearPlotTotalColonies <- function() {
  
  # Setting y limit for aesthetic graph purposes
  y_max_lim <- GetAxisLim("max", data_pesticide_per_year$total_colonies,
                          0.5, TOTAL_COLONY_DIVISOR)
  
  total_colonies <- ggplot(
    data = data_pesticide_per_year,
    aes(x = data_pesticide_per_year$year,
        y = data_pesticide_per_year$total_colonies/TOTAL_COLONY_DIVISOR)) +
    geom_line(size = 0.8) +
    geom_point() +
    ylim(0, y_max_lim) +
    labs(title = "Total Bee Colonies in USA\n",
         x = "\nYear",
         y = "Colony numbers (millions)\n") +
    CentrePlotTitle()
  
  print(total_colonies)
}

# Linear plot to comparing differnt neonic pesticide usage
# against total neonic use for each year
LinearPlotNeonicTypes <- function() {
  
  # Melt dataframe to create only one column to plot - uses 'reshape2' lib
  neonic_melted <- melt(data_neonic_types, id = "year")
  
  pesticide_types <- ggplot(
    neonic_melted,
    aes(x = year,
        y = value/TOTAL_NEONIC_DIVISOR,
        color = variable)) +
    geom_line(size = 0.8)+
    geom_point() +
    labs(title = "Total pesticides applied\n",
         x = "\nYear",
         y = "Amount of pesticides\napplied (tonnes)\n",
         color = "Neonic Type") +
    CentrePlotTitle()
  
  print(pesticide_types)
}


# Graph plot for bee summary
PlotBeeGraphSummary <- function() {
  
  bee_graph_summary <- data_all_bee %>%
    ggplot(aes(x = log(nAllNeonic),
               y = log(total_annual_loss_colonies),
               color = year,
               size = colonies/SUMMARY_COLONY_DIVISOR)) +
    geom_point(alpha=0.3) +
    geom_smooth(method = lm) +
    facet_wrap(~region) +
    labs(title = "Bee data summary\n",
         x = TeX("\n $\\log{}$( Neonics applied (kg) )"), 
         y = TeX("$\\log{}$( Annual bee loss )\n"), # Using Latex lib to format log
         color = "Year",
         size = "Bee colony numbers\n (thousands)") +
    CentrePlotTitle()
  
  print(bee_graph_summary)
}


# *****************************************************************************
# ---------------------------- Map Visualisation ------------------------------

# For USA maps

# To normalise gradient scale between maps
NormaliseGradientScale <- function(column1, column2) {
  max_vals <- data.frame(pmax(column1, column2))
  return(ceiling(max(max_vals)*2)/2)
}



CreateUsaMap <- function(region_scale, dataframe, data_variable,
                         low_lim, high_lim, label_name, chart_title) {
  
  usa_map <- plot_usmap(regions = region_scale,
                        data = dataframe, values = data_variable, color = "black") +
    scale_fill_gradient(low = "white", high = "red",
                        limits = c(low_lim, high_lim), name = label_name) +
    theme(plot.title = element_text(hjust = 0.5),legend.position = "right") +
    labs(title = chart_title)
  
  print(usa_map)
  
}


# *****************************************************************************
# --------------------------------- END ---------------------------------------
# *****************************************************************************



