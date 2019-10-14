# Packages ----
library(tidyverse)  # Hadley Wickham's tidyverse - the theme of this tutorial
library(broom)  # To summarise model outputs
library(ggExtra)  # To make pretty graphs - addon package to ggplot2
library(maps)  # To make pretty maps - warning: maps masks map from purr!
library(RColorBrewer)  # To make pretty colours
library(gridExtra)  # To arrange multi-plot panels

# Load data ----
load("LPDdata_Feb2016.RData")

# Inspect data ----
head(LPDdata_Feb2016)

# Format data for analysis ----

# Transform from wide to long format usign gather (opposite is spread)
LPD_long <- gather(data = LPDdata_Feb2016, key = "year", value = "pop", select = 26:70)

# Get rid of the X in front of years # *** parse_number() from the readr package in the tidyverse ***
LPD_long$year <- parse_number(LPD_long$year)

# Rename variable names for consistency
names(LPD_long)
names(LPD_long) <- tolower(names(LPD_long))
names(LPD_long)

# Create new column with genus and species together
LPD_long$species.name <- paste(LPD_long$genus, LPD_long$species, sep = " ")

# Get rid of strange characters like " / "
LPD_long$country.list <- gsub(",", "", LPD_long$country.list, fixed = TRUE)
LPD_long$biome <- gsub("/", "", LPD_long$biome, fixed = TRUE)

# Examine the tidy data frame
head(LPD_long)
view(LPD_long)


# Data manipulation ----

# *** piping from from dplyr
LPD_long2 <- LPD_long %>%
  # Remove duplicate rows 
  distinct(LPD_long) %>%
  # remove NAs in the population column 
  filter(is.finite(pop)) %>%
  # Group rows so that each group is one population
  group_by(id) %>%  
  # Make some calculations
  mutate(maxyear = max(year), minyear = min(year),
         duration = maxyear - minyear, # Calculate duration
         scalepop = (pop - min(pop))/(max(pop) - min(pop))) %>% # Scale population trend data
         filter(is.finite(scalepop), 
                length(unique(year)) > 5) %>% # Keep populations with >5 years worth of data and calculate length of monitoring
  ungroup () # Remove any groupings you've greated in the pipe

#------------------------------------------------------------
# Calculate summary statistics for each biome
LPD_biome_sum <- LPD_long2 %>%
  group_by(biome) %>%
  # Create columns, number of populations
  summarise(populations = n(),   
            mean_study_length_years = mean(duration),
            dominant_sampling_method = names(which.max(table(sampling.method))), # Model sampling method
            dominant_units = names(which.max(table(units)))) %>% # Model unit type
  ungroup()

# Take a look at some of the records
head(LPD_biome_sum)  

#===================================================================
#Subset to just temperate forest species -----
  # Notice the difference between | and & when filtering  # | is an "or" whereas & is "and", i.e. both conditions have to be met at the same time
  LPD_long2$biome <- as.factor(LPD_long2$biome)
  LPD.forest <- filter(LPD_long2, biome == "Temperate broadleaf and mixed forests" | biome == "Temperate coniferous forests")

#------------------------
  theme_LPD <- function(){
    theme_bw()+
      theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14, face = "plain"),             
            axis.title.y = element_text(size = 14, face = "plain"),             
            panel.grid.major.x = element_blank(),                                          
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),  
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
            plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
            legend.text = element_text(size = 12, face = "italic"),          
            legend.title = element_blank(),                              
            legend.position = c(0.5, 0.8))
  }
#------------------------
# Data visualisation ----
  # Data distribution - a histogram
  (forest.hist <- ggplot(LPD.forest, aes(x = scalepop)) +
     geom_histogram(aes(fill = biome),
                    position = "identity", colour = "grey40") +
     geom_vline(aes(xintercept = mean(scalepop)),  # Adding a line for mean abundance
                colour = "darkred", linetype = "dashed", size = 1) +
     scale_fill_manual(values = c("#66CD00", "#53868B")) +
     theme_LPD() +
     labs(title = "a) Data distribution\n") +
     guides(fill = F)) # Hiding the legend - this will be a two plot panel -  thus we don't need the same legend twice

  # Density histogram of duration of studies in the two biomes
  (duration.forests <- ggplot(LPD.forest, aes(duration, colour = biome)) +
      stat_density(geom = "line", size = 2, position = "identity") +
      theme_LPD() +
      scale_colour_manual(values = c("#66CD00", "#53868B")) +
      labs(x = "\nYears", y = "Density\n", title = "b) Monitoring duration\n"))
  
  forest.panel <- grid.arrange(forest.hist, duration.forests, ncol = 2)
  ggsave(forest.panel, file = "forest_panel.png", height = 5, width = 10)

#################
#We are now ready to model how each population has changed over time. There are 1785 populations, 
#so with this one code chunk, we will run 1785 models and tidy up their outputs. 
#NOTE -- when you add the lm() function in a pipe, you have to add data = ., 
#which means use the outcome of the previous step in the pipe for the model.
  
# Calculate population change for each forest population -  1785 models in one go!
  #"do" function - let's you use external functions that a

forest.slopes <- LPD.forest %>%
  # Group by the key variables that we want to interate over
  group_by(decimal.latitude, decimal.longitude, class, species.name, id, duration, location.of.population) %>%
  # Create a linear model for each group
  do(mod = lm(scalepop ~ year, data = .)) %>%
  # Extract model coefficients using tidy() from the model *** tidy() function from the broom package ***
  tidy(mod) %>%
  filter(term == "year") %>% # Filter out slopes and remove intercept values
  dplyr::select(-term) %>%
  ungroup()  

#-------------------------------------------------  
# Visualising model outputs ----
  
# Plotting slope estimates and standard errors for all populations and adding histograms along the margins
(all.slopes <- ggplot(forest.slopes, aes(x = duration, y = estimate)) +
   geom_pointrange(aes(ymin = estimate - std.error,
                       ymax = estimate + std.error),
                     alpha = 0.3, size = 0.3) +
   geom_hline(yintercept = 0, linetype = "dashed") +
     theme_LPD() +
     ylab("Population change\n") +
     xlab("\nDuration (years)"))
  
(density.slopes <- ggExtra::ggMarginal(p = all.slopes, type = 'density', margins = 'both', size = 5, col = 'gray40',fill = 'gray'))
  
  # Save the plot
  ggsave(density.slopes, filename = "slopes_duration.png", height = 6, width = 6)

  
##########################################################
  # PART 2: Using pipes to make figures with large datasets ----
  
  # Make histograms of slope estimates for each taxa -----
  # Set up new folder for figures
  # Set path to relevant path on your computer/in your repository
  path1 <- "Taxa_Forest_LPD/"
  # Create new folder
  dir.create(path1)  
  
#-------------------------------------------------------------- 
# First we will do this using dplyr and a pipe
  
forest.slopes %>%
  # Select the relevant data
  dplyr::select(id, class, species.name, estimate) %>%
  # Group by taxa
  group_by(class) %>%
  # Save all plots in new folder
  do(ggsave(ggplot(., aes(x = estimate)) +
      # Add histograms
      geom_histogram(colour = "darkgreen", fill = "darkgreen", binwidth = 0.02) +
      # Use custom theme
      theme_LPD() +
      # Add axis lables
      xlab("Rate of population change (slopes)"),
      # Set up file names to print to
      filename = gsub("", "", paste0(path1, unique(as.character(.$class)), ".pdf")), device = "pdf"))
 
  #--------------------------------
  #Now we can do the same thing using the map() function from the purrr package.
  #We have to change the format of the data, in our case we will split the data using spread() from the tidyr package.
  
  # Here we select the relevant data   # Let's get rid of the other levels of 'class'
  forest.slopes$class <- as.character(forest.slopes$class)
  # Selecting the relevant data and splitting it into a list
  taxa.slopes <- forest.slopes %>%
    dplyr::select(id, class, estimate) %>%
    spread(class, estimate) %>%
    dplyr::select(-id)
  
  #We can apply the mean function using purrr::map():
  taxa.mean <- purrr::map(taxa.slopes, ~mean(., na.rm = TRUE))
  # This plots the mean population change per taxa
  taxa.mean
  
  #Now we can write our own function to make histograms and use the purrr package to apply it to each taxa.
  
  # This function takes one argument x, the data vector that we want to make a histogram
  plot.hist <- function(x) {
    ggplot() +
      geom_histogram(aes(x), colour = "darkgreen", fill = "darkgreen", binwidth = 0.02) +
      theme_LPD() +
      xlab("Rate of population change (slopes)")
  }
  #-----------
  taxa.plots <- purrr::map(taxa.slopes, ~plot.hist(.))
  # We need to make a new folder to put these figures in
  path2 <- "Taxa_Forest_LPD_purrr/"
  dir.create(path2)
  #----------
  # *** walk2() function in purrr from the tidyverse ***
  walk2(paste0(path2, names(taxa.slopes), ".pdf"), taxa.plots, ggsave)
  