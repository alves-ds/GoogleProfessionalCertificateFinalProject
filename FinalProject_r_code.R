# Import libraries
library("tidyverse")
library("dplyr")
library("magrittr")
library("qgraph")


# Load datasets

# Sleep day
daily_sleep_data <- read.csv("data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

# Daily activity
daily_activity_data <- read.csv("data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")



# First, checking the structure of the datasets using the function str(). We can note that the date column is in the string format, so we need to convert all to a date format.
# For this, I used the function 'as.Date()'
daily_sleep_data$SleepDay <- as.Date(daily_sleep_data$SleepDay, format = "%m/%d/%Y")
daily_activity_data$ActivityDate <- as.Date(daily_activity_data$ActivityDate, format = "%m/%d/%Y")



# Secondly, lets check how many data points we have for each individual
count_datas_for_id_sleep <- table(daily_sleep_data$Id)
count_datas_for_id_activity <- table(daily_activity_data$Id)
# We note different monitoring times for the individuals


# Let's check how many individuals we have for dataset
individuals_sleep_data <- daily_sleep_data %>% distinct(Id) #24
individuals_activity_data <- daily_activity_data %>% distinct(Id) #33


# In the next step, we will merge two datasets, so we need that they share the same name for the date column
colnames(daily_activity_data)[colnames(daily_activity_data) == "ActivityDate"] ="Date"
colnames(daily_sleep_data)[colnames(daily_sleep_data) == "SleepDay"] ="Date"


# Merge the datasets based in the ID and date columns. The activity dataset already have steps, intensities and calories, so, it will be merged with sleep only
merged_data <- daily_activity_data %>%
  left_join(daily_sleep_data, by = c('Id', 'Date'))


# Now, let's check how many individuals we have in the merged dataset
merged_data_sample_size <- merged_data %>% distinct(Id) # 33


# We know that our merged dataset has 33 individuals, now we can proceed with the verification of how clean it is the data. 
# First, let organize the data based in Date, from the oldest to the newest for each ID
merged_data <- merged_data %>% arrange(Id, Date)



# Create a variable 'week' to indicate how many weeks the individuals were monitored
min_week <- min(isoweek(merged_data$Date))
merged_data <- merged_data %>%
  mutate(week = isoweek(Date) - min_week + 1)


# Having a short look we can see that the individuals were monitored through 5 weeks, but differed in the number of monitoring days in some weeks, for example in the last week in which some individuals have 4 and others have 3 days of monitoring
# Let's compute the weekly amount of steps, very active minutes, fairly active minutes, lightly active minutes, sedentary minutes, calories and total time in bed for to seed how this variables interrelate within then
weekly_merged_data <- merged_data %>%
  group_by(Id, week) %>%
  summarise(
    wk_steps = sum(TotalSteps),
    very_activemin = sum(VeryActiveMinutes),
    fairly_activemin = sum(FairlyActiveMinutes),
    lightly_activemin = sum(LightlyActiveMinutes),
    sed_min = sum(SedentaryMinutes),
    wk_calories = sum(Calories), 
    wk_timebed = sum(TotalTimeInBed),
    wk_asleep = sum(TotalMinutesAsleep)
  )

# Looking to the weekly data we can see that the variables related to sleep have a lot of missing data. 
# We can see too data related to activity with "0" values, which can be an error of measurement, so we need to be cautions when interpreting the results derived from this data.


# Let's have a look in some summary statistics from the daily data
merged_data %>%  
  select(TotalSteps,
         VeryActiveMinutes,
         FairlyActiveMinutes, 
         LightlyActiveMinutes,
         SedentaryMinutes,
         Calories) %>%
  summary()
# From the summary statistics we can see that the individuals from this sample do a mean of 7652 steps per day, being this the unique information that we can use to compare with recommendations given, because the intensity thresholds of the data do not align with the thresholds adopted from the WHO or ACSM. But we can see that the sample do more minutes of sedentary physical activity than very active or fairly active, which needs to be reduced. 


# Let's build another dataset with only the data that we want to plot with histograms
merged_data_hist <- merged_data %>% select(TotalSteps, 
                                           VeryActiveMinutes, 
                                           FairlyActiveMinutes, 
                                           LightlyActiveMinutes, 
                                           SedentaryMinutes, 
                                           Calories)

# Let's take a better look using histograms
# First, let define a function for create subplots, using a function retrieved from: https://rstudio-pubs-static.s3.amazonaws.com/285012_aca1535265c24407bf17e05741936f7f.html
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Now let's have a look into the histograms for each variable 
myplots <- list()  # new empty list
for(i in 1:ncol(merged_data_hist)){
  col <- names(merged_data_hist)[i]
  ggp <- ggplot(merged_data_hist, aes_string(x = col)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = mean(merged_data_hist[[col]]), col = "red", lwd=1.5)

  
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 3)


# Let's have a look in the behavior of the steps given per hour

# Load dataset
steps_hour <- read_csv("data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

# Convert hour from chr to a data format and posteriorly to a hour format
steps_hour$Hour <- as.POSIXct(steps_hour$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
steps_hour$Hour <- format(steps_hour$Hour, format = "%H:%M:%S")


# Estimate steps per hour
steps_h <- steps_hour %>%
  group_by(Hour) %>%
  drop_na() %>%
  summarise(mean_total_steps = mean(StepTotal))


# Plot steps per hour
ggplot(data=steps_h, aes(x=Hour, y=mean_total_steps)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Steps per hour")
# From the plot above we can see that the mean steps per day starts to increase next to 6am and starts to decrease next to 9PM, showing its peak nearby 7PM. 

# Now let's look to the weekly behavior using a network analysis. For this, first we need to filter our dataset to only the data obtained for some week. I arbitrarily used the second week of monitoring. 
data_to_network <- weekly_merged_data %>%
  filter(week == 2)

# Let's filter again the data, with only the data that will be used to estimate the network
data_to_model <- data_to_network %>% 
                ungroup() %>%
                select(wk_steps,
                       very_activemin,
                        fairly_activemin, 
                        lightly_activemin, 
                        sed_min, 
                        wk_calories)

covmat_model <- cov(data_to_model)



png(file = "graph_model.png", width=1600, height=1200, res=300)
cornet_model <- qgraph(covmat_model, layout = "spring", minimum = 0.3, graph = "pcor", nodeNames = c("Steps", "Very_active", "Fairly_active", "Lightly-active", "Sed", "Calories"), labels = colnames(data_to_model), 
                       edge.labels = FALSE, legend.mode = "names")
                        
dev.off()

centralityPlot(cornet_model)


# Destaque que os dados de sono são poucos para o modelo de redes. 