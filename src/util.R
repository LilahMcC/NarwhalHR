
# Reading Functions -------------------------------------------------------

# read_acc creates a dataframe with columns acceleration (in g) and DateTime.
#Parameters: 
# x_path, y_path, z_path = name of text file for x,y,z axis acceleration. eg: "name_of_file.txt"
# nrow = number of rows to read. eg: 10000. Default = Inf

read_acc <- function(narwhal_id, nrow = Inf) {
  add_sec_frac <- function(time, freq_hz) {
    time + (seq(0, length(time) - 1) %% freq_hz) / freq_hz 
  }
  read_acc_axis <- function(narwhal_id, nrow, axis_name) {
    map_dfr(narwhal_id, 
            read_tsv, 
            col_names = c(axis_name, "drop", "DateTime"), 
            n_max = nrow) %>%
      mutate(DateTime = add_sec_frac(mdy_hms(DateTime), 100),# 100 Hz
             {{ axis_name }} := .data[[axis_name]] / 1000) %>% # makes units g
      select(-drop) #removes "%" column
  }
  
  accx <- read_acc_axis(find_accx(narwhal_id), nrow, "acc_x")
  accy <- read_acc_axis(find_accy(narwhal_id), nrow, "acc_y")
  accz <- read_acc_axis(find_accz(narwhal_id), nrow, "acc_z")
  
  # Assert datetimes are all the same
  stopifnot(all(accx$DateTime == accy$DateTime),
            all(accx$DateTime == accz$DateTime))
  
  accx %>% 
    mutate(acc_y = accy$acc_y,
           acc_z = accz$acc_z) %>%  #combine acc columns into one df with DateTime
    relocate(DateTime) #move DateTime col to front 
}

#read_depth processes the .txt depth file into a dataframe
#parameters: path to depth file
read_depth <- function(path){
  depth_data <- read_tsv(path) %>% #read file 
    mutate(DateTime = mdy_hms(`Date time`), .keep = "unused")
}

# Calculation Functions ---------------------------------------------------

#calculate the norm of the matrix. 
#Parameters: a 3 column matrix of columns of equal length. 
anorm <- function(xyz) {
  sqrt(rowSums(xyz * xyz))
}

#calculate the pitch of the animal
#parameters: column containing x-axis (surge) acceleration values
pitch <- function(x, anorm) {
  asin(x/anorm)
}

#calculate the roll of the animal
#parameters: 2 columns containing the y-axis (sway) and z-axis (heave) acceleration values respectively. 
roll <- function(y,z) {
  atan2(y,z)
}


# Plotting Functions ------------------------------------------------------


# plot_dive plots a dive profile given a set of parameters to minimize repetition of code when plotting dive profiles at multiple timescales.
# Parameters: 
# dataframe - must be a dataframe. 
# start_time/end_time - must be character strings in 'YYYY-MM-DD hh:mm:ss' format.
plot_dive <- function(dataframe, start_time, end_time, title = NULL, interactive = FALSE) {
  one_dive <- filter(dataframe, DateTime >= start_time, DateTime <= end_time)
  dive_plot <- ggplot(one_dive, aes(x = DateTime, y = Depth)) +
    geom_line() +
    scale_y_reverse() + #makes depth right orientation 
    labs(x = "Time",
         y = "Depth (m)",
         title = title) + #changes labels on axes and title 
    theme_bw()  #white background
  
  if(interactive == FALSE)
    dive_plot
  else ggplotly(dive_plot)
}

#plot_calculated plots anorm, roll, or pitch. 
#parameters:
#dataframe 
#start_time and end_time in YYYY-MM-DD hh:mm:ss format 
#col_name is a column name to be assigned to the y axis of the figure

plot_calculated <- function(dataframe, start_time, end_time, col_name) {
  filter(dataframe, DateTime >= start_time, DateTime <= end_time) %>% 
    ggplot(aes(DateTime, y = {{col_name}})) +
    geom_line() +
    theme_bw()
}

#plot acceleration
#parameters:
#dataframe 
#start_time and end_time in YYYY-MM-DD hh:mm:ss format 
#designed to plot each axis of triaxial acceleration data in a separate panel 
plot_acc <- function(dataframe, start_time, end_time) {
  filter(dataframe, DateTime >= start_time, DateTime <= end_time) %>% 
    ggplot(aes(x = DateTime, y = acc, color = axis)) +
    geom_line() +
    labs(x = "time",
         y = "acceleration") + #changes labels on axes and title
    theme_bw() +  #white background
    facet_grid(rows = vars(axis))
}
