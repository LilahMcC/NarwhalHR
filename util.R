#READING FUNCTIONS:

# read_acc creates a dataframe with columns acceleration (in g) and DateTime.
#Parameters: 
# x_path, y_path, z_path = name of text file for x,y,z axis acceleration. eg: "name_of_file.txt"
# nrow = number of rows to read. eg: 10000. Default = Inf

read_acc <- function(x_path, y_path, z_path, nrow = Inf) {
  read_acc_axis <- function(axis_path, nrow, axis_name){
    accx <- read_tsv(axis_path,
                     col_names = c(axis_name, "drop", "DateTime"),
                     n_max = nrow) %>%
      mutate(DateTime = mdy_hms(DateTime),
             {{ axis_name }} := .data[[axis_name]] / 1000) %>% # makes units g
      select(-drop) #removes "%" column
  }
  
  accx <- read_acc_axis(x_path, nrow, "acc_x")
  accy <- read_acc_axis(y_path, nrow, "acc_y")
  accz <- read_acc_axis(z_path, nrow, "acc_z")
  
  # Assert datetimes are all the same
  stopifnot(all(accx$DateTime == accy$DateTime),
            all(accx$DateTime == accz$DateTime))
  
  accx %>% 
    mutate(acc_y = accy$acc_y,
           acc_z = accz$acc_z) %>%  #combine acc columns into one df with DateTime
    relocate(DateTime) #move DateTime col to front 
}

#parameters: path to depth file
read_depth <- function(path){
  depth_data <- read_tsv("Depth_Asgeir.txt") %>% #read file 
    mutate(DateTime = mdy_hms(`Date time`), .keep = "unused")
}

#CALCULATION FUNCTIONS 

#anorm calculates the norm of the matrix. 
#Parameters: a 3 column matrix of columns of equal length. 
anorm <- function(xyz) {
  sqrt(rowSums(xyz * xyz))
}

#pitch 
#parameters: column containing x-axis (surge) acceleration values
pitch <- function(x, anorm) {
  pitch = -asin(x)*(anorm)
}
#roll 
#parameters: 2 columns containing the y-axis (sway) and z-axis (heave) acceleration values respectively. 
roll <- function(y, z) {
  roll = atan(y/z)
}


#PLOTTING FUNCTIONS

# plot_dive plots a dive profile given a set of parameters to minimize repetition of code when plotting dive profiles at multiple timescales.
# Parameters: 
# dataframe - must be a dataframe. 
# start_time/end_time - must be character strings in 'YYYY-MM-DD hh:mm:ss' format.
plot_dive <- function(dataframe, start_time, end_time) {
  one_dive <- filter(dataframe, DateTime >= start_time, DateTime <= end_time)
  dive_plot <- ggplot(one_dive, aes(x = DateTime, y = Depth)) +
    geom_line() +
    scale_y_reverse() + #makes depth right orientation 
    labs(x = "Time",
         y = "Depth (m)") + #changes labels on axes and title 
    theme_bw()  #white background
  dive_plot
}

plot_anorm <- function(dataframe, start_time, end_time)
filter(dataframe, DateTime >= start_time, DateTime <= end_time) %>% 
  ggplot(aes(DateTime, anorm)) +
  geom_line() +
  theme_bw()