# Assignment 4: Martians are coming! 
# By: Sanari Wickramaratne 

# Ensure that dplyr and tidyr package is installed. If not, please run the two lines of code below:
install.packages("dplyr")
install.packages("tidyr")


# Loading the libraries of the required packages. 
library(dplyr)
library(tidyr)


# 1. Read the data into a data frame and make sure that column names do not have spaces in them.
ufo <- read.csv("ufo_subset.csv") # Ensure this csv file is saved onto your working directory before running code.
colnames(ufo) # Checked output to confirm that column names do not have any spaces in them. Can proceed to next step. 


# 2. Visually inspect and compare your data frame to the original csv to make sure that all data is loaded as expected.
# DONE
View(ufo)
class(ufo) # Verify that ufo is a data frame. 


# 3. Find the rows where Shape information is missing and impute with "unknown".
ufo_updated <- ufo %>% 
  mutate(shape = replace(shape, shape == "", "unknown"))
View(ufo_updated) # Updated data set. 


# 4. Remove the rows that do not have Country information.
ufo_updated <- ufo_updated %>%
  filter(country != "")
View(ufo_updated) 


# 5. Convert Datetime and Date_posted columns into appropriate formats
ufo_updated$datetime <- as.Date(strptime(ufo_updated$datetime, format = "%Y-%m-%d %H:%M"))
ufo_updated$date_posted <- as.Date(strptime(ufo_updated$date_posted, format = "%d-%m-%Y"))
View(ufo_updated)


# 6. Identifying possible hoax reports. 
filtered_comments <- ufo %>% # Taking a look at the comments that contain "NUFORC" to see what keywords can be pulled. 
  filter(grepl("NUFORC", comments, ignore.case = TRUE)) %>%
  select(comments)
filtered_comments$comments # Display the filtered comments by NUFORC. 

ufo_updated <- ufo_updated %>%
  mutate(is_hoax = grepl("hoax|false|fake|not real", comments, ignore.case = TRUE)) # Use mutate() and grepl to search for comments that include "hoax", "false", "fake", "not real", ignore case and create new Boolean column titled "is_hoax". 
# It is difficult to include other keywords that could potentially signal a hoax, such as "airplane" or "star" since these words could be used as describing words for the UFO. 
sum(ufo_updated$is_hoax == TRUE) # Count number of comments that contain these words, identifying as possible hoax. 
View(ufo_updated) 


# 7. Create a table reporting the percentage of hoax sightings per country.
hoax_percentage <- ufo_updated %>%
  group_by(country) %>%
  summarize(percentage_hoax = mean(is_hoax) * 100)
View(hoax_percentage) # View table form in new tab. 
print(hoax_percentage) # View table form in output in console (alternate way of viewing table). 


# 8. Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
ufo_updated <- ufo_updated %>%
  mutate(report_delay = as.numeric(difftime(as.Date(date_posted), as.Date(datetime), units = "days")))
View(ufo_updated)


# 9. Remove the rows where the sighting was reported before it happened.
ufo_updated <- ufo_updated %>%
  filter(report_delay >= 0)
View(ufo_updated)


# 10. Create a table reporting the average report_delay per country.
avg_delay <- ufo_updated %>%
  group_by(country) %>%
  summarize(avg_report_delay = mean(report_delay))
View(avg_delay)


# 11. Check the data quality (missingness, format, range etc) of the "duration seconds" column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
duration_empty <- sum(ufo_updated$duration.seconds == "") # Checking for empty spaces in the column. Provides output 0. 
print(duration_empty)

duration_na <- sum(is.na(ufo_updated$duration.seconds)) # Checking for NA's. Provides output of 0. 
print(duration_na)

duration_letters <- grepl("[a-zA-Z]", ufo_updated$duration.seconds) # Double-checking for any letters in the column (ensure it is in the right format). There are none. 
table(duration_letters)

duration_range <- sum(ufo_updated$duration.seconds > 86400) # Check range of values in the column to see if there are any above 86400 seconds. This equates to 24 hours which is an excessive amount of time for sighting duration. 
print(duration_range) # Output says there are 38 values greater than 86400 seconds.                                        
ufo_updated <- subset(ufo_updated, duration.seconds <= 86400) # Delete the values that exceed 86400 seconds. 


# 12. Create a histogram using the "duration seconds" column.
hist(log10(ufo_updated$duration.seconds, main = "Histogram of Duration of Sightings in Seconds - 2010 - 2014", xlab = "Duration in Seconds", ylab = "Frequency"))

