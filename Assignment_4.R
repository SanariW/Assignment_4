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
class(ufo) # Verify that ufo_subset is a data frame. 

# 3. Find the rows where Shape information is missing and impute with "unknown".
ufo_updated <- ufo %>% 
  mutate(shape = replace(shape, shape == "", "unknown"))
View(ufo_updated) # Updated data set. 

# 4. Remove the rows that do not have Country information.
ufo_updated <- ufo %>%
  filter(country != "")
View(ufo_updated) # Updated data set. 

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
sum(ufo_updated$is_hoax == TRUE) # Count number of comments that contain these words, identifying as possible hoax. 
View(ufo_updated) # Updated dataset. 

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
# 12. Create a histogram using the "duration seconds" column.