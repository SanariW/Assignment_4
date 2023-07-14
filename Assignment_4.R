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
colnames(ufo_subset) # Checked output to confirm that column names do not have any spaces in them. Can proceed to next step. 

# 2. Visually inspect and compare your data frame to the original csv to make sure that all data is loaded as expected.
# DONE
View(ufo)
class(ufo) # Verify that ufo_subset is a data frame. 

# 3. Find the rows where Shape information is missing and impute with "unknown".
ufo <- ufo %>% 
  mutate(shape = replace(shape, shape == "", "unknown"))
View(ufo) # Updated data set. 

# 4. Remove the rows that do not have Country information.
ufo <- ufo %>%
  filter(country != "")
View(ufo) # Updated data set. 

# 5. BONUS: For some of the rows where Country column is blank, the country name is found in the City column in brackets. Where Country info is missing, try to extract the information in brackets in the City column and impute that value in the Country column.
# 6. Convert Datetime and Date_posted columns into appropriate formats

ufo$datetime <- as.POSIXct(ufo$datetime, format = "%Y-%m-%d %H:%M")
ufo$date_posted <- as.POSIXct(ufo$date_posted, format = "%d-%m-%Y")
View(ufo)

# 7. Identifying possible hoax reports. 

filtered_comments <- ufo %>% # Taking a look at the comments that contain "NUFORC" to see what keywords can be pulled. 
  filter(grepl("NUFORC", comments, ignore.case = TRUE)) %>%
  select(comments)
filtered_comments$comments # Display the filtered comments by NUFORC. 

ufo <- ufo %>%
  mutate(is_hoax = grepl("hoax|false|fake|not real", comments, ignore.case = TRUE)). # Use mutate() and grepl to search for comments that include "hoax", "false", "fake", "not real", ignore case and create new Boolean column titled "is_hoax". 
sum(ufo$is_hoax == TRUE) # Count number of comments that contain these words, identifying as possible hoax. 
View(ufo) # Updated dataset 

# 8. Create a table reporting the percentage of hoax sightings per country.
hoax_percentage <- ufo %>%
  group_by(country) %>%
  summarize(percentage_hoax = mean(is_hoax) * 100)
View(hoax_percentage) # View table form in new tab. 
print(hoax_percentage) # View table form in output in console. 

# 9. Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
# 10. Remove the rows where the sighting was reported before it happened.
# 11. Create a table reporting the average report_delay per country.
# 12. Check the data quality (missingness, format, range etc) of the "duration seconds" column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
# 13. Create a histogram using the "duration seconds" column.