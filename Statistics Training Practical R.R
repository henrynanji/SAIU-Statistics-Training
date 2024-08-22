

### install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("gtsummary")
install.packages("ggplot2")


### load libraries
library(readxl)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(dplyr)





### import data 

discrete_data <- read_xlsx('Data for Statistics Training NNICB AHO Mins.xlsx', sheet = 'Synthetic_Discrete_Inpatient')

continuous_data <- read_xlsx('Data for Statistics Training NNICB AHO Mins.xlsx', sheet = 'Continuous_Ambulance_Handovers')







### FREQUENCY TABLES ###


## count of inpatients per diagnosis
frequency_table_count <- discrete_data %>% ## the first name on the left is the table name. This is what we are saving our frequency table as. The data source is written next. This means we are taking this data, and assigning it to the new variable name. This allows us to manipulate and edit the data without overwirting it. %>% can be understand as "and then", so we are taking our newlwy names data and then (continues on next line)
  group_by(Diagnosis_ICD10_Description) %>% ## this is grouping by the ICD10 diagnosis description column
  summarise(Count = n()) ## this is counting the number of rows per group and listing the values in a new column called Count
View(frequency_table_count) ## this opens up the frequency table in a new window to view



## proportion of inpatients with each diagnosis 
frequency_table_prop <- discrete_data %>%
  group_by(Diagnosis_ICD10_Description) %>%
  summarise(Count = n()) %>%
  mutate('Proportion (%)' = (Count / sum(Count)*100)) ## this line uses mutate to create a new column. We are calling it Proportion (%). It takes the Count column created in the line above and divides it by the sum of Count (the total number of patients) and then multiplies this by 100
View(frequency_table_prop)




## TASK 1: Change the ICD10 column name into something easier to read and understand. 
## Follow the steps below.

frequency_table_count <- discrete_data %>% 
  group_by(Diagnosis_ICD10_Description) %>% 
  summarise(Count = n()) %>% ## line 57 to here is our frequency table from above, and we are using the pipe operator to add the rename below
  rename( ## using the rename function
    = Diagnosis_ICD10_Description,  ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in "" or ''
    = Count ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in ""
  )
View(frequency_table_count) 




## count of patients with each diagnosis, split by sex
frequency_table_count_sex <- discrete_data %>%
  group_by(Diagnosis_ICD10_Description, Sex) %>% ## we are adding a second variable (Sex) to the grouping
  summarise(Count = n()) %>%
  rename(
    'Diagnosis Description' = Diagnosis_ICD10_Description, ## renaming the ICD10 and Count column (line below). No need to rename Sex column as name is appropriate
    'Count of Inpatients' = Count
  )
View(frequency_table_count_sex)


## TASK 2: Group by Ethnicity instead of Sex and view the new table. 
## Make sure you assign a different table name. HINT: The table name is the first name on line 67. Check you have changed the table in view()

frequency_table_count_sex <- discrete_data %>% ## assign a new table name here e.g. frequency_table_count_ethnicity
  group_by(Diagnosis_ICD10_Description, Sex) %>% ## change grouping variable
  summarise(Count = n()) %>%
  rename(
    'Diagnosis Description' = Diagnosis_ICD10_Description, 
    'Count of Inpatients' = Count
  )
View(frequency_table_count_sex) ## use correct table name HINT: use the table name you assigned in line 83




## TASK 3: Create a new frequency table that counts patients per diagnosis, but grouped by 
## at least 2 variables (try using new ones) and rename any columns that aren't easy to understand. 
##To look at the column names again, access the data via Environment. 

## HINT: Assign a table name on line 105 Add new variables to line 106 after the diagnosis description variable. 
## Rename columns by adding a comma to the end of line 110 and renaming your other columns beneath this. 
## Don't forget to call your table name in the brackets in line 112. 
## Look back through the previous code for helpful comments if required. 
## Have a look at the Example Answers sheet if you get stuck, or ask for help, but have a go yourself first if you can!

  <- discrete_data %>% ## add table name
  group_by(Diagnosis_ICD10_Description) %>% 
  summarise(Count = n()) %>%
  rename(
    'Diagnosis Description' = Diagnosis_ICD10_Description, 
    'Count of Inpatients' = Count
  )
View() ## don't forget to call table





### BAR CHARTS ###


## a simple column chart
ggplot(discrete_data, aes(x = ED_Location)) + ## stating the data source and the thing we are counting i.e. number of aptients per ED location
  geom_bar() + ## this adds the bars
  labs(title = "Count of Inpatients that Arrived via ED, by ED Location", x = "ED Location", y = "Count of Patients") + ## this allows us to add and specifiy the labels and title
  theme_minimal() ## minimal theme



## a customised chart, building on the above, with bars flipped horizontally to make a bar chart
ggplot(discrete_data, aes(x = ED_Location)) +
  geom_bar(fill = "skyblue", color = "black") + ## add colour to the bars
  labs(title = "Count of Inpatients that Arrived via ED, by ED Location", x = "ED Location", y = "Count of Patients") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30),      ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),    ## X-axis title text size
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),    ## Y-axis title text size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), ## X-axis text size and bold
    axis.text.y = element_text(size = 25, face = "bold")  ## Y-axis text size and bold
  ) + 
  coord_flip() ## for horizontal bars


## bar chart showing count of inpatients by their ward admittance status
ggplot(discrete_data, aes(x = Admitted_To_Ward)) +
  geom_bar(fill = "skyblue", color = "black") + ## add colour to the bars
  labs(title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission)", x = "Admitted to Ward", y = "Count of Patients") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30),      ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),    ## X-axis title text size
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),    ## Y-axis title text size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), ## X-axis text size and bold
    axis.text.y = element_text(size = 25, face = "bold")  ## Y-axis text size and bold
  ) + 
  coord_flip()




## QUESTION 4: Does the chart output from the above code look right to you? Is there anything that looks unusual about it?
## We will answer this together on the next slide
# You can also find answer in the Answers R file



## bar chart showing count of inpatients by their ward admittance status WITH CORRECT Y AXIS SCALE
discrete_data$Admitted_To_Ward <- as.factor(discrete_data$Admitted_To_Ward) ## convert Admitted_To_Ward to factor

## It's good practice to convert all categorical variables to facotrs before you start your analysis
## we just wanted to leave it until now so you could see the effect of not doing this
## let's go ahead and convert all our other categorical variables to factors too
discrete_data$ED_Location <- as.factor(discrete_data$ED_Location)
discrete_data$Ward_Transferred_To <- as.factor(discrete_data$Ward_Transferred_To)
discrete_data$Diagnosis_ICD10_Code <- as.factor(discrete_data$Diagnosis_ICD10_Code)
discrete_data$Diagnosis_ICD10_Description <- as.factor(discrete_data$Diagnosis_ICD10_Description)
discrete_data$Sex <- as.factor(discrete_data$Sex)
discrete_data$Ethnicity <- as.factor(discrete_data$Ethnicity)
discrete_data$Language <- as.factor(discrete_data$Language)
discrete_data$Post_Code <- as.factor(discrete_data$Post_Code)


ggplot(discrete_data, aes(x = Admitted_To_Ward)) +
  geom_bar(fill = "skyblue", color = "black") + ## add colour to the bars
  labs(title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission)", x = "Inpatient", y = "Count of Patients") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30),      ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),    ## X-axis title text size
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),    ## Y-axis title text size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), ## X-axis text size and bold
    axis.text.y = element_text(size = 25, face = "bold")  ## Y-axis text size and bold
  ) + 
  coord_flip()


## TASK 5: Create a new barchart by changing the variable in the first line from Admitted_To_Ward 
## or ED_Location to another column name. 
## HINT: Don't forget to update the chart and axis titles appropriately  
## EXTRA TASK: Change the colours of the bars. You can try just typing a colour - it will 
## highlight in the colour if valid - or you can google R colour names.

ggplot(discrete_data, aes(x = Admitted_To_Ward)) +
  geom_bar(fill = "skyblue", color = "black") + ## add colour to the bars
  labs(title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission)", x = "Inpatient", y = "Count of Patients") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30),      ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),    ## X-axis title text size
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),    ## Y-axis title text size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), ## X-axis text size and bold
    axis.text.y = element_text(size = 25, face = "bold")  ## Y-axis text size and bold
  ) + 
  coord_flip()





## Admittance stats split by sex
ggplot(discrete_data, aes(x = Admitted_To_Ward, fill = Sex)) + ## we have added fill = Sex to split the stacked bars by sex
  geom_bar(color = "black") + ## stacked bars (rather than clustered)
  labs(
    title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission) and Sex", 
    x = "Admitted to Ward", 
    y = "Count of Patients"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30, margin = margin(t = 20)), 
    axis.title.y = element_text(size = 30, margin = margin(r = 20)), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), 
    axis.text.y = element_text(size = 25, face = "bold") 
  ) + 
  coord_flip()


## TASK 6: Split the stacked bar chart by ethnicity instead of sex
## HINT: don't forget to change the title

ggplot(discrete_data, aes(x = Admitted_To_Ward, fill = Sex)) + ## we have added fill = Sex to split the stacked bars by sex
  geom_bar(color = "black") + ## stacked bars (rather than clustered)
  labs(
    title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission) and Sex", 
    x = "Admitted to Ward", 
    y = "Count of Patients"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30, margin = margin(t = 20)), 
    axis.title.y = element_text(size = 30, margin = margin(r = 20)), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), 
    axis.text.y = element_text(size = 25, face = "bold") 
  ) + 
  coord_flip()






### PIE CHARTS ###


## this is one of our previous proportion frequency tables. Give it a run again. Check line 277 to see the additional column being added
frequency_table_prop <- discrete_data %>%
  group_by(Diagnosis_ICD10_Description) %>%
  summarise(Count = n()) %>%
  mutate('Proportion (%)' = (Count / sum(Count)*100),
         Label = paste0(round(`Proportion (%)`, 1), "%")) ## this time we will add this row to create a data labels column that has the % rounded to 2 dp
View(frequency_table_prop)


## we can then use this data to create a simple pie chart
ggplot(frequency_table_prop, aes(x = "", y = `Proportion (%)`, fill = Diagnosis_ICD10_Description)) + ## we have stated the data source (frequency_table_prop) and columns within this that we want to use. X axis is empty because it's a pie chart so there isn't really an x axis. y will be our segments and fill is like the legend.
  geom_bar(width = 1, stat = "identity") + ## width is the width of the segments and stat = "identity" ensures the actual values from Proportion (%) are used, rather than e.g. counting occurances
  coord_polar(theta = "y") + ## circular element
  theme_void() + ## no background grid etc.
  labs(fill = "Diagnosis Description", title = "Proportion of ED Diagnoses") ## chart labels


## or a more customised version
ggplot(frequency_table_prop, aes(x = "", y = `Proportion (%)`, fill = Diagnosis_ICD10_Description)) + ## data source and columns 
  geom_bar(stat = "identity", width = 1) + ## format the pie chart segments
  coord_polar(theta = "y") + ## make the chart a pie chart - adds the circular element 
  labs(title = "Pie Chart Showing % Proportion of Inpatient Diagnoses", fill = "Diagnosis Description") + ## chart and legend title 
  theme_void() +
  theme(
    legend.title = element_blank(), ## no legend title
    legend.text = element_text(size = 30), ## legend text size
    plot.title = element_text(size = 30) ## chart title tezxt size
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) ## data labels



## TASK 7: Use the Ethnicity proportion table (provided below) to make a pie chart showing the 
## proportion of Inpatients by Ethnicity. 
## HINT: Change the y and Fill variables and the title. Ask for help/look at model answers if you get stuck

frequency_table_prop <- discrete_data %>%
  group_by(Ethnicity) %>%
  summarise(Count = n()) %>%
  mutate(`Proportion (%)` = (Count / sum(Count) * 100),
         Label = paste0(round(`Proportion (%)`, 1), "%"))

ggplot(frequency_table_prop, aes(x = "", y = `Proportion (%)`, fill = Diagnosis_ICD10_Description)) + ## data source and columns 
  geom_bar(stat = "identity", width = 1) + ## format the pie chart segments
  coord_polar(theta = "y") + ## make the chart a pie chart - adds the circular element 
  labs(title = "Pie Chart Showing % Proportion of Inpatient Diagnoses", fill = "Diagnosis Description") + ## chart and legend title 
  theme_void() +
  theme(
    legend.title = element_blank(), ## no legend title
    legend.text = element_text(size = 30), ## legend text size
    plot.title = element_text(size = 30) ## chart title tezxt size
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) ## data labels







### HISTOGRAMS ###

## QUESTION: The ambulance handover times are measured in seconds. Is this data discrete or continuous?
##           Would the following data be discrete or continuous:
##                                                      Ambulance handover times in minutes = 
##                                                      Length of stay in days:hours:minutes:seconds = 
##                                                      Length of stay in days =                              
##                                                      Birthweight in pounds = 
## We will go through the answers on the next slide and you can also refer back to the Answers R script or Notes Wor ddoc at a later date




## create histogram showing frequency of ambulance handover per minutes range bin
q <- hist(continuous_data$Handover_Minutes, ## specify the data source. Assign histogram to random variable q (for customisation below)
          breaks = "Scott", ## the method to create your bins
          xlab = 'Handover Time (mins)', ## x axis title
          ylab = 'Frequency', ## y axis title
          main = 'Distribution of EMAS Ambulance Handover Times in 2023/24 FY', ## chart title
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
          freq = TRUE
)
q <- par(cex.main = 2,    ## chart title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size



## TASK 8: Change the binning method from Scott to Sturges, and then breaks = n to select your own number of bins. Do they show different patterns? 


## Sturges
## HINT: overwrite the name enclosed in "" in the breaks line

q <- hist(continuous_data$Handover_Minutes, ## specify the data source. Assign histogram to random variable q (for customisation below)
          breaks = "Scott", ## the method to create your bins
          xlab = 'Handover Time (mins)', ## x axis title
          ylab = 'Frequency', ## y axis title
          main = 'Distribution of EMAS Ambulance Handover Times in 2023/24 FY', ## chart title
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 2,    ## chart title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size


## n - custom number of bins
## HINT: To select your custom number of bins, do not use "" simply replace n with the number of bins you want

q <- hist(continuous_data$Handover_Minutes, ## specify the data source. Assign histogram to random variable q (for customisation below)
          breaks = n, ## the method to create your bins
          xlab = 'Handover Time (mins)', ## x axis title
          ylab = 'Frequency', ## y axis title
          main = 'Distribution of EMAS Ambulance Handover Times in 2023/24 FY', ## chart title
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 2,    ## chart title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size




### BOX PLOT ###

## Prepare data
continuous_data <- continuous_data ## assign data to a new data frame so we can leave the orignal unedited

continuous_data$Date <- as.Date(continuous_data$Date, format="%Y-%m-%d") ## convert Date column to proper date format

continuous_data <- continuous_data %>%
  filter(Date >= as.Date("2023-12-01") & Date <= as.Date("2023-12-05")) ## filter to smaller data range so we can compare boxplots of EMAS handover times across different days

## create box plot of EMAS handover times across each day from 01/12/23 - 05/12/23
ggplot(continuous_data, aes(x = as.factor(Date), y = Handover_Minutes)) + ## specify data source and data for x and y axes. Ensure Date is treated as a factor to get one boxplot per date
  geom_boxplot(fill = "slateblue", alpha = 0.2) + ## size and colour 
  xlab("Day") + ## x axis label
  ylab("Handover Minutes") + ## y axis label
  ggtitle("Boxplot of EMAS Handover Time by Date") + ## plot title
  theme(
    plot.title = element_text(size = 30), ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)), ## x axis title text size and margins
    axis.title.y = element_text(size = 30, margin = margin(r = 20)), ## y axis title text size and margins
    axis.text.x = element_text(angle = 45, hjust = 1, size = 17, face = "bold"), ## x axis data points tilted and size and style
    axis.text.y = element_text(size = 25, face = "bold") ## y axis size and style
  )

## QUESTION 9: 

## 1: Which day has the single highest handover time value? (ignore the dots for now - these are outliers. We will discuss this further on the next slide)

## 2: How would you describe the median/Q2 value across all 5 days?

## 3: Which day has the highest Q3 value?

## 4: How would you describe the Q1 value across all 5 days?

## 5: Which day has the largest spread across the middle 50% of the data?

##  We will go through answers on the slide and you can later check R Answer file/notes for answer




### SUMMARY MEASURES ###

summary(continuous_data$Handover_Minutes)




### HISTOGRAM - LOG TRANSOFRMATION ###

## process the data
log_data <- continuous_data ## create a duplicate data frame called log_data so we can use this and leave our original dataframe unedited

log_data$Handover_Minutes <- log(continuous_data$Handover_Minutes, base = 10) ## instead of our normal handover seconds, take the log. I am using base 2, you can change this depending on your requirements

## create the transformed histogram
q <- hist(log_data$Handover_Minutes,  ## create the histogram as above but use our log data
          breaks = "Scott", 
          xlab = 'Handover Time (mins)', 
          ylab = 'Frequency', 
          main = 'Log Transformed Distribution of EMAS Ambulance Handover Times in 2023/24 FY',
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 2,  ## title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size


## QUESTION 10: Is this histogram normally distributed or skewed?
## We will go through answers on the slide and you can later check R Answer file/notes for answer




### HISTOGRAM - SQUARE ROOT TRANSOFRMATION ###

## prepare data 
sqrt_data <- continuous_data ## create a duplicate data frame called SQRT_data so we can use this and leave our original dataframe unedited

sqrt_data$Handover_Minutes <- sqrt(continuous_data$Handover_Minutes)

## create transformed histogram
q <- hist(sqrt_data$Handover_Minutes, ## create the histogram as above but use our square root data
          breaks = "Scott", 
          xlab = 'Handover Time (mins)', 
          ylab = 'Frequency', 
          main = 'Square Root Transformed Distribution of EMAS Ambulance 
Handover Times in 2023/24 FY',
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 3,  ## title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size



## QUESTION 11: Is this histogram normally distributed or skewed?
## We will go through answers on the slide and you can later check R Answer file/notes for answer





















