## This file is intended to provide example answers to the questions and tasks in Statistics 
## Training Practical R.R, however, these are not the only answers. 


## Task 1
frequency_table_count <- discrete_data %>% 
  group_by(Diagnosis_ICD10_Description) %>% 
  summarise(Count = n()) %>% ## line 57 to here is our frequency table from above, and we are using the pipe operator to add the rename below
  rename( ## using the rename function
  Diagnosis  = Diagnosis_ICD10_Description,  ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in "" or ''
  "Count of Inpatients"  = Count ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in ""
  )
View(frequency_table_count) 

  ## OR

frequency_table_count <- discrete_data %>% 
  group_by(Diagnosis_ICD10_Description) %>% 
  summarise(Count = n()) %>% ## line 57 to here is our frequency table from above, and we are using the pipe operator to add the rename below
  rename( ## using the rename function
    "Diagnosis Description"  = Diagnosis_ICD10_Description,  ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in "" or ''
    "Count of Inpatients"  = Count ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in ""
  )
View(frequency_table_count) 

  ## OR

frequency_table_count <- discrete_data %>% 
  group_by(Diagnosis_ICD10_Description) %>% 
  summarise(Count = n()) %>% ## line 57 to here is our frequency table from above, and we are using the pipe operator to add the rename below
  rename( ## using the rename function
    Diagnosis_Description  = Diagnosis_ICD10_Description,  ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in "" or ''
    "Count of Inpatients"  = Count ## rename the column to something easier to understand. If you are using multiple words, join with _ or encase in ""
  )
View(frequency_table_count) 



## Task 2
frequency_table_count_eth <- discrete_data %>%
  group_by(Diagnosis_ICD10_Description, Ethnicity) %>% 
  summarise(Count = n()) %>%
  rename(
    'Diagnosis Description' = Diagnosis_ICD10_Description, 
    'Count of Inpatients' = Count
  )
View(frequency_table_count_eth)


## Task 3
frequency_table_count_Daignosis_Admittance <- discrete_data %>%
  group_by(Diagnosis_ICD10_Description, Ward_Transferred_To, ED_Location) %>% 
  summarise(Count = n()) %>%
  rename(
    'Diagnosis Description' = Diagnosis_ICD10_Description, 
    'Ward Admitted to (incl. SDEC)' = Ward_Transferred_To, ## I included (incl. SDEC) in the name as SDEC in same day emergency care, but often still treated as inpatient
    'ED Attended' = ED_Location,
    'Count of Inpatients' = Count
  )
View(frequency_table_count_Daignosis_Admittance)


## Question 4:
## We don't want a scale on the inpatient axis, we want just 0 and 1. To achieve this, we need to convert
## the column to a factor
## discrete_data$Admitted_To_Ward <- as.factor(discrete_data$Admitted_To_Ward)
## normally we would do this after importing the data and before manipulating the data, as part of 
## the data cleaning steps. We would want to convert all categorical variables to factors
## However, we did it at this stage just to show you how not converting to factors may affect visuals such as bar charts


## Task 5:
ggplot(discrete_data, aes(x = Ward_Transferred_To)) +
  geom_bar(fill = "magenta", color = "lightgreen") + ## add colour to the bars
  labs(title = "Count of Inpatients by Ward Transferred to
  (incl. SDEC)", x = "Ward", y = "Count of Patients") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30),      ## title text size
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),    ## X-axis title text size
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),    ## Y-axis title text size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold"), ## X-axis text size and bold
    axis.text.y = element_text(size = 25, face = "bold")  ## Y-axis text size and bold
  ) + 
  coord_flip()


## Task 6:
ggplot(discrete_data, aes(x = Admitted_To_Ward, fill = Ward_Transferred_To)) + ## we have added fill = Sex to split the stacked bars by sex
  geom_bar(color = "black", position = "stack") + ## stacked bars (rather than clustered)
  labs(
    title = "Count of Inpatients by Ward Admittance Status 
(1 = admitted to ward, 0 = SDEC admission) and Ward Location", 
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


## Task 7:
frequency_table_prop <- discrete_data %>%
  group_by(ED_Location) %>%
  summarise(Count = n()) %>%
  mutate(`Proportion (%)` = (Count / sum(Count) * 100),
         Label = paste0(round(`Proportion (%)`, 1), "%"))

# Generate the pie chart
ggplot(frequency_table_prop, aes(x = "", y = Count, fill = ED_Location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart Showing % Proportion of Inpatients by ED Attended Location", fill = "ED_Location") +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 30)
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 8, color = "white") 


## Task 8:
q <- hist(continuous_data$Handover_Minutes, ## specify the data source. Assign histogram to random variable q (for customisation below)
          breaks = "Sturges", ## the method to create your bins
          xlab = 'Handover Time (mins)', ## x axis title
          ylab = 'Frequency', ## y axis title
          main = 'Distribution of EMAS Ambulance Handover Times in 2023/24 FY', ## chart title
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 2,    ## chart title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size


q <- hist(continuous_data$Handover_Minutes, ## specify the data source. Assign histogram to random variable q (for customisation below)
          breaks = 30, ## the method to create your bins
          xlab = 'Handover Time (mins)', ## x axis title
          ylab = 'Frequency', ## y axis title
          main = 'Distribution of EMAS Ambulance Handover Times in 2023/24 FY', ## chart title
          col = "skyblue",       ## colour of bars
          border = "black",      ## border colour of bars
)
q <- par(cex.main = 2,    ## chart title size
         cex.lab = 2,   ## axis labels size
         cex.axis = 2)  ## axis data labels size



## Question 9:
## 1: Which day has the single highest handover time value? (ignore the dots for now - these are outliers. We will discuss this further on the next slide)
## 05/12/23

## 2: How would you describe the median/Q2 value across all 5 days?
## fairly constant

## 3: Which day has the highest Q3 value?
## ## 05/12/23

## 4: How would you describe the Q1 value across all 5 days?
## fairly constant

## 5: Which day has the largest spread across the middle 50% of the data?
## ## 05/12/23

## Question 10:
## This histogram looks much more normally distributed now.

## Question 11:
## This histogram has improved slightly but still looks highly right skewed





