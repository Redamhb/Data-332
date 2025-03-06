#Course
# Load required library
library(ggplot2)

# Create dataset based on your table
Instance_ID <- c("BKKPNGP022", "CMPSCNF009", "CMPSCNP032", "DGTMKTP015", "DGTSCRF002", 
                 "DGTSCRP013", "DTXSCIF004", "DTXSCIP055", "FRLNCXP030")
Title <- c("Bookkeeping", "Computer Science", "Computer Science", "Digital Marketing", 
           "Digital Security", "Digital Security", "Data Science", "Data Science", "Freelancing")
Hours_Per_Week <- c(9, 40, 9, 9, 40, 9, 40, 9, 9)
Cost <- c(4650, 24500, 4650, 4650, 24500, 4650, 24500, 4650, 4650)

# Create a data frame
data <- data.frame(Instance_ID, Title, Hours_Per_Week, Cost)

# Convert Hours_Per_Week to a factor (to distinguish 9-hour and 40-hour programs)
data$Hours_Per_Week <- factor(data$Hours_Per_Week)

# Stacked Bar Chart
ggplot(data, aes(fill = Hours_Per_Week, y = Cost, x = Title)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Course Costs by Title and Hours Per Week",
       x = "Course Title",
       y = "Cost (USD)",
       fill = "Hours Per Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability 


#Registration
# Load required libraries
library(ggplot2)

# Create dataset based on Registration data
Instance_ID <- c("CMPSCNF009", "DTXSCIF004", "CMPSCNP032", "DGTMKTP015", "DGTSCRP013")
Total_Cost <- c(19600, 19600, 4650, 4650, 4650)
Payment_Plan <- c("No", "Yes", "No", "No", "No")  # Convert True/False to Yes/No

# Create a data frame
data <- data.frame(Instance_ID, Total_Cost, Payment_Plan)

# Stacked Bar Chart
ggplot(data, aes(fill = Payment_Plan, y = Total_Cost, x = Instance_ID)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Course Costs by Instance ID and Payment Plan",
       x = "Course",
       y = "Total Cost (USD)",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

#Student
# Load required libraries
library(ggplot2)
library(readxl)

# Correct file path
file_path <- "~/Documents/r_projects/trucking/Student.xlsx"  
student_data <- read_excel(file_path)

# Check column names
colnames(student_data)

# Convert necessary columns to factors
student_data$City <- as.factor(student_data$City)
student_data$Assessment <- as.factor(student_data$Assessment)

# Stacked Bar Chart: Number of students per city, grouped by Assessment status
ggplot(student_data, aes(fill = Assessment, x = City)) + 
  geom_bar(position = "stack") +
  labs(title = "Student Distribution by City and Assessment Status",
       x = "City",
       y = "Number of Students",
       fill = "Assessment Passed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

