# R programming project (EDA AND VISUALIZATION)

#Installing Package

install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("cowplot")


##Loading Packages
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(corrplot)   # used for making correlation plot
library(cowplot)   # used for combining multiple plots
library(data.table) # used for reading and manipulation of data


setwd("C:\\Users\\tejas\\OneDrive\\Desktop\\rproject")
data <- read.csv("train.csv")  # readimg our file
View(data)


#Dimensions of Data

dim(data)


#Features of Data

names(data)


#Structure of Data

str(data)


#EXLORATOERY DATA ANALYSIS

ggplot(data) + geom_histogram(aes(data$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")

#Independent Variables(numeric variables)

p1 = ggplot(data) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(data) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(data) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

## Item_Weight vs Item_Outlet_Sales
p9 = ggplot(data) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
p9

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(data) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
p10
# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(data) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)
p11

#Target Variable vs Independent Categorical Variables

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(data) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p12
# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(data) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p13

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(data) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)
p14

#remaining variables

p15 = ggplot(data) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
p16 = ggplot(data) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
plot_grid(p15, p16, ncol = 1)

#find missing values in a variable.

sum(is.na(data$Item_Weight))

#Imputing Missing Value

missing_index = which(is.na(data$Item_Weight))
for(i in missing_index){
  
  item = data$Item_Identifier[i]
  data$Item_Weight[i] = mean(data$Item_Weight[data$Item_Identifier == item], na.rm = T)
}
View(data)


#Replacing 0's in Item_Visibility variable

ggplot(data) + geom_histogram(aes(Item_Visibility), bins = 100)

#replace the zeroes

zero_index = which(data$Item_Visibility == 0)
for(i in zero_index){
  
  item = data$Item_Identifier[i]
  data$Item_Visibility[i] = mean(data$Item_Visibility[data$Item_Identifier == item], na.rm = T)
  
}
View(data)
# checking by graph
ggplot(data) + geom_histogram(aes(Item_Visibility), bins = 100)

#Correlated Variables

# Assuming 'Item_Identifier' is a character column
# Select numeric columns only
numeric_data <- data[, sapply(data, is.numeric)]

# Calculate correlation
cor_data <- cor(numeric_data)

# Print the correlation matrix
print(cor_data)

#Replace LF INTO LOW FAT AND REG INTO REGULAR OM ITEM_FAT_CONTENT

unique(data$Item_Fat_Content)

data$Item_Fat_Content[data$Item_Fat_Content == "LF"] <- "lOW Fat"
View(data)

data$Item_Fat_Content[data$Item_Fat_Content == "lOW Fat"] <- "low Fat"
View(data)

data$Item_Fat_Content[data$Item_Fat_Content == "reg"] <- "Regular"
View(data)


data$Item_Fat_Content[data$Item_Fat_Content == "low fat"] <- "Low Fat"
View(data)


data$Item_Fat_Content[data$Item_Fat_Content == "low Fat"] <- "Low Fat"
View(data)

unique(data$Item_Fat_Content)
