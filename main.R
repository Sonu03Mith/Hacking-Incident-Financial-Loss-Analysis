# Suhana Oravuthodi4 - TP078849 


#Libraries
library("readr") 
library("dplyr")  
library("tidyverse") 
library("naniar") 
library("ggplot2") 
library("psych") 
library("broom")
library("corrplot")
library("plotly")
library("stringr")
library("tidyr")
library("lubridate")
library("DescTools")
library("stringi")
library("countrycode")


# ------------------------------------------------------------------------------------------------DATA IMPORT
hackingdata = read.csv("/Users/suhanashabhik/Downloads/hackingData.csv")

#--------------------------------------------------------------------------------------------REMOVE DUPLICATES
nrow(hackingdata) #[212093]
hackingdata[duplicated(hackingdata) | duplicated(hackingdata, fromLast = TRUE), ]
hackingdata <- distinct(hackingdata)
nrow(hackingdata) #[211913]
hackingdata[duplicated(hackingdata) | duplicated(hackingdata, fromLast = TRUE), ] #Double Check

# --------------------------------------------------------------------------------------HANDLING MISSING VALUES

#Viewing Missing Value
hackingdata %>%
  summarise(across(everything(), ~ sum(is.na(.))))

mcar_test(hackingdata) # p-value < 0.5 thus must impute

#1. Cleaning Ransom --- (Abdallah & Salsabila)

#Ransom [159,820 Missing Values] 

summary(lm(Ransom ~ DownTime, data = hackingdata))
summary(lm(Ransom ~ Loss, data = hackingdata))

sqrt(2.31e-05)

#Checking the Distribution and Outliers
hist(hackingdata$Ransom,
     main = "Histogram: Ransom",
     xlab = "Ransom('000)",
     ylab = "Frequency",
     col = "lightgreen")

boxplot(hackingdata$Ransom,
        main = "Boxplot: Ransom",
        ylab = "Ransom('000)",
        col = "lightgreen")

hackingdata <- hackingdata %>%
  mutate(Ransom = ifelse(is.na(Ransom), mean(Ransom, na.rm = TRUE), Ransom))

#2. Cleaning Loss  --- (Abdallah)

#Loss [33,594 Missing Values]
#Checking the Distribution and Outliers
hist(hackingdata$Loss,
     main = "Histogram: Loss",
     xlab = "Loss('000)",
     ylab = "Frequency",
     col = "lightgreen")

boxplot(hackingdata$Loss,
        main = "Boxplot: Loss",
        ylab = "Loss('000)",
        col = "lightgreen")

hackingdata <- hackingdata %>%
  mutate(Loss = ifelse(is.na(Loss), median(Loss, na.rm = TRUE), Loss))

#3. Cleaning Encoding --- (Salsabila)

#Encoding [7 Missing Values]
#Showing the count of categories
encoding_tbl <- table(hackingdata$Encoding)

#I replaced every instance of NULL/N with “Unknown” for better readability
hackingdata$Encoding[hackingdata$Encoding %in% c("NULL", "N")] <- "Unknown"
hackingdata$Encoding <- factor(hackingdata$Encoding)

# when counting mode exclude "n" and "na" or in this case "Unknown"values
valid_values <- hackingdata$Encoding[!(hackingdata$Encoding %in% c("Unknown"))]
mode_value <- names(sort(table(valid_values), decreasing = TRUE))[1]  # Get the most frequent category
hackingdata$Encoding[is.na(hackingdata$Encoding)] <- mode_value # is utf-8

#4. Cleaning Date --- (Noor)
# Check if "Date" is already in Date format
print(str(hackingdata$Date))  # This helps us see its structure

# Detect and convert only valid dates
hackingdata$Year <- as.numeric(format(as.Date(hackingdata$Date, format = "%d/%m/%Y"), "%Y"))

# Print unique years to check correctness
print(unique(hackingdata$Year))


#5. Cleaning Country --- (Suhana)

#Country [56716 Missing Values]
##### Cleaning the Country Column #####

# Check for invalid country entries (empty strings, NULL values, or missing values)
sum(hackingdata$Country == "" | is.na(hackingdata$Country) | hackingdata$Country == "null")

# Convert all country names to lowercase for standardization
hackingdata$Country <- tolower(hackingdata$Country)

# Replace missing or invalid country names with "Unknown"
hackingdata <- hackingdata %>%
  mutate(Country = ifelse(
    is.na(Country) | Country %in% c("null", "unknown", ""),  # Condition to check invalid values
    "Unknown",  # Replace them with "Unknown"
    Country  # Keep the existing country name if it's valid
  ))

# Standardizing known variations of country names
hackingdata <- hackingdata %>%
  mutate(Country = case_when(
    Country %in% c("united states", "united state", "americansamoa", "virginislands(u.s.)", "virgin islan","america") ~ "united states of america",
    Country %in% c("united kingdom", "united kingd", "virginislands(british)", "virgin islands") ~ "united kingdom of great britain",
    Country %in% c("russian federation", "russian fede", "russia") ~ "russian federation",
    Country %in% c("korea", "korea, repub") ~ "south korea",
    Country %in% c("new zealand", "newzealand", "new zealand\"", "cook islands") ~ "new zealand",
    Country %in% c("brunei darussalam", "bruneidarussalam", "brunei") ~ "brunei",
    Country %in% c("new caledonia", "newcaledonia", "caledonia", "new caledoni", "french polyn", "french polynesia") ~ "france",
    Country %in% c("syrian arab", "syrian arab republic") ~ "syria",
    Country %in% c("libyanarabjamahiriya", "libyan arab jamahiriya") ~ "libya",
    Country %in% c("palestinian territory", "palestine", "israel") ~ "palestine",
    TRUE ~ Country  # Keep unchanged if it doesn’t match any condition
  ))

# List of continent and region labels that should be removed (since they are not actual countries)
continents_and_regions <- c(
  "europe", "asia", "africa","middleeast", "micronesia",
  "asia/pacific region", "european uni", "european union", 
  "southamerica", "oseania", "westeuro", "easteuro", 
  "anonymous proxy", "satellite provider"
)

# Function to clean and standardize country names
clean_country_names <- function(country) {
  # Convert country names to title case (first letter uppercase)
  country <- str_to_title(country)  
  
  # Dictionary of incorrect country names and their correct versions
  replacements <- c(
    "Czech Republ" = "Czech Republic",
    "Iran, Islami" = "Iran",
    "Saudiarabia" = "Saudi Arabia",
    "Taiwan, Prov" = "Taiwan",
    "Moldova, Rep" = "Moldova",
    "Elsalvador" = "El Salvador",
    "Puertorico" = "Puerto Rico",
    "Costarica" = "Costa Rica",
    "Cayman Islan" = "Cayman Islands",
    "Yugoslavia" = "Serbia",
    "Macedonia, T" = "Macedonia",
    "Capeverde" = "Cape Verde",
    "Viet Nam" = "Vietnam",
    "United Arabemirates" = "United Arab Emirates",
    "Burkinafaso" = "Burkina Faso",
    "Nigeria\"" = "Nigeria",
    "Netherlands\"" = "Netherlands",
    "Vaticancitystate" = "Vatican City",
    "Srilanka" = "Sri Lanka",
    "Netherlandsantilles" = "Netherlands Antilles",
    "Saotomeandprincipe" = "São Tomé and Príncipe",
    "Trinidad And Tobago" = "Trinidad and Tobago",
    "Cotedivoire" = "Côte d'Ivoire",
    "Turksandcaicosislands" = "Turks and Caicos Islands",
    "Ascensionisland" = "Ascension Island",
    "Saintkittsnevis" = "Saint Kitts and Nevis",
    "Norfolkisland" = "Norfolk Island",
    "Lao People's Democratic Republic" = "Laos",
    "easttimor" = "Timor-Leste"
  )
  
  # Replace incorrect country names using the defined dictionary
  country <- ifelse(country %in% names(replacements), replacements[country], country)
  
  # Check if the country name belongs to a continent or region and mark it as "Unknown"
  if (tolower(country) %in% tolower(continents_and_regions)) {
    return("Unknown")
  }
  
  return(country)  # Return the cleaned country name
}

# Apply the country name cleaning function to every row in the 'Country' column
hackingdata <- hackingdata %>%
  mutate(Country = sapply(Country, clean_country_names))

# Trim any extra spaces that may still be present in the country names
hackingdata$Country <- str_trim(hackingdata$Country)

# Display the unique country names after cleaning
unique(hackingdata$Country)

# Count occurrences of each country in the dataset
country_counts <- table(hackingdata$Country)

# -------------------------------------------------------------------------------------------------------- 
#EDA
#------------------------------------
#TP078712 

#Objective 1:
#  To examine if the financial loss resulting from a defaced website is influenced by the amount of ransom paid and the downtime experienced.
#Analysis Questions:
#  Analysis 1-1: Does increased ransom contribute to a higher financial loss?
#  Analysis 1-2: Does increased downtime contribute to a higher financial loss?
#  Analysis 1-3: Can the combination of ransom amount and downtime effectively predict financial loss?

#Analysis 1-1: Does increased ransom contribute to a higher financial loss?
hackingdata <- hackingdata %>%
  mutate(ransom_category = cut(
    Ransom,
    breaks = c(-Inf, 1546, 2300, Inf),
    labels = c("Low","Medium","High")
  ))

#Boxplot
ggplot(hackingdata, aes(x = ransom_category, y = Loss, fill = ransom_category)) +
  geom_boxplot(color = "black") +  # Keeps box outlines black
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 0)), 
               color = "red", vjust = -0.5, size = 3) +
  labs(title = "Loss by Ransom Category",
       x = "Ransom Category",
       y = "Loss") +
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "lightgreen", "High" = "lightcoral")) + 
  theme_minimal() +
  theme(legend.position = "none")  # Removes legend since colors already correspond to x-axis categories

#Bar plot of boxplot
ggplot(hackingdata, aes(x = ransom_category, y = Loss, fill = ransom_category)) +
  stat_summary(fun = mean, geom = "bar", color = "black", position = "dodge") +  
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 0)), 
               color = "red", vjust = -0.5, size = 3) +
  labs(title = "Average Loss by Ransom Category",
       x = "Ransom Category",
       y = "Average Loss") +
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "lightgreen", "High" = "lightcoral")) + 
  theme_minimal() +
  theme(legend.position = "none")

#Mean of Each Category
hackingdata %>%
  group_by(ransom_category) %>%
  summarize(mean_ransom = mean(Loss, na.rm = TRUE))

#Analysis 1-2: Does increased downtime contribute to a higher financial loss?
#Categorizing 
hackingdata <- hackingdata %>%
  mutate(downtime_category = cut(
    DownTime,
    breaks = c(-Inf, 10, 20, 30, 40, 50, Inf),
    labels = c("A", "B", "C", "D", "E", "F")
  )) %>%
  group_by(downtime_category) %>%
  mutate(total_loss = sum(Loss, na.rm = TRUE)) %>%
  ungroup()

#Bar Plot
downtime_labels <- c(
  "A" = "0–10 days",
  "B" = "11–20 days",
  "C" = "21–30 days",
  "D" = "31–40 days",
  "E" = "41–50 days",
  "F" = "51–60 days"
)

ggplot(hackingdata, aes(x = downtime_category, y = total_loss, fill = downtime_category)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = downtime_labels) +  # Update x-axis labels
  scale_fill_manual(values = c("#F0F8FF", "#ADD8E6", "#87CEEB", "#4682B4", "#1E90FF", "#00008B"), 
                    labels = downtime_labels, name = "DownTime Category") +  # Update legend
  labs(
    title = "Total Loss by DownTime Category",
    x = "DownTime Category",
    y = "Total Loss"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

ggplot(hackingdata, aes(x = DownTime, y = Loss)) +
  geom_col(fill = "lightgreen") +  # Use bars to represent loss per downtime
  labs(title = "Loss by Downtime",
       x = "Downtime (Days)",
       y = "Loss") +
  theme_minimal()

#Analysis 1-3: Can the combination of ransom amount and downtime effectively predict financial loss?
#Heatmap
ggplot(hackingdata, aes(x = Ransom, y = DownTime)) +
  stat_summary_2d(aes(z = Loss), bins = 40) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "Heatmap of Ransom Amount, Downtime, and Financial Loss",
       x = "Ransom Amount ('000)",
       y = "Downtime (Days)",
       fill = "Avg Loss") +
  theme_minimal()

#-------------------------------
# TP078641 
# Objective 2:
#   To analyze if the amount of ransom paid, type of encoding methods influence the amount of financial loss incurred.

# Analysis Questions:
#  Analysis 2.1: How does the distribution of financial loss compare between ransom amounts that are categorized into low, medium, and high? 
#  Analysis 2.2: Is there an association between specific encoding methods that influence the amount of financial loss.
#  Analysis 2.3: Does ransom amount and encoding method collectively influence the prediction of financial loss?

#  Analysis 2.1: How does the distribution of financial loss compare between ransom amounts that are categorized into low, medium, and high? 
# low (-inf - 1456), med(1456-2300), high(2300- inf)
hackingdata$ransom_category <- cut(dfdata_cleaned$Ransom, 
                                   breaks = c(-Inf, 1456, 2300, Inf),
                                   labels = c("Low", "Medium", "High"),
                                   right = FALSE)

ggplot(hackingdata, aes(x = Loss, fill = ransom_category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Financial Loss for Different Ransom Categories",
       x = "Financial Loss",
       y = "Density") +
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "lightpink", "High" = "lightgreen")) +
  theme_minimal()

#  Analysis 2.2: Is there an association between specific encoding methods that influence the amount of financial loss.
#2.2.1. Bar plot for every single category
ggplot(hackingdata, aes(x = Encoding, y = Loss)) +
  stat_summary(fun = "mean", geom = "bar", fill = "pink", color = "black") +
  labs(title = "Average Loss by Encoding Method", 
       x = "Encoding Method", y = "Average Revenue Loss ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2.2.2. Bar plot for a broader category
hackingdata$Encoding_Grouped <- case_when(
  str_detect(hackingdata$Encoding, "windows") ~ "Windows",
  str_detect(hackingdata$Encoding, "utf") ~ "UTF",
  str_detect(hackingdata$Encoding, "iso|ISO") ~ "ISO",
  str_detect(hackingdata$Encoding, "ascii") ~ "ASCII",
  str_detect(hackingdata$Encoding, "big5|Big5") ~ "Big5",
  str_detect(hackingdata$Encoding, "EUC") ~ "EUC",
  str_detect(hackingdata$Encoding, "gb2312|GB2312") ~ "GB",
  str_detect(hackingdata$Encoding, "KOI") ~ "KOI",
  str_detect(hackingdata$Encoding, "tis|TIS") ~ "TIS",
  str_detect(hackingdata$Encoding, "shift_jis") ~ "Shift-JIS",
  str_detect(hackingdata$Encoding, "LiteSpeed") ~ "LiteSpeed",
  str_detect(hackingdata$Encoding, "Unknown") ~ "Unknown",
  TRUE ~ "Other"
)

ggplot(hackingdata, aes(x = Encoding_Grouped, y = Loss)) +
  stat_summary(fun = "mean", geom = "bar", fill = "pink", color = "black") +
  labs(title = "Average Loss by Encoding Method (Grouped)", 
       x = "Encoding Method (Grouped)", y = "Average Revenue Loss ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Analysis 2.3: Does ransom amount and encoding method collectively influence the prediction of financial loss?
# Heat map of ransom, encoding grouped with the financial loss as coloRS
ggplot(hackingdata, aes(x = Ransom, y = Encoding_Grouped)) +
  stat_summary_2d(aes(z = Loss), bins = 50) +  
  scale_fill_viridis_c(option = "plasma", direction = -1) +  
  labs(title = "Financial Loss Across Ransom Amount & Encoding Type",
       x = "Ransom Amount ($)",
       y = "Character Encoding Type",
       fill = "Avg Loss ($)") + 
  theme_minimal(base_size = 14) +  
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  
    plot.title = element_text(face = "bold", size = 18),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()) +
  scale_y_discrete(limits = rev(levels(hackingdata$Encoding_Grouped)), drop = TRUE) +
  coord_cartesian(expand = FALSE)

#-------------------------------
# TP078599
# Objective 3:
# To determine whether the year of attack significantly predicts the amount of ransom paid.

# Analysis Questions:
#  Analysis 3.1: Is there a significant relationship between the year of attack and ransom amount?
#  Analysis 3.2: How has the average ransom paid changed over the years?
#  Analysis 3.3: Are there trends in ransom demands over time?

#  Analysis 3.1: Is there a significant relationship between the year of attack and ransom amount?
#Step 1: Check the distribution of ransom paid
ggplot(hackingdata, aes(x = Loss)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Ransom Paid", x = "Ransom Amount", y = "Frequency") +
  theme_minimal()

#Step 2: Check correlation between Year and Loss
if (!any(is.na(hackingdata$Year)) && !any(is.na(hackingdata$Loss))) {
  correlation <- cor(hackingdata$Year, hackingdata$Loss, use = "complete.obs", method = "pearson")
  print(paste("Correlation between Year and Ransom Paid:", correlation))
} else {
  print("Warning: Missing values detected in Year or Loss. Correlation may be inaccurate.")
}

#Step 3: Scatter plot to see the trend
ggplot(hackingdata, aes(x = Year, y = Loss)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Trend of Ransom Paid Over Years", x = "Year of Attack", y = "Ransom Paid") +
  theme_minimal()

#  Analysis 3.2: How has the average ransom paid changed over the years?
#Step 1: Calculate the average ransom paid per year
avg_ransom_per_year <- hackingdata %>%
  group_by(Year) %>%
  summarise(Average_Ransom = mean(Loss, na.rm = TRUE)) %>%
  arrange(Year)  # Ensure proper chronological order

# Print the summary
print(avg_ransom_per_year)

#Step 2: Line plot for ransom trend over years
ggplot(avg_ransom_per_year, aes(x = Year, y = Average_Ransom)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Trend of Average Ransom Paid Over Years", x = "Year", y = "Average Ransom Paid") +
  theme_minimal()

#  Analysis 3.3: Are there trends in ransom demands over time?
#Step 1: Count number of ransom cases per year
ransom_cases_per_year <- hackingdata %>%
  group_by(Year) %>%
  summarise(Num_Cases = n()) %>%
  arrange(Year)  # Ensure proper chronological order

# Print the summary
print(ransom_cases_per_year)


#Step 2: Bar chart for number of ransom cases per year
ggplot(ransom_cases_per_year, aes(x = factor(Year), y = Num_Cases, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "plasma") +  # Fixed color scale for more years
  labs(title = "Number of Ransom Cases Per Year", x = "Year", y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#-------------------------------
# TP078849
# Objective 4:
#   The objective is to identify the countries that have suffered the most financial losses as a result of hacking, analyse how they are distributed across continents, and determine each country's percentage contribution to the overall global loss.

# Analysis Questions:
#  Analysis 4.1: How do total financial losses differ between continents?
#  Analysis 4.2: Which countries have the greatest average financial damage per hacking incident?
#  Analysis 4.3: How does each country's financial loss relate with the overall global economic impact?

#  Analysis 4.1: How do total financial losses differ between continents?
# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(countrycode)  # For country-to-continent mapping

# Add Continent column
hackingdata <- hackingdata %>%
  mutate(Continent = case_when(
    Country == "Unknown" ~ NA_character_,  # Treat "Unknown" as missing data
    TRUE ~ countrycode(Country, "country.name", "continent", warn = FALSE)  # Convert all other countries, suppress warnings
    
  ))

#Check if Continent column was successfully added
print(unique(hackingdata$Continent))  # Debugging step

#Group by continent and calculate total financial loss
continent_loss <- hackingdata %>%
  filter(!is.na(Continent)) %>%  # Remove unknown continents
  group_by(Continent) %>%
  summarise(Total_Loss = sum(Loss, na.rm = TRUE))

# Plot the total financial loss by continent
ggplot(continent_loss, aes(x = reorder(Continent, -Total_Loss), y = Total_Loss, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # Format Y-axis labels with commas
  labs(title = "Total Financial Loss by Continent",
       x = "Continent", y = "Total Loss") +
  theme_minimal()

#  Analysis 4.2: Which countries have the greatest average financial damage per hacking incident?
#Load the necessary packages
library(dplyr)

country_avg_loss <- hackingdata %>%
  group_by(Country) %>%
  summarise(Average_Loss = mean(Loss, na.rm = TRUE)) %>%
  arrange(desc(Average_Loss)) %>%
  filter(Country != "Unknown")

ggplot(country_avg_loss[1:10, ], aes(x = reorder(Country, -Average_Loss), y = Average_Loss, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Countries by Average Loss Per Hacking Incident",
       x = "Country", y = "Average Loss") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Analysis 4.3: How does each country's financial loss relate with the overall global economic impact?
library(dplyr)
library(ggplot2)
library(scales)

# Group data by Country and calculate Total Loss
country_loss_pie <- hackingdata %>%
  group_by(Country) %>%
  summarise(Total_Loss = sum(Loss, na.rm = TRUE)) %>%
  arrange(desc(Total_Loss)) %>%
  filter(Country != "Unknown") %>%
  slice_head(n = 10)  # Select top 10 countries

# Create a Pie Chart
ggplot(country_loss_pie, aes(x = "", y = Total_Loss, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Convert to pie chart
  scale_fill_brewer(palette = "Paired") +  # Use a color palette
  labs(title = "Top 10 Countries by Total Financial Loss from Hacking") +
  theme_void()


#-------------------------------------------------------------------------------------------------------
#1rst round -- Error: cannot allocate vector of size 35642.2 Gb
glm_model <- glm(Loss ~ Ransom*DownTime*Encoding*Date*Country, data = hackingdata)
#2nd round - grouped data
glm_model <- glm(Loss ~ ransom_category*DownTime*Encoding_Grouped*Year*Country, data = hackingdata)


# Convert categorical variables to factors
hackingdata$Country <- as.factor(hackingdata$Country)
hackingdata$Encoding_Grouped <- as.factor(hackingdata$Encoding_Grouped)
hackingdata$ransom_category <- as.factor(hackingdata$ransom_category)
hackingdata$Year <- as.factor(hackingdata$Year)

# Run GLM with reduced interactions (avoid too many terms)
glm_model <- glm(Loss ~ ransom_category + DownTime + Encoding_Grouped + Year + Country, 
                 data = hackingdata, family = gaussian())

# Display summary of the model
summary(glm_model)

contrasts(hackingdata$Country) <- contr.sum(length(unique(hackingdata$Country)))
contrasts(hackingdata$Encoding_Grouped) <- contr.sum(length(unique(hackingdata$Encoding_Grouped)))

# Run GLM
glm_model <- glm(Loss ~ ransom_category + DownTime + Encoding_Grouped + Year + Country, 
                 data = hackingdata, family = gaussian())

summary(glm_model)

summary(glm(Loss ~ Ransom*Encoding_Grouped*Year*Country, data = hackingdata))

# Standardize Loss
hackingdata$Loss <- scale(hackingdata$Loss)

# Run ANOVA with reduced interactions
anova_model <- aov(Loss ~ (ransom_category * DownTime) + (Encoding_Grouped * Year) + Country, 
                   data = hackingdata)

summary(anova_model)

step(glm(Loss ~ Ransom*DownTime*Encoding*Date*Country, data = hackingdata), direction = "both")
