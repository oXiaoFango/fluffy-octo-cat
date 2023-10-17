---
title: "Quantium Job Simulation"
author: "Xiao Fang"
date: "2023-10-13"
output: 
  html_document:
    number_sections: false
    toc: true
---

# Quantium Virtual Internship - Retail Strategy and Analytics - Task 1

<img src="https://i0.wp.com/post.healthline.com/wp-content/uploads/2022/04/potato-chips-basket-1296-728-header.jpg?w=1155&amp;h=1528" width="70%" height="70%" align="center"/>

## 1. Define the Problem / Objective:

#### Objective:

The objective of this analysis is to provide actionable insights to the Category Manager for Chips in order to better understand the types of customer who purchase Chips and their purchasing behavior within the region. These insights will be used to inform the strategic plan for the chip category in the next half year.

## 2. Data Collection

The data is given in xls files.

1.  QVI Transaction files This dataset contains transactions spanning from Jul 2018 to Jun 2019. It includes information such as transaction date, total sales, product quantity, product name and loyal customer ID.

2.  QVI Purchase Behaviour This dataset provides details about loyal customers. it includes attributes like LIFESTAGE and PREMIUM CUSTOMER.

    LIFESTAGE - This attribute categorize customer based on family status and life stage. It indicates factor whether they have children in pre-school, primary, or secondary school.

    PREMIUM CUSTOMER - This segmentation distinguishes shoppers by their preferences for products based on price point and type. It helps identify if customers lean towards quality or brand, or if they opt for mode budget friendly options.

```{r - Loading required libraries, include=FALSE}
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
```

```{r - Loading excel file}
customerData <- read.csv(file = "/kaggle/input/quantum-job-simulation-dataset/QVI_purchase_behaviour.csv")
transactionData <- read_excel("/kaggle/input/quantum-job-simulation-dataset/QVI_transaction_data.xlsx", sheet = "in")
```

##### Data Observation - Customer Data

1.  There are total 72,637 observations and 3 variables.
2.  There is no NA data in the dataset.
3.  There are 7 types of LIFESTAGE: MIDAGE SINGLES/COUPLES, NEW FAMILES, OLDER FAMILIES, OLDER SINGLES/COUPLES, RETIREES, YOUNG FAMILIES, YOUNG SINGLES/COUPLES
4.  There are 3 types of PREMIUM CUSTOMER (Budget, Mainstream, Premium)

```{r - Data Observation - Customer Data, echo=FALSE}
##### Data Observation - Customer Data
head(customerData)
summary(customerData)
print(paste("Any missing data: ", any(is.na(customerData))))
table(customerData$LIFESTAGE)
table(customerData$PREMIUM_CUSTOMER)
```

##### Data Observation - Transaction Data

1.  There are total 264,836 observations and 8 variables.
2.  There is no NA in the dataset.
3.  The DATE is in double format.
4.  PROD_NAME consists of Brand, Variety and Package Size.
5.  customerData is our target analysis, there is no missing Loyal Customer ID from transactionData in customerData.

```{r - Data Observation - Transaction Data, echo=FALSE}
##### Data Observation - Transaction Data
head(transactionData)
summary(transactionData)
print(paste("Any missing data:", any(is.na(transactionData))))

# LYLYY_CARD_NBR exists in transactionData but not in customerData
print(paste("Loyal ID not found in Customer Data:", setdiff(customerData$LYLTY_CARD_NBR, transactionData$LYLTY_CARD_NBR)))
```

## 3. Data Cleaning

1.  From data observation, we noticed that in transactionData, the DATE is in double format.

    **Action:** Convert DATE in transactionData to date format.

```{r - Data Cleaning - convert DATE into date format}
##### Convert DATE in transactionData into date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
```

2.  To facilitate the analysis later, extract day, month and day of week from DATE

    **Action:** Extract DAY, MONTH and DAY_OF_WEEK from DATE

```{r - Data Cleaning - Extract DAY, MONTH and DAY_OF_WEEK from DATE}
##### Extract DAY, MONTH and DAY_OF_WEEK from DATE

transactionData$DAY <- format(transactionData$DATE, "%d")
transactionData$MONTH <- format(transactionData$DATE, "%m")
transactionData$DAY_OF_WEEK <- format(transactionData$DATE, "%A")

head(transactionData, 3)
```

3.  We also noticed that, in PROD_NAME in transactionData has whitespaces or double spaces and inconsistency name.

    **Action:** Remove the double spaces from PROD_NAME

```{r - Data Cleaning - remove the double spaces}

print(paste("No of transaction before update:", 
transactionData %>%
  filter(grepl("\\s\\s+", PROD_NAME)) %>%
  summarise(count = n())))

transactionData$PROD_NAME <- gsub("\\s+", " ", transactionData$PROD_NAME)

print(paste("No of transaction after update:", 
transactionData %>%
  filter(grepl("\\s\\s+", PROD_NAME)) %>%
  summarise(count = n())))
```

4.  There is a lot of information in PROD_NAME, which can be extracted for further analysis, i.e. Brand and Pack Size. However, it is crucial to address ant inconsistencies in the Brand Name before processing with extraction process, i.e. Dorito, Doritos

    **Action:** Rectify and correct Brand name

```{r - Data Cleaning - Rectify and correct Brand name}
##### Rectify and correct Brand name
# replaces Dorito to Doritos
transactionData$PROD_NAME <- gsub("Dorito", "Doritos", transactionData$PROD_NAME)

# replaces Doritoss to Doritos
transactionData$PROD_NAME <- gsub("Doritoss", "Doritos", transactionData$PROD_NAME)

# replaces GrnWves to Grain Waves
transactionData$PROD_NAME <- gsub("GrnWves", "Grain Waves", transactionData$PROD_NAME)

# replaces Infzns to Infuzions
transactionData$PROD_NAME <- gsub("Infzns", "Infuzions", transactionData$PROD_NAME)

# replaces Natural Chip Compny to Natural ChipCo
transactionData$PROD_NAME <- gsub("Natural Chip Compny", "Natural ChipCo", transactionData$PROD_NAME)

# replaces NCC to Natural ChipCo
transactionData$PROD_NAME <- gsub("NCC", "Natural ChipCo", transactionData$PROD_NAME)

# replaces RRD to Red Rock Deli
transactionData$PROD_NAME <- gsub("RRD", "Red Rock Deli", transactionData$PROD_NAME)

# replaces Smith to Smiths
transactionData$PROD_NAME <- gsub("Smith", "Smiths", transactionData$PROD_NAME)

# replaces Smithss to Smiths
transactionData$PROD_NAME <- gsub("Smithss", "Smiths", transactionData$PROD_NAME)

# replaces WW to Woolworths
transactionData$PROD_NAME <- gsub("WW", "Woolworths", transactionData$PROD_NAME)

# replaces Snbts to Sunbites
transactionData$PROD_NAME <- gsub("Snbts", "Sunbites", transactionData$PROD_NAME)

# replaces Thins Potato Chips to Thins Chips
transactionData$PROD_NAME <- gsub("Thins Potato Chips", "Thins Chips", transactionData$PROD_NAME)
```

5.  With the Brand name now corrected in PROD_NAME column, we can proceed to create the BRAND attribute. This involves extracting the first word from PROD_NAME to determine the Brand name. Additionally, we'll extract any numerical values from PROD_NAME to derive the PACK_SIZE attribute.

    **Action**: Extract the first word from PROD_NAME and store in variable BRAND and numerical values to PACK_SIZE.

```{r - Data cleaning - Extract BRAND and PACK_SIZE}
##### Extract the first word from PROD_NAME and store in attribute BRAND and numerical values to PACK_SIZE

##### Step 1: Extract the first word from PROD_NAME and store in attribute BRAND
transactionData$BRAND <- word(transactionData$PROD_NAME, 1)

##### Step 2: Extract numerical values to PACK_SIZE
transactionData$PACK_SIZE <- as.integer(str_extract(transactionData$PROD_NAME, "\\d+"))

head(transactionData,3)
```

6.  There are salsa products in the dataset but we only interested in the chips category. So, let's remove the salsa products.

    **Action:** Remove Salsa products from transactionData

```{r - Data Cleaning - Remove salsal product}
##### Remove salsa products from transactionData
transactionData <- transactionData %>%
  filter(!grepl("salsa", tolower(PROD_NAME)))
```

## 4. Exploratory Data Analysis:

Now, we have cleaned the data. Let's summarise the data to check for any nulls or outlier.

```{r - EDA - summarise data to check any nulls or outlier}
summary(transactionData)
```

##### 1. Investigate the case where 200 packets of chips are bought in one transaction

    From the summary, there are no nulls in the columns but product quantity appears to have an outlier, which we should investigate further.

    Customer 226000 has bought 200 packets of chips in one transaction. Let's see if the customer have had other transactions.

    From the observation, look like Customer 226000 only had 2 transactions over the year and product quantity is 200. This is not an ordinary customer.

    They might buying it for commercial purposes. Remove this 226000 from further analysis.

```{r - EDA - investigate customer 226000}
##### Let's check which customer had bought 200 packets of chips in one transaction
transactionData %>%
    filter(PROD_QTY == 200)

##### Let's check if the customer have had other transactions
transactionData %>%
    filter(LYLTY_CARD_NBR == 226000)

##### Remove this 226000 from further analysis.
transactionData <- transactionData %>%
    filter(!LYLTY_CARD_NBR == 226000)

##### Reexamine the data
summary(transactionData)
```

##### 2. Investigate the number of date for a year is 364.

    There's only 364 rows, meaning only 364 dates which indicates there is a missing date. Let's create a sequence of dates from 01-Jul-2018 to 30-Jun-2019 and use this to create a chart of number of transactions over time to find the missing date.

    We can see that the increase in the sales occurs in the lead up to Christmas and that there are 0 sales on Christmas day itself. This is due to shops being closed on Christmas day.

    Now, we are satisfied that the data no longer has outlier, we can move on to creating other features such as Brand of chips or Pack Size from PROD name.

```{r - EDA - investigate the number of date}
##### Count the number of transactions by date
print(paste("Number of date:" ,transactionData %>%
  distinct(DATE) %>%
  count()))

##### Create a sequence of dates and join this the count of transactions by date
start_date = as.Date("2018-07-01")
end_date = as.Date("2019-06-30")
date_sequence = seq(start_date,end_date, by = "day")
date_df = data.frame(DATE = date_sequence)

transaction_count <- transactionData %>%
                        group_by(DATE) %>%
                        summarise(no_of_trans = n())

transaction_by_day <- left_join(date_df, transaction_count, by = "DATE")

head(transaction_by_day)

##### Setting plot themes to format graphs
theme_set(theme_classic())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(data = transaction_by_day) + 
  geom_line(mapping = aes(x = DATE, y = no_of_trans), color = "#756bb1") + 
  labs(x = "Date", y = "No of Transactions", title = "Transaction Over Time") +
  scale_x_date(breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.background = element_rect(fill = "#efedf5", linewidth = 0.5, color = "black"))

##### Filter December to look at individual days.
ggplot(data = transaction_by_day %>%
              filter(month(DATE) == 12)) +
  geom_line(mapping = aes(x = DATE, y = no_of_trans), color = "#dd1c77", size = 1) + 
  labs(x = "Days", y = "No of Transaction", title = "Transaction Over Time") + 
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.background = element_rect(linewidth = 0.5, color = "black", fill = "#fde0dd"))
```

##### 3. Let's check if BRAND and PACK_SIZE extracted look sensible

    We have extracted BRAND and PACK_SIZE from PROD_NAME at Data Cleaning phase. Let's observe if the BRAND and PACK_SIZE look sensible.

```{r - EDA - Check PACK_SIZE look sensible}
##### Let's check if the PACK_SIZE looks sensible.
summary(transactionData$PACK_SIZE)

##### Plot a histogram showing the number of transactions by pack size.
ggplot(data = transactionData %>%
                group_by(PACK_SIZE) %>%
                summarise(no_of_trans = n())) + 
  geom_col(mapping = aes(x = as.factor(PACK_SIZE), y = no_of_trans), fill = "#2c7fb8") + 
  labs(x = "Pack Size (g)", y = "No of Transaction", title = "No of Transaction over Pack Size") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5), panel.background = element_rect(linewidth = 0.5, color = "black", fill = "#ffffcc"))
```

    From the summary, the largest size is 380g and the smallest size is 70g. The data look's sensible.

    From the bar chart, we observe our customer prefers pack size 175g. The pack sizes created look sensible.

    Now, let's check if the BRAND's result looks sensible.

```{r - EDA - check BRAND looks sensible}
table(transactionData %>%
    distinct(BRAND))
```

    The BRAND name has been corrected during Data cleaning. The result look sensible.

    Now, we are happy with the Transaction dataset. Let's have a look at Customer dataset.

    After that, let's merge the transactionData to customerData.

```{r - EDA - Examine customer data}
##### Do some basic summaries of the dataset, including distributions of any key columns
summary(customerData)
```

```{r - EDA - Merge transactionData to customerData}
##### Merge transactionData to customerData
data <- left_join(transactionData, customerData, by = "LYLTY_CARD_NBR")
```

    As the number of rows in "data" is same as "transactionData", we can be sure that no duplicated created or missing observation. This is because the data is created by left_join which means take all rows in "transactionData" and find rows with matching values in shared column (LYLTY_CARD_NBR) and then joining the details in the rows to "customerData".

    Let's also check if some customers were not matched by checking for nulls.

```{r - EDA - check for missing customer details}
##### See if any transactions did not have a matched customer
print(paste("Transaction did not have matched customer", any(is.na(data))))
```

    Great, there are no nulls! So, all our customers in the transactionData has been accounted for in customerData.

    Now, we are continuing to Task 2. Let's maintain this dataset which we can save it as cvs.

```{r - EDA - write the data as save as csv}
write_xlsx(data, "QVI_data.xlsx")
```

## 5. Data Analysis:

Now, the data is ready for analysis, we can define some metrics of interest to the client:

1.  Who spends the most on chips (total sales), describing customer by lifestage and how premium their general purchase behavious is
2.  How many customers are in each segment
3.  How many chips are bought per customer by segment
4.  What's the average chip price by customer segment

##### Step 1: Let's start by calculating total sales by LIFESTAGE and PREMIUM CUSTOMER and plotting the split by these segment to describe which customer segment contribute most to chip sales.

```{r - Data Analysis - Total sales by LIFESTAGE and PREMIUM_CUSTOMER, fig.width = 10, fig.align = "center"}
##### Calculate the summary of sales by those dimensions and create a plot

totalSales <- data %>%
    group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
    summarise(total_sales = sum(TOT_SALES)) %>% mutate(percentage = (total_sales / sum(total_sales)) * 100) %>% arrange(desc(percentage))

fill_category = c(
  "Budget" = "#feebe2",
  "Mainstream" = "#f768a1",
  "Premium" = "#7a0177"
)

ggplot(data = totalSales, mapping = aes(x = LIFESTAGE, y = total_sales, fill = PREMIUM_CUSTOMER)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(percentage, 2), "%"), y = total_sales), vjust = 0.3, position = position_stack(vjust = 0.5)) +
  labs(x = "Life Stage", y = "Total Sales", title = "Total Sales over Life Stage and Premium Customer", fill = "Premium Customer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(linewidth = 0.5, color = "black"))+
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = fill_category)
```

**Conclusion:** Sales are mainly coming from Budget - Older Families, Mainstream - Young Singles/Couples, Mainstream - Retirees.

##### Step 2: Let's see if the higher sales are due to there being more customers who buy chips.

```{r - Data Analysis, No of customers by LIFESTAGE and PREMIUM_CUSTOMER, fig.width = 10, fig.align = "center"}
##### Calculate the summary of number of customers by those dimensions and create a plot.

totalCustomer <- data %>%
    group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
    summarise(total_customer = n_distinct(LYLTY_CARD_NBR)) %>%
    arrange(desc(total_customer))

ggplot(data = totalCustomer, mapping = aes(x = LIFESTAGE, y = total_customer, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity") + 
  geom_text(mapping = aes(label = total_customer), position = position_stack(vjust = 0.5)) + 
  labs(x = "Lifestage", y = "No of Customers", fill = "Premium Customer", title = "No of Customer ober Lifestage and Premium Customer") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(linewidth = 0.5, color ="black")) + scale_fill_manual(values = fill_category)

```

**Conclusion:** There are more Mainstream - Young Singles/Couples and Mainstream - Retirees who buy chips but this is not the major drive for the Budget - Older Families segment.

##### Step 3: Higher sales may also driven by more units of chips being bought per customer. Let's have a look at this.

```{r - Data Analysis -  Average bought per customer by segment,fig.width = 10, fig.align = "center" }
##### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER by segment

averageQuantity <- data %>%
                  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
                  summarise(quantity = sum(PROD_QTY), average = quantity/n_distinct(LYLTY_CARD_NBR)) %>%
                  arrange(desc(quantity))

ggplot(data = averageQuantity, mapping = aes(x = LIFESTAGE, y = average, fill = PREMIUM_CUSTOMER)) + 
  geom_bar(stat = "identity") + 
  geom_text(mapping = aes(label = round(average,2)), position = position_stack(vjust = 0.5)) +
  labs(x = "Life Stage", y = "Average Quantity", fill = "Premium Customer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(linewidth = 0.5, color = "black")) +
  scale_fill_manual(values = fill_category)
```

**Conclusion:** Older families and young families in general buy more chips per customer.

##### Step 4: Let's also investigate the average price per unit chips bought for each customer segment as this also a driver of total sales.

```{r - Data Analysis - Average price per unit, fig.width = 10, fig.align = "center"}
##### Calculate and plot the average price per unit sold (average sale price) by those two customer dimensions

averagePrice <- data %>%
                  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
                  summarise(average_price = sum(TOT_SALES)/sum(PROD_QTY)) %>%
                  arrange(desc(average_price))

ggplot(data = averagePrice, mapping = aes(x = LIFESTAGE, y = average_price, fill = PREMIUM_CUSTOMER)) + 
         geom_bar(stat = "identity") +
         geom_text(mapping = aes(label = round(average_price,2)), position = position_stack(vjust = 0.5)) +
         labs(x = "Life Stage", y = "Average Price", fill = "Premium Customer", title = "Average Price per Unit Sold") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(linewidth = 0.5, color ="black")) +
  scale_fill_manual(values = fill_category)
```

**Conclusion:** Mainstream Midage Single/Couples and Mainstream Young single/Couples are more willing to pay more per packet of chips compare to their budget and premium counterparts. This may be due to premium shoppers being more likely to buy healthy snacks and when they buy chips, this is mainly for entertainment purposes rather than their own consumption. This is also supported by there being fewer Premium Midage and Young Single/Couples buying chips compared to their mainstream counterparts.

##### Step 5: As the difference is average price per unit isn't large, we can check if this difference is statistically different.

```{r - Data Analysis - Perform T-test between mainstream vs premium and budget}

##### Perform an independent t-test between mainstream vs premium and budget midage and young singles and couples

data_unit_price <- data %>%
  mutate(PRICE =TOT_SALES/PROD_QTY)

t_test1 <- data_unit_price %>%
            filter(PREMIUM_CUSTOMER == "Mainstream" & LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"))

t_test2 <- data_unit_price %>%
            filter(!PREMIUM_CUSTOMER == "Mainstream" & LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"))

t.test(t_test1$PRICE,
       t_test2$PRICE,
       var.equal = TRUE,
       mu = 0,
       conf.level = 0.95)
```

-   The t-statistic is very large (t = 37.832), indicating a significant difference between the means of the two groups.

-   The p-value is extremely small (p \< 2.2e-16), which is essentially zero. This indicates very strong evidence against the null hypothesis.

-   The alternative hypothesis suggests that the true difference in means is not equal to zero. In other words, there is a significant difference between the two groups.

-   The 95% confidence interval for the true difference in means is between approximately 0.316 and 0.351. This means we can be highly confident (95% confidence) that the true difference falls within this range.

**Conclusion:** Based on this t-test result, we have strong evidence to reject null hypothesis. The t-test results in a p-value of 2.2e-16, i.e. the unit price for mainstream, young and mid-age singles and couples are significantly higher than that of budget or premium, young and midage singles and couples.

##### Step 6: We have quite a few interesting insights that we can dive into. We might want to target customer segments that contribute the most to sales to retain them or further increase sales. Let's look at Mainstream - young single/couples. For instance, let's find out if they tend to buy particular brand of chips.

```{r - Data Analysis - Affinity analysis (Brand), fig.align = "center"}
##### Work out of there are brands that these two customer segments prefer more than others. You could use a technique called affinity analysis or a-priori analysis (or any other method if you prefer)

##### Deep dive into Mainstream, young singles/couples

##### Step 1: Let's calculate the sum total
sum_total <- data %>%
  summarise(sum_total = n())

##### Step 2: The LHS is brand, for whole dataset
LHS <- data %>%
    group_by(BRAND) %>%
    summarise(LHS_freq = n()/sum_total)

##### Step 3: The Mainstream & Young Couples is RHS, for whole dataset
RHS <- data %>%
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream") %>%
  summarise(RHS_freq = n()/sum_total)

##### Step 4: Calculate the occurrences for Young Singles/Couples and Mainstream for each brand.

occurences <- data %>%
              filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream") %>%
              group_by(BRAND) %>%
              summarise(occurences = n())

##### Step 6: Calculate Predicted Trans
predicted_trans <- LHS %>%
    group_by(BRAND) %>%
    summarise(predicted_trans = LHS_freq * RHS$RHS_freq*sum_total)

##### Step 6: Merge occurences and predicted trans to calculate Lift for each brand

lift_data <- merge(occurences,
                  predicted_trans,
                  by = "BRAND")

lift_data$lift <- lift_data$occurences / lift_data$predicted_trans

lift_data %>%
    mutate(predicted_trans = unlist(predicted_trans),
           lift = unlist(lift)) %>%
  arrange(desc(lift))

```

The output \<data.frame [22 × 1]\> predicted_trans and lift columns in the lift_dataare still data frames, rather than vectors. This is because the format() function does not convert a data frame to a vector, it only formats the values in a data frame.

To convert the data frame to a vector, you can use the unlist() function. The unlist() function converts a data frame to a vector by extracting the values from the data frame and combining them into a single vector.

**Interpreting Lift:**

Lift > 1: Indicates that the brand is more likely to be preferred in the segment compared to the overall population. 

Lift < 1: Indicates that the brand is less likely to be preferred in the segment compared to the overall population. 

Lift = 1: Indicates that the brand preference is proportional in both the segment and the overall population.

**Conclusion:** It's evident that among Mainstream - Young Singles/Couples, Tyrrells stands out as the preferred brand which more than 21%, while Burger is the least favored option which is less than 50%"

Additionally, Twisties, Doritos, Tostitos, Kettle, Pringles, and Cobs present promising opportunities for further sales growth within the Mainstream - Young Singles/Couples segment, complementing the popularity of Tyrrells.

##### Step 7: Let's also find out if our target segment tends to buy larger packs of chips.

```{r - Data Analysis - Affinity analysis (PACK_SIZE), fig.align = "center"}
##### Work out of there are PACK_SIZE that these two customer segments prefer more than others. You could use a technique called affinity analysis or a-priori analysis (or any other method if you prefer)

##### Deep dive into Mainstream, young singles/couples

##### Step 1: Let's calculate the sum total
sum_total_pack <- data %>%
  summarise(sum_total_pack = n())

##### Step 2: The LHS is brand, for whole dataset
LHS_pack <- data %>%
    group_by(PACK_SIZE) %>%
    summarise(LHS_freq_pack = n()/sum_total_pack)

##### Step 3: The Mainstream & Young Couples is RHS, for whole dataset
RHS_pack <- data %>%
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream") %>%
  summarise(RHS_freq_pack = n()/sum_total_pack)

##### Step 4: Calculate the occurrences for Young Singles/Couples and Mainstream for each brand.

occurences_pack <- data %>%
              filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream") %>%
              group_by(PACK_SIZE) %>%
              summarise(occurences_pack = n())

##### Step 6: Calculate Predicted Trans
predicted_trans_pack <- LHS_pack %>%
    group_by(PACK_SIZE) %>%
    summarise(predicted_trans_pack = LHS_freq_pack * RHS_pack$RHS_freq_pack*sum_total_pack)

##### Step 6: Merge occurences and predicted trans to calculate Lift for each brand

lift_data_pack <- merge(occurences_pack,
                  predicted_trans_pack,
                  by = "PACK_SIZE")

lift_data_pack$lift_pack <- lift_data_pack$occurences_pack / lift_data_pack$predicted_trans_pack

lift_data_pack %>%
    mutate(predicted_trans_pack = unlist(predicted_trans_pack),
           lift_pack = unlist(lift_pack)) %>%
  arrange(desc(lift_pack))

data %>%
  filter(PACK_SIZE == 270) %>%
  select(BRAND) %>%
  distinct()

```

**Conclusion:** We can see that: It's evident that among Mainstream - Young Singles/Couples, 270g stands out as the preferred pack size which is more than 24%, while 220g is the least favored option, which is less than 50%.

Twisties are the only brand offering 270g packs and so this may instead be reflecting a higher likelihood of purchasing Twisties.

## 6. Summary
1. The main contributors to sales are Budget - Older Families, Mainstream - Young Singles/Couples, and Mainstream - Retirees.

2. While Mainstream - Young Singles/Couples and Mainstream - Retirees show higher chip purchases, this behavior is not as pronounced in the Budget - Older Families segment.

3. Both Older and Young families tend to buy more chips per customer.

4. Mainstream Midage Singles/Couples and Mainstream Young Singles/Couples demonstrate a higher willingness to pay for a packet of chips compared to their budget and premium counterparts. This suggests that premium shoppers may prioritize healthier snacks, and when they do buy chips, it's likely for entertainment purposes rather than personal consumption. This pattern is further supported by the lower number of Premium Midage and Young Singles/Couples buying chips compared to their mainstream counterparts.

5. The t-test results yield a p-value of 2.2e-16, indicating that unit prices for mainstream, young, and mid-age singles and couples are significantly higher than those for budget or premium, young, and midage singles and couples.

6. Among Mainstream - Young Singles/Couples, Tyrrells emerges as the preferred brand with over a 21% preference rate, while Burger is the least favored option, with less than 50% preference.

7. It's evident that among Mainstream - Young Singles/Couples, the 270g pack size is the preferred choice, accounting for over 24% of purchases, while the 220g option is the least favored.
