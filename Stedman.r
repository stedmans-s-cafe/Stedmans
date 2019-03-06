# TABLES FROM THE MYSQL DATABASE #####
library(tidyverse)
library(RMySQL)
mydb = dbConnect(MySQL(), user='viewer', password='', dbname='stedmans', host='dadata.cba.edu')
summary(mydb)

## View the names of the tables in the database
dbListTables(mydb)

## View the names of the columns in each of the tables that we have
dbListFields(mydb, 'orders')
dbListFields(mydb, 'product_sales')

## Convert "id" to character (orders)
orders_id <- as.character(orders$id)
class(orders_id)

## Convert orders into table we can use
order_data <- dbSendQuery(mydb, "SELECT * FROM orders")
orders <- fetch(order_data, n=-1)
summary(orders)
nrow(orders)

## Convert "id" to character (product_sales)
product_sales_id <- as.character(product_sales$id)
class(product_sales_id)

## Convert product_sales into table we can use
product_sales_data <- dbSendQuery(mydb, "SELECT * FROM product_sales")
product_sales_prep <- fetch(product_sales_data, n=-1)
nrow(product_sales_prep)


## perform left join so the tables are combined
stedman2join <- orders %>% left_join(product_sales, by = c("id" = "receiptKey"))
head(stedman2join)
sted <- stedman2join %>% select(-id.y)
nrow(sted)

## write.csv(stedman2, file = "database.csv")



# TABLES FROM THE EXCEL SHEETS FROM LEVI #####
# Download the xlsx file from here (https://github.com/stedmans-s-cafe/Stedmans)
library(readxl)
xl_data <- "/Users/Sam/Desktop/Project/levi.xlsx" # creating a variable name for the file path
excel_sheets(path = xl_data) # viewing the names of the two tabs

## reading in our first tab (Inventory)
tab2_name <- read_excel(path = xl_data, sheet = "Inventory")

## selecting only the 4 columns that we need from the Inventory tab
tab3 <- tab2_name %>% select(Category, ProductName, Cost, Price) 

## calculating profit margin per item
margin1 <- mutate(tab3, prof_margin = Price - Cost) 
excel_table <- mutate(margin1, prof_margin_perc = prof_margin / Cost)

## reading in our other tab (Product)
tab1_name <- read_excel(path = xl_data, sheet = "Product")
head(tab1_name)

### selecting the columns we want. ignoring the mess to the right
tab4 <- tab1_name %>% select(ProductType, ProductName) 





# Preparing for join -----------------------------------------------------------------
# merging all of the empty space from tab1_name into one column
class(tab1_name)
data <- as.data.frame(tab1_name[, c(4, 21:83)])
cbind(1:nrow(data), max.col(!is.na(data[-1]), "last"))
flavors <- data[-1][cbind(1:nrow(data), max.col(!is.na(data[-1]), "last"))]
product_side <- cbind(tab4,flavors)

### join the two excel tabs together
excel_join <- product_side %>% left_join(excel_table, by = c("ProductName" = "ProductName"))

### attaching strings ('ProductName' + 'flavor')
library(stringr)
new_excel_join <- as_tibble(mutate(excel_join,
                                   matching = str_c(ProductName, flavors, sep = " + ")))

nrow(new_excel_join)
match <- sub("\\s\\+\\sOff$", "", new_excel_join$matching)


# Two tables before the merge
# excel = cost sheet
# sted = mySQL transactions
new_excel_join <- new_excel_join[,-9]
excel <- as_tibble(cbind(new_excel_join,match))
sted <- as_tibble(sted) # relevant columns from mySQL database
#write.csv(excel, file = "excel.csv")
#write.csv(sted, file = "sted.csv")



# Joining tables ----------------------------------------------------------
library(sqldf)
library(gdata)
detach("package:RMySQL", unload=TRUE)


# Cost sheet (2098 rows)
product_margins <- read.csv(file='excel.csv', header=TRUE, sep=",")
product_margins <- as_tibble(product_margins)
product_margins <- product_margins %>% select(-X)

# mySQL transactions (202k rows)
sted <- read.csv(file='sted.csv', header=TRUE, sep=",")
sted <- as_tibble(sted)
sted <- sted %>% select(-X)

# joined tables (232k rows)
joinedData <- sqldf('SELECT * FROM sted JOIN product_margins ON sted.product LIKE product_margins.match')
nrow(joinedData)
ncol(joinedData)

deduped.data <- unique( joinedData[ , 1:17 ] )
stedmans <- as_tibble(deduped.data)
stedmans
nrow(stedmans)




# subset
stedmans <- stedmans %>% select(-Type,-product,-ProductType,-Subtotal,-Tax,-Total,-Discounts,-match)
stedmans <- stedmans %>% filter(Category != "Bird" & 
                                  Cost != "NA" & 
                                  Category != "Catering" &                             
                                  Category != "Gift Card" &
                                  Category != "Catering" &
                                  Category != "Extras" &
                                  Category != "NA" &
                                  ProductName != "Tea Bib")
nrow(stedmans)
(stedmans <- as_tibble(stedmans))
View(stedmans)

#write.csv(stedmans, file = "tableau.csv")



# Distributions -------------------------------------------------------------
library(ggplot2)
library(lubridate)
stedmans <- stedmans %>% mutate(Date = ymd_hms(stedmans$DateTime, tz = "UTC")) %>% select(-DateTime)


#Viewing unique types of categories -- catering and others were throwing off results
(unidff <- stedmans[!duplicated(stedmans$Category), ])

# Charting sales by category
bfreq <- stedmans %>% group_by(Category) %>% summarise(n = n(), p = n/nrow(stedmans)) %>% arrange(desc(n))
btop <- bfreq[1:13,]
ggplot(btop,aes(x = reorder(Category, n),y=n))+geom_col() + coord_flip() +
  labs(x='Categories',y='Number of Sales',title='Sales by Category')

# Charting sales by product
bfreq2 <- stedmans %>% group_by(ProductName) %>% summarise(n = n(), p = n/nrow(stedmans)) %>% arrange(desc(n))
btop2 <- bfreq2[1:30,]
ggplot(btop2,aes(x = reorder(ProductName, n),y=n))+geom_col() + coord_flip() +
  labs(x='ProductName',y='Number of Sales',title='Sales by Product')


# sales by hour
b <- ggplot(data=stedmans,aes(hour(Date)))
b + geom_histogram(binwidth = .5)

# getting dates formatted correctly (using dplyr, lubridate)
stedmans$h <- hour(stedmans$Date)
stedmans$m <- month(stedmans$Date)
stedmans$y <- year(stedmans$Date)

# product sales by month (top 9)
toplist <- bfreq2$ProductName[1:9]  # "toplist" is now the 9 most frequently sold products
btop <- stedmans %>% select(id,ProductName,h,m,y) %>% # this is selecting our id for each transaction, the product name, and our hour / month / year
  filter(ProductName %in% toplist) %>% 
  group_by(y,m,ProductName) %>% summarize(n=n()) %>% arrange(ProductName, y, m)
btop$date <- ymd(paste(btop$y, btop$m,12, sep="/"))
ggplot(btop,aes(x=date,y=n,color=ProductName)) + geom_line(lwd=0.8) + geom_point() + 
  facet_wrap(ProductName~.) + labs(x='Date',y='Number of Sales',title='Product Sales by Months (Top 7-12)')

# by hour
toplist <- bfreq2$ProductName[1:9]
sdata <- subset(stedmans,ProductName %in% toplist) 
avgdata <- sdata %>% group_by(Date,h,ProductName) %>% summarise(n = n()) %>% group_by(h,ProductName) %>% filter(h>=8 & h<=18) %>% summarise(avg_n = mean(n))
ggplot(avgdata,aes(x=h,y=avg_n,color=ProductName)) + geom_line(lwd=0.8) + geom_point() + 
  facet_wrap(ProductName~.) + labs(x='Hour',y='Average Number of Sales',title='Product Sales by Hour (Top 1-9)')



