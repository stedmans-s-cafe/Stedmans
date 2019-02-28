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
summary(sted)
nrow(sted)

## write.csv(stedman2, file = "database.csv")
















# TABLES FROM THE EXCEL SHEETS FROM LEVI #####
## reading in an xlsx workbook with multiple tabs (https://rpubs.com/tf_peterson/readxl_import)
## NOTES
library(readxl)
# Renamed the first tab "
xl_data <- "/Users/Sam/Desktop/Project/levi.xlsx" # creating a variable name for the file path

excel_sheets(path = xl_data) # viewing the names of the two tabs

## reading in our first tab (Inventory)
tab2_name <- read_excel(path = xl_data, sheet = "Inventory")

## selecting only the 4 columns that we need
tab3 <- tab2_name %>% select(Category, ProductName, Cost, Price) 


## calculating profit margin per item
margin1 <- mutate(tab3, prof_margin = Price - Cost) 
excel_table <- mutate(margin1, prof_margin_perc = prof_margin / Cost)



## reading in our first tab (Product)
tab1_name <- read_excel(path = xl_data, sheet = "Product")
nrow(tab1_name)
ncol(tab1_name)
head(tab1_name)

### selecting the columns we want. ignoring the mess to the right
tab4 <- tab1_name %>% select(ProductType, ProductName) 












# Preparing for join -----------------------------------------------------------------




# merging all of the empty space from tab1_name into one column
# https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas
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
write.csv(excel, file = "excel.csv")
write.csv(sted, file = "sted.csv")



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

deduped.data <- unique( joinedData[ , 1:12 ] )
stedmans <- as_tibble(deduped.data)
nrow(stedmans)




# subset (226k rows)
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
arrange(stedmans, desc(Total))
View(stedmans)




library(lubridate)
Date <- ymd_hms(stedmans$DateTime, tz = "UTC")

# Distributions -------------------------------------------------------------
library(ggplot2)


#Viewing unique types of categories -- catering and others were throwing off results
(unidff <- stedmans[!duplicated(stedmans$Category), ])

# Charting sales by category
bfreq <- stedmans %>% group_by(Category) %>% summarise(n = n(), p = n/nrow(stedmans)) %>% arrange(desc(n))
btop <- bfreq[1:13,]
ggplot(btop,aes(x = reorder(Category, n),y=n))+geom_col() + coord_flip() +
  labs(x='Categories',y='Number of Sales',title='Sales by Category')

# sales by hour
b <- ggplot(data=stedmans,aes(hour(Date)))
b + geom_bar(binwidth = .5)




