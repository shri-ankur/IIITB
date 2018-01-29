#Task1:
# A:
# Superstore sales data contains all data about transactions that take place.
# The data is subdivided into 5 tables that are connected to each other through Foreign keys.
# The data integrity is maintained through Primary keys which are either atomic or composite
# in nature. Cust_dimen contains details about all customers and uniquely identifies each Customer
# through Cust_id. Similarly, Orders_dimen uniquely identifies each order, prod_dimen uniquely 
# identifies each product and shipping_dimen uniquely identifies each shipment. Market_fact table
# contains all the transactions and each transaction is uniquely identified through a combination
# of Order id, Product id, shipping id and customer id.

# B:
# Cust_dimen contains Cust_id as the Primary Key of text datatype. Other fields are Customer_name,
# Province, Region which are text datatype. Apart from the geographic information, Superstore
# categorizes each customer into a segment which is provided by Customer_Segment field also of text
# datatype. Orders_dimen has Ord_id as the Primary key and of Text datatype. Other fields are Order
# date of date datatype, Order_id and order priority of text type. Prod_dimen contains Prod_id as
# the Primary key of text datatype and Product category and subcategory also of text datatype. 
# Shipping_dimen contains Ship_id as the primary key of text datatype, Ship_date of date datatype
# Ship_Mode of text datatype and Order_id of text datatype. Order_id is a foreign key from the
# Orders_dimen table. Market_fact contains details of all the transactions that take place at 
# Superstore. Market_fact does not have an atomic Primary key but a Composite Primary key consisting of
# Ord_id, Prod_id, Ship_id and Cust_id each of text datatype. In addition, each of ord_id, Prod_id
# Ship_id and Cust_id are also individually Foreign keys into the market_fact table. Other fields are
# Order_Quantity of integer datatype and Sales, Discount, Profit, Shipping_Coost and 
# Product_base_margin each of double datatype. 

#Task 2: Basic Analysis
#A This requires using aggregate functions of sum and average over the sales column.
select round(sum(sales),2) as total_sales, round(avg(sales),2) as avg_sales
from market_fact;

#B This requires grouping of customers by Region, applying count aggregate function and displaying
# in descending order. 
select Region, count(distinct Cust_id) as no_of_customers
from cust_dimen
group by Region
order by no_of_customers desc;

#Q3 This requires grouping the customers by Region, applying count function on groups and then
# selecting the maximum aggregate value using the having clause. 
select Region, count(distinct Cust_id) as no_of_customers
from cust_dimen
group by Region
having no_of_customers >= all(select count(distinct Cust_id) 
							  from cust_dimen
                              group by Region);

#D Group the transactions by product id, apply sum function on order quatity of each group and display
# the result in descending order of the sums. 
select Prod_id, sum(Order_Quantity) as no_of_products_sold
from market_fact
group by Prod_id
order by no_of_products_sold desc;

#E Join the market_fact table with cust-dimen table on cust-id nd with Prod_dimen table on prod_id. 
#Filter those records where Region is Atlantic and where product sub-category is Tables, group them
# by cust_id and apply sum function on order quantity
select c.customer_name, sum(order_quantity) as no_of_tables
from cust_dimen c 
inner join market_fact m on m.Cust_id = c.Cust_id
inner join prod_dimen p on m.Prod_id = p.Prod_id
where c.Region = "ATLANTIC"
and p.Product_Sub_Category = "TABLES"
group by m.Cust_id;

#Task 3: Advanced Analysis
#A. Join prod_dimen table with market_fact table on prod_id, group by product category, apply sum 
# function and display the records in descending order. 
select p.product_category, round(sum(m.profit),2) as profits
from prod_dimen p
inner join market_fact m on p.Prod_id = m.Prod_id
group by p.Product_Category
order by profits desc;

#B. Join prod-dimen table with market_fact table on Prod_id, group by product_category and then by
# product sub-category, apply sum function on profit . 
select p.product_category, p.Product_Sub_Category, round(sum(m.profit),2) as profits
from prod_dimen p
inner join market_fact m
on p.Prod_id = m.Prod_id
group by p.Product_Category, p.Product_Sub_Category;

# C. Least profitable product subcategory  can be found from a vaiant of the above query in B.
select p.product_category, p.Product_Sub_Category, round(sum(m.profit),2) as profits
from prod_dimen p
inner join market_fact m
on p.Prod_id = m.Prod_id
group by p.Product_Category, p.Product_Sub_Category
order by profits;

# This is tables sub-category. this can then be harcoded into the following query
# Join cust_dimen table with market_fact table on cust_id and then join with prod-dimen table on
# prod_id, filetr the records for where sub-category is Tables, group the records by region, apply
# count function on shipping ids and sum function on profit display the records in descending order
# of sum of profits
select c.region, count(m.ship_id) as no_of_shipments, round(sum(m.profit),2) as profit_in_each_region
from cust_dimen c 
inner join market_fact m
on c.Cust_id = m.Cust_id
inner join prod_dimen p on p.Prod_id = m.Prod_id
where p.Product_Sub_Category = "TABLES"
group by c.Region
order by profit_in_each_region desc;