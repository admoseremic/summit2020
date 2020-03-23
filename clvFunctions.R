require("RPostgreSQL")
library(dbplyr)
library(dplyr)
library(rstudioapi)

connectToQueryService = function(host_name, port = 80, db_driver = "PostgreSQL", db_name){
  drv = dbDriver(db_driver)
  port = port
  con = dbConnect (drv, 
                   dbname = paste0("dbname=", db_name, " sslmode=require"), 
                   host = host_name, 
                   port = port, 
                   user = rstudioapi::askForPassword("Username"), 
                   password = rstudioapi::askForPassword("Password"))
  
  return(con)
}

lifetimeQuery = function(tbl_con, revenue_id, user_id, page = 1, page_size = 50000) {
  revenue = sql(revenue_id)
  user_id = sql(user_id)
  
  lifetime_value_data = tbl_con %>%
    filter(revenue > 0) %>%
    arrange(timestamp) %>%
    mutate(user_id = user_id,
           revenue = revenue,
           timestamp = timestamp,
           row_num = row_number()) %>%
    select(user_id,
           revenue,
           timestamp,
           row_num) %>%
    filter(row_num > (page-1)*page_size & row_num <= page_size * page ) %>%
    select(-row_num) %>%
    collect()
  
  return(lifetime_value_data)
}

getCLVData = function(table_name, con_obj, query_size = 50000, revenue_id, user_id) {
  table_conn = tbl(con_obj, table_name)
  
  page_iter = 1
  query_df = data.frame()
  while(query_size >= 50000) {
    new_df = lifetimeQuery(tbl_con = table_conn,
                           revenue_id = revenue_id,
                           user_id = user_id,
                           page = page_iter)
    
    query_df = query_df %>%
      bind_rows(new_df)
    
    query_size = nrow(new_df)
    page_iter = page_iter + 1
    print(c(query_size, page_iter))
  }
  
  # Convert timestamp to date and rename columns
  query_df = query_df %>%
    mutate(date = as.Date(timestamp, "%m/%d/%y %H:%M")) %>%
    rename(cust = user_id,
           sales = revenue) %>%
    select(-timestamp)
  
  dbDisconnect(con)
  return(query_df)
}

calculateBGNBDParams = function(cbs_df) {
  params_bgnbd = bgnbd.EstimateParameters(cbs_df)
  return(params_bgnbd)
}


calculateGGParams = function(customer_data) {
  spend_df = customer_data %>%
    group_by(cust) %>%
    summarise(average_spend = mean(sales),
              total_transactions = n())
  
  spend_params = spend.EstimateParameters(spend_df$average_spend, 
                                          spend_df$total_transactions)
  
  return(spend_params)
}


