# Create Dataset

library("rvest")
library("dplyr")
library("xml2")

## Initialize dataframe with the desired data columns
df <- data_frame(title = character(), 
                 amount = character(), 
                 dep = character(), 
                 trans = character(), 
                 views = character(),
                 city = character(),
                 date = character(), 
                 time = character())

## Create empty dataframe
dftemp <- data_frame()

## Scrape data from web

for (i in 0:99) { #loop through first 100 pages, 10 results per page = 1000 
  link <- paste("http://www.ipaidabribe.com/reports/paid?page=",i*10, sep = "") #Create hyperlink based on loop function
  print(paste("processing", i, sep = " ")) #progress report
  main <- read_html(link, encoding = "UTF-8") #define the static part of link references
  
  title <- main %>% 
    html_nodes(".heading-3 a") %>% 
    html_text() 
  
  amount <- main %>% # feed `main.page` to the next step
    html_nodes(".paid-amount span") %>% # get the CSS nodes
    html_text() # extract the link text
  
  dep <- main %>% # feed `main.page` to the next step
    html_nodes(".name a") %>% # get the CSS nodes
    html_text() # extract the link text
  
  trans <- main %>% # feed `main.page` to the next step
    html_nodes(".transaction a") %>% # get the CSS nodes
    html_text() # extract the link text
  
  views <- main %>% # feed `main.page` to the next step
    html_nodes(".overview .views") %>% # get the CSS nodes
    html_text() # extract the link text
  
  city <- main %>% # feed `main.page` to the next step
    html_nodes(".location") %>% # get the CSS nodes
    html_text() # extract the link text
  
  date <- main %>% 
    html_nodes(".date") %>% 
    html_text() 
  
  time <- main %>% 
    html_nodes(".time-span") %>% 
    html_text() 
  
  dftemp <- cbind(title, amount, dep, trans, views, city, date, time) #bind the variables together into a 10 by n dataframe
  df <- rbind(df,dftemp)
  
  Sys.sleep(1) #timer, wait 1 second
  cat(" done!\n") #progress report
}

## Split the city column

df$states <- lapply(strsplit(as.character(df$city), "\\,"), "[", 2)
df$city <- lapply(strsplit(as.character(df$city), "\\,"), "[", 1)

## Clean and order dataset

library(stringr)

df$title <- df$title %>% 
  str_replace_all(pattern = "\\n" , replacement = " ") %>%
  str_trim()

df$amount <- df$amount %>%
  str_replace_all(pattern = "Paid INR" , replacement = " ") %>%
  str_replace_all(pattern = "," , replacement = "") %>%
  str_trim()

df$views <- df$views %>%
  str_replace_all(pattern = "views" , replacement = " ") %>%
  str_trim()

df$city <- df$city %>% 
  as.character(df$city) %>%
  str_trim()

df$states <- df$states %>% 
  as.character(df$states) %>%
  str_trim()

df <- df[, c(1,2,3,4,5,6,9,7,8)]

## Suggestion: Analysis of Variance between transaction and amount per province or city