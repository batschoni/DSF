library(RSelenium)
library(rvest)
library(tidyverse)


# start the Selenium server
rD <- rsDriver(verbose = FALSE, browser = "firefox")

#set the xPaths
xPath = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[4]/div/div/span[1]"
xPath2 = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[1]"
x_Path_rev_number = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[4]/div/div/span[2]/span/a/span"
x_Path_rev_number2 = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[2]/span/a/span"

# assign the client to a new variable, visit a webpage
myclient <- rD$client
    
#data handling

data = read.csv("/data/Unique_data_food_inspections.csv")

scraping_parameter = select(data, Trade.Name, City)
scraping_parameter = unite(scraping_parameter, "searching", Trade.Name:City, remove = FALSE, sep = " ")
scraping_p_vector = as.factor(pull(scraping_parameter, searching))

####possible sample for testing purpose
# sample <- sample(c(TRUE, FALSE), 
#                  length(scraping_p_vector), replace = T, 
#                  prob = c(0.03,0.97))
# scraping_p_vector = scraping_p_vector[sample]
#scraping_p_vector = scraping_p_vector[1:10]
#scraping_parameter = slice(scraping_parameter, 1:10)

results_scraping = select(scraping_parameter, -searching)
results_scraping = mutate(results_scraping, Reviews = rep(1, length(scraping_p_vector)), Number_of_Reviews = rep(1, length(scraping_p_vector)))


#defining "a" for filtering purpose in for-loop
a <- character(0)

#scraping loop
for (i in 1:length(scraping_p_vector)) {
  print(i)
  myclient$navigate("http://www.google.com/ncr")
  Sys.sleep(1)
  webElem <- myclient$findElement('xpath', "//input[@name='q']") #select typing element
  
  webElem$sendKeysToElement(list(scraping_p_vector[i], key = "enter")) #enter the respective adress and trade name
  Sys.sleep(2)
  html_doc = read_html(myclient$getPageSource()[[1]]) #download the page content
  link_nodes = html_nodes(html_doc, xpath = xPath)
  link_nodes_number = html_nodes(html_doc, xpath = x_Path_rev_number)
  link_nodes2 = html_nodes(html_doc, xpath = xPath2)
  link_nodes_number2 = html_nodes(html_doc, xpath = x_Path_rev_number2)
  
  if (!identical(a, html_text(html_nodes(link_nodes2, xpath = xPath2)))){
    
    results_scraping[i,3] = html_text(html_nodes(link_nodes2, xpath = xPath2)) #filter the review data out of the html document
    results_scraping[i,4] = html_text(html_nodes(link_nodes_number2, xpath = x_Path_rev_number2))
    Sys.sleep(1)
    print("review found")
    
  } else if (!identical(a, html_text(html_nodes(link_nodes, xpath = xPath)))) {
    
    results_scraping[i,3] = html_text(html_nodes(link_nodes, xpath = xPath)) #filter the review data out of the html document
    results_scraping[i,4] = html_text(html_nodes(link_nodes_number, xpath = x_Path_rev_number))
    Sys.sleep(1)
    print("review found")
    
    
  } else {
    results_scraping[i,3] = 0
    results_scraping[i,4] = 0
    Sys.sleep(1)
    
  }
}

#Cleaning the scraped data
results_scraping[,4] = as.numeric(str_remove(results_scraping[,4], "Google review" )) #removes "google reviews" string +  converts to numeric
results_scraping[,3] = as.numeric(results_scraping[,3])
results_scraping$Number_of_Reviews = substr(results_scraping$Number_of_Reviews,1,nchar(results_scraping$Number_of_Reviews)-14)
results_scraping$Number_of_Reviews[which(results_scraping$Number_of_Reviews == "")] = 0
results_scraping$Number_of_Reviews = as.numeric(str_trim(str_remove_all(results_scraping$Number_of_Reviews, "’")))


#Saving the Review-Data in csv
write.csv(results_scraping, file="results_scraping.csv")


myclient$close()
rD$server$stop()

