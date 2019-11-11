library(RSelenium)
library(rvest)
library(tidyverse)
# start the Selenium server
rD <- rsDriver(verbose = FALSE, browser = "firefox")
xPath = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[1]"
x_Path_rev_number = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[2]/span/a/span"
# assign the client to a new variable, visit a webpage
myclient <- rD$client
    
#data handling
data = read.csv("../Data/Retail_Food_Store_Inspections___Current_Ratings.csv")   
scraping_parameter = select(data, Trade.Name, City)
scraping_parameter = unite(scraping_parameter, "searching", Trade.Name:City, remove = FALSE, sep = " ")
scraping_p_vector = as.factor(pull(scraping_parameter, searching))

#possible sample for testing purpose
sample <- sample(c(TRUE, FALSE), 
                 length(scraping_p_vector), replace = T, 
                 prob = c(0.03,0.97))
scraping_p_vector = scraping_p_vector[sample]
#scraping_p_vector = scraping_p_vector[1:10]
#results_scraping = slice(results_scraping, 1:10)

results_scraping = select(scraping_parameter, -searching)
results_scraping = mutate(results_scraping, Reviews = rep(1, length(scraping_p_vector)), Number_of_Reviews = rep(1, length(scraping_p_vector)))

#defining "a" for filtering purpose in for-loop
a <- character(0)

#webscraping function with RSelenium; needs input_vector and connected xPath
webscraping = function (input_vector) {
  start.time = Sys.time()
  for (i in 1:length(input_vector)) {
    
    myclient$navigate("http://www.google.com/ncr")
    webElem <- myclient$findElement('xpath', "//input[@name='q']") #select typing element
    
    webElem$sendKeysToElement(list(input_vector[i], key = "enter")) #enter the respective adress and trade name
    Sys.sleep(2)
    html_doc = read_html(myclient$getPageSource()[[1]]) #download the page content
    link_nodes = html_nodes(html_doc, xpath = xPath)
    link_nodes_number = html_nodes(html_doc, xpath = x_Path_rev_number)
    
    if (identical(a, html_text(html_nodes(link_nodes, xpath = xPath)))){ #filter the ones without a review
      results_scraping[i,3] = 0
      results_scraping[i,4] = 0
      Sys.sleep(1)
      
    } else {
      
      results_scraping[i,3] = html_text(html_nodes(link_nodes, xpath = xPath)) #filter the review data out of the html document
      results_scraping[i,4] = html_text(html_nodes(link_nodes_number, xpath = x_Path_rev_number))
      Sys.sleep(1)
    }
    
  }
  
  end.time <- Sys.time()
  (time_parallel_computation = end.time - start.time)
  return(results_scraping)
}
results_scraping = webscraping(scraping_p_vector)
results_scraping[,4] = as.numeric(str_remove(results_scraping[,4], "Google reviews" )) #removes "google reviews" string +  converts to numeric
results_scraping[,3] = as.numeric(results_scraping[,3]) #converts class character to numeric
#Saving the Review-Data in csv
#write.csv(review, file="1000 with name-street-new-york.csv")


length(which(results_scraping[,3] == 0))/length(scraping_p_vector)
#1. 0.3837772 % keine Bewertung

myclient$close()
rD$server$stop()

