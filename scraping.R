rmarkdown::render("scraping.R", "html_document")

#' ---
#' title: "Jobscraping search by position and city"
#' author: "Angeles Jimenez"
#' date: "13 02 2022"
#' ---
#' adapted from https://thatdatatho.com/web-scraping-indeed-jobs-r-rvest/

library(tidyverse)
library(rvest)
library(xml2)


#Defining a sequence with the number of pages to scrap.
# In this proof of concept we look at the # of pages manually

page_result_start <- 0 # starting page 
page_result_end <- 40 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
full_df <- data.frame()

#select job title and city
title <- "Business+analyst" #fill in job title with words separated by "+"
city <- "Madrid"#fill in city
first_page_url <- paste0("https://es.indeed.com/jobs?q=%22",title,"%22&l=",city)

# Function to extract query results (read_html), and create a dataframe with the desired fields. 

for(i in seq_along(page_results)) {
    url <- paste0(first_page_url, "&start=", page_results[i])
    #parse the page: read in the code from the webpage and break it down into different elements (<div>, <span>, <p>, etc.) for you to analyse it.
    page_full <- xml2::read_html(url)    
    # Sys.sleep pauses R for random number of seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    sleep <- sample(3:8, 1)
    Sys.sleep(sleep)

   links <- page_full %>% 
       rvest::html_nodes(xpath = '//a[@id]') %>% 
       rvest::html_attr("href")
   
# remove non relevant links with regex and create variable's placeholder
   links <- grep("^/rc/clk", links, value = T)
 
    job_description <- c()
    job_location <- c()
    company_name <- c()
    job_title <- c()

for(i in seq_along(links)) {
    url <- paste0("https://es.indeed.com", links[i])
    page_full <- xml2::read_html(url)
    job_description[[i]] <- page_full %>%
        rvest::html_nodes("span")  %>% 
        rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
    company_name [[i]]<- page_full %>% 
        rvest::html_nodes("span")  %>% 
        rvest::html_nodes(xpath = '//*[@class="icl-u-lg-mr--sm icl-u-xs-mr--xs"]')  %>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
    job_title [[i]]<- page_full %>% 
        rvest::html_nodes("span") %>%
        rvest::html_nodes(xpath = '//*[@class = "icl-u-xs-mb--xs icl-u-xs-mt--none jobsearch-JobInfoHeader-title"]') %>%
        rvest::html_text() %>%
        stringi::stri_trim_both()
    }
#create the dataframe
    job_description <-unlist(job_description)
    company_name <-unlist(company_name)
    job_title <-unlist(job_title)
    df <- data.frame(job_title, company_name, links, job_description)
    full_df <- rbind(full_df, df)
}

#remove duplicated jobs based on exact description
full_df <- full_df %>% distinct(job_description, .keep_all= TRUE)
###############CODE ENDS HERE


#####

write.csv (full_df, paste0(title,"_",city,"_",Sys.Date(),".csv"), eol = "\r")
getwd()

####   STEP BY STEP EXPLANATION HERE, FULL CODE BELOW 
#now, righ click on the web page and select "inspect"to see the elements we want to scrap
# Xpath: a path to specifically extract certain parts from a tree-structured document such as XML or HTML.
# CSS Selectors:  similar function to xpath, locating certain nodes in a document and extracting information from these nodes. 
# Every CSS selector can be translated into an equivalent xpath but not the other way around.

## \\\
#this is no longer working since the structure of data has changed. 
# example: syntax of a xml path works: //tagname[@attribute = “value“]
# indeed: Here we can see that there is an attribute data-tn-element which value is ”jobTitle“.
#This particular attribute is under the anchor node. So let’s construct the xpath:
# //a[@data-tn-element = “jobTitle“].
# And voila we get all job titles. You’ll notice that we have included //* instead of //a 
#in our code below. The star acts as a wildcard and selects all elements or nodes not just 
# the anchor node. For Indeed’s website, the attribute data-tn-element is always under 
# the anchor node so the wild card symbol wouldn’t be necessary.

#We looked at the source code and identified that the jobtitle is located within the
# anchor <a> and <div> nodes. Then we looked at the attribute data-tn-element with the value “jobTitle“.
# From there, we grabbed the “title” attribute and extracted the information.

#get the job title use xpath
#job_title <- page_full %>% 
#    rvest::html_nodes("div") %>%
#    rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
#    rvest::html_attr("title")

## ///
## /// the hidden code below is now included in the main function to avoid missmatch between data extractions.
#get the job title using CSS node
#job_title <- page_full %>% 
#rvest::html_nodes("span") %>%
#rvest::html_nodes(xpath = '//*[@class = "jobTitle"]') %>%
#rvest::html_text() %>%
#stringi::stri_trim_both()

#get the company name using CSS node
#company_name <- page_full %>% 
#rvest::html_nodes("span")  %>% 
#rvest::html_nodes(xpath = '//*[@class="companyName"]')  %>% 
#rvest::html_text() %>%
#stringi::stri_trim_both()

#get job location
#job_location <- page_full %>% 
#rvest::html_nodes("span") %>% 
#rvest::html_nodes(xpath = '//*[@class="companyLocation"]')%>% 
#rvest::html_text() %>%
#stringi::stri_trim_both()
# Lastly, we want to get the job description from every single job on the website. 
# You’ll notice, that on the current page, there is just a little meta description of the 
# job summary. However, we want to get the full description of how many years of experience 
# we need, what skill set is required, and what responsibilities the job entails.
# In order to do that we have to collect the links on the website

# get links xpath



