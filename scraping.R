#https://thatdatatho.com/web-scraping-indeed-jobs-r-rvest/

library(tidyverse)
library(rvest)
library(xml2)

#select in indeed the title and area
url <- "https://es.indeed.com/jobs?q=commercial+operations&l=Madrid"

#parse the page: read in the code from the webpage and break it down into different elements (<div>, <span>, <p>, etc.) for you to analyse it.
page <- xml2::read_html(url)

#now, righ click on the web page and select "inspect"to see the elements we want to scrap
# Xpath: a path to specificly extract certain parts from a tree-structured document such as XML or HTML.
# CSS Selectors:  similar function to xpath, locating certain nodes in a document and extracting information from these nodes. 
# Every CSS selector can be translated into an equivalent xpath but not the other way around.

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
job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>%
    rvest::html_attr("title")

#get the job title using CSS node
page %>% 
    rvest::html_nodes('[data-tn-element="jobTitle"]') %>%
    rvest::html_attr("title")

# We can see that company location and name are located in the <span> element with a 
# class attribute value of location and company respectively.
# get company location
job_location <- page %>% 
    rvest::html_nodes("span") %>% 
    rvest::html_nodes(xpath = '//*[@class="location accessible-contrast-color-location"]')%>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()


# get company name
company_name <- page %>% 
    rvest::html_nodes("span")  %>% 
    rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()


# Lastly, we want to get the job description from every single job on the website. 
# You’ll notice, that on the current page, there is just a little meta description of the 
# job summary. However, we want to get the full description of how many years of experience 
# we need, what skill set is required, and what responsibilities the job entails.
# In order to do that we have to collect the links on the website

# get links xpath
links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")

# get links CSS selectors
page %>% 
    rvest::html_nodes('[data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")

#we now need to inspect the offer content, by clicking in the link
# get job description xpath

job_description <- c()
for(i in seq_along(links)) {
        url <- paste0("https://es.indeed.com", links[i])
        page <- xml2::read_html(url)
        job_description[[i]] <- page %>%
        rvest::html_nodes("span")  %>% 
        rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
}

df_job <- data.frame(job_title, company_name, job_location, job_description)
library(httr)
url <-"https://es.indeed.com/ofertas?q=commercial+operations&l=Madrid&start=0"
url_test <- GET(url)
status = status_code(url_test)
status

##################full code
#we have to implement in our scraper are multiple page results. We need to look at the results manually


page_result_start <- 0 # starting page 
page_result_end <- 190 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
full_df <- data.frame()
for(i in seq_along(page_results)) {
    
    first_page_url <- "https://es.indeed.com/jobs?q=commercial+operations&l=Madrid"
    url <- paste0(first_page_url, "&start=", page_results[i])
    page_full <- xml2::read_html(url)
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    Sys.sleep(4)
    
    #get the job title
    job_title <- page_full %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
        rvest::html_attr("title")
    
    #get the company name
    company_name <- page_full %>% 
        rvest::html_nodes("span")  %>% 
        rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
    
    #get job location
    job_location <- page_full %>% 
        rvest::html_nodes("span") %>% 
        rvest::html_nodes(xpath = '//*[@class="location accessible-contrast-color-location"]')%>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
    # get links
    links <- page_full %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
        rvest::html_attr("href")
    
    job_description <- c()
    for(i in seq_along(links)) {
        url <- paste0("https://es.indeed.com", links[i])
        page_full <- xml2::read_html(url)
        job_description[[i]] <- page_full %>%
            rvest::html_nodes("span")  %>% 
            rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
            rvest::html_text() %>%
            stringi::stri_trim_both()
        }
    df <- data.frame(job_title, company_name, job_location, job_description)
    full_df <- rbind(full_df, df)
}





