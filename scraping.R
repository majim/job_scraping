#https://thatdatatho.com/web-scraping-indeed-jobs-r-rvest/

library(tidyverse)
library(rvest)
library(xml2)


##################              full code     ####
# we have to implement in our scraper are multiple page results.
# We need to look at the # of pages manually

page_result_start <- 0 # starting page 
page_result_end <- 40 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
full_df <- data.frame()
for(i in seq_along(page_results)) {
    #select in indeed the title and area
    first_page_url <- "https://es.indeed.com/jobs?q=%22commercial+operations%22+OR+%22sales+operations%22&l=Madrid"
    url <- paste0(first_page_url, "&start=", page_results[i])
    #parse the page: read in the code from the webpage and break it down into different elements (<div>, <span>, <p>, etc.) for you to analyse it.
    page_full <- xml2::read_html(url)    
     # Sys.sleep pauses R for random number of seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    sleep <- sample(3:8, 1)
    Sys.sleep(sleep)
    ####   STEP BY STEP EXPLANATION HERE, FULL CODE BELOW 
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
    job_title <- page_full %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
        rvest::html_attr("title")

    #get the comapny name using CSS node
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
    # Lastly, we want to get the job description from every single job on the website. 
    # You’ll notice, that on the current page, there is just a little meta description of the 
    # job summary. However, we want to get the full description of how many years of experience 
    # we need, what skill set is required, and what responsibilities the job entails.
    # In order to do that we have to collect the links on the website
    
    # get links xpath
    links <- page_full %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
        rvest::html_attr("href")
    #we now need to inspect the offer content, by clicking in the link
    # get job description xpath   
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
    job_description <-unlist(job_description)
    df <- data.frame(job_title, company_name, job_location, links, job_description)
    full_df <- rbind(full_df, df)
}

write.csv (full_df, "commops_madrid_june_21.csv", eol = "\r")


# create a test DB
test_df <- full_df


### TEXT EXTRACTION ##########

full_df_analysis <- full_df %>% 
    mutate(english = 
               stringr::str_extract(full_df,
                                    regex("(?<=Amadeus\\swill\\spay).+?(?=in\\scontrast)", dotall = TRUE, ignore_case = TRUE))) %>% 
    mutate(bonus = stringr::str_extract(bonus,"(€|EUR)?\\s?[0-9]+(,|\\.)[0-9]+\\s?(€|EUR)?")) %>% 
    mutate(bonus = stringr::str_squish(bonus)) %>% 
    mutate(bonus= replace_na(bonus, "No signature bonus string"))