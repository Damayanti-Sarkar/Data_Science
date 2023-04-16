# IMPORTING LIBRARIES
library(xml2)      #pull html data
library(rvest)     #get html nodes
library(purrr)     #for map functions
library(stringr)   #for str functions

# SCRAPPING WEBSITE
link <- 'https://www.amazon.in/dp/B08V9VMRQF/ref=twister_B09WRGTWRX?_encoding=UTF8&psc=1#customerReviews'

# HTML ELEMENTS FROM WEBSITE
web <- read_html(link)

# SEGREGATING USER NAMES
user_name <- web %>% html_nodes(".a-profile-name") %>% html_text()

# VIEWING USER NAME LIST
View(user_name)

# SEGREGATING MOBILE REVIEW RATING
mobile_rating <- web %>% html_nodes(".review-rating") %>% html_text()

# VIEW MOBILE RATING LIST
View(mobile_rating)

# SEGREGATING REVIEW 
mobile_review <- web %>% html_nodes(".a-expander-partial-collapse-content") %>% html_text() %>%
  str_split("\\n") %>% map_chr(11) %>% str_trim()

# VIEW MOBILE REVIEW LIST
View(mobile_review)

# CREATING DATAFRAME
Reviews <- data.frame(user_name, mobile_rating, mobile_review)

# VIEW DATAFRAME
View(Reviews)
dim (Reviews)
head (Reviews, n=12)

# SAVING DATA
write.csv (Reviews, 'Amazon_reviews.csv')

