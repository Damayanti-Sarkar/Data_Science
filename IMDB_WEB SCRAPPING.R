# INSTALL LIBRARIES

library(robotstxt)
library(rvest)

# CHECK WHETHER THE WEBSITE IS ALLOWED TO SCRAP

Path= paths_allowed("https://www.imdb.com/chart/top/")

# ASSIGN THE WEBSITE TO A VARIABLE

url<-"https://www.imdb.com/chart/top/"

# READ THE HTML PART OF THE URL
my_html<-read_html(url)

# SEGREGATE THE HTML NODES, GET THE TABLES AND ASSIGN THEM TO A VARIABLE

my_tables <- html_nodes(my_html,"table")

# TAKE THE FIRST TABLE AND ASSIGN THEM TO A SEPARATE VARIABLE
movie_table <- html_table(my_tables)[[1]]

# VIEW THE TABLE
View(movie_table)

# TAKE ONLY THE SECOND AND THIRD COLUMN FROM THE TABLE AND STORE IT IN A VARIABLE

movies <- movie_table[,2:3]

# VIEW THE TABLE
View(movies)

# CHECK FOR DATATYPES

str(movies)

# EXPORT THE TABLE TO A CSV FILE IN THE COMPUTER

write.csv(movies,"My_movies.csv")
