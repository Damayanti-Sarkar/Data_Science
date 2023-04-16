# Data visualization
count<-table(mtcars$gear)
View(count)
plot(count)
barplot(count)

barplot(count, horiz=TRUE)

barplot(count, main="Sample bar plot",
        xlab = "Improvement",
        ylab = "Frequency",
        legend = rownames(count),
        col=c("red","yellow","green"))

pie(count)


#Let us try line, scatter and box plot
# Importing Dataset
dataset = read.csv("Runs_Data.csv")
Runs <-read.csv("Runs_Data.csv")
View(Runs)

install.packages("ggplot2")
library(ggplot2)

ggplot(Runs,aes(x=Matches,y=Runs))+
  geom_point()

ggplot(Runs,aes(x=Matches,y=Runs,))+
  geom_line()

ggplot(Runs,aes(x=Matches,y=Runs))+
  geom_boxplot()


#Let's analyze placement scenario
#Download Placement Dataset
#Set the folder as working directory
Placement<-read.csv("Placement_Data.csv",stringsAsFactors = FALSE)
View(Placement)

# Analyze the dataset
str(Placement)

#Install GGPLOT2 library (Grammar of Graphics)

Placement$gender<-as.factor(Placement$gender)
Placement$degree_t<-as.factor(Placement$degree_t)
Placement$status<-as.factor(Placement$status)
Placement$hsc_s<-as.factor(Placement$hsc_s) 
str(Placement)


#Let's first understand the amount of students placed during Placement
prop.table(table(Placement$status))

ggplot(Placement,aes(x=status))+
  geom_bar()

ggplot(Placement,aes(x=status))+
  theme_classic()+
  geom_bar()+
  labs(y="Students count",
       title = "Amount of students Placed during Placement")


#Gender wise placement
ggplot(Placement,aes(x=gender,fill=status))+
  theme_light()+
  geom_bar()+
  labs(y="Students count",
       title = "Placement Count by gender")


#Placement based on degree_t
ggplot(Placement,aes(x=degree_t,fill=status))+
  theme_light()+
  geom_bar()+
  labs(y="Students count",
       title = "Placement Count by degree_t")


#Faceting Data
ggplot(Placement,aes(x=gender,fill=status))+
  theme_light()+
  facet_grid(~degree_t)+
  geom_bar()+
  labs(y="Students count",
       title = "Placement Count by degree_t and gender")


#Trying a pie chart
ggplot(Placement, aes( x ="", fill = status ) ) +
  geom_bar( position="fill" ) +
  facet_grid(~degree_t) +
  coord_polar( theta = "y" )


#Lets see the salary group who got placed or not 
ggplot(Placement,aes(x=salary))+
  theme_bw()+
  geom_histogram(binwidth = 200)+
  labs(y="Students count",
       x="salary",
       title = "Placement salary distribution")


ggplot(Placement,aes(x=salary,fill = status))+
  theme_bw()+
  geom_histogram(binwidth = 200)+
  labs(y="Students count",
       x="salary",
       title = "Placement salary distribution")


ggplot(Placement,aes(x=status,y = salary))+
  theme_bw()+
  geom_boxplot()+
  labs(y="salary",
       x="status",
       title = "Placement status rate by salary")


#facet and density plot
ggplot(Placement,aes(x=salary,fill=status))+
  theme_bw()+
  facet_wrap(gender~degree_t)+
  geom_density(alpha=1.5)+
  labs(y="salary",
       x="status",
       title = "Placement status rate by salary, degree_t and gender")

