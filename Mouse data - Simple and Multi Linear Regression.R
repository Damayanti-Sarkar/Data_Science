# Here's the data
mouse.data <- data.frame(
  size = c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
  weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

mouse.data

# Let's start by reviewing simple regression
# by modeling mouse size with mouse weight.

#STEP 1: Draw a graph of the data to make sure the relationship make sense
plot(mouse.data$weight, mouse.data$size, pch=16, cex=2)

# STEP 2: Do the regression
simple.regression <- lm(size ~ weight, data=mouse.data)

# STEP 3: Look at the values
summary(simple.regression)

abline(simple.regression, lwd=5, col="red")

# Now apply our formula
ss.mean <- sum((mouse.data$size - mean(mouse.data$size))^2)
ss.simple <- sum(simple.regression$residuals^2)

(ss.mean - ss.simple) / ss.mean 


f.simple <- ((ss.mean - ss.simple) / (2 - 1)) / 
  (ss.simple / (nrow(mouse.data) - 2))

f.simple

# Now let's draw a distribution curve 
x <- seq(from=0, to=15, by=0.1)
y <- df(x, df1=1, df2=7)
plot(x, y, type="l")

# Now draw a verticle line for this test
abline(v=f.simple, col="red")

# color the graph on the left side of the line blue
x.zero.to.line <- seq(from=0, to=f.simple, by=0.1)
y.zero.to.line <- df(x.zero.to.line, df1=1, df2=7)
polygon(x=c(x.zero.to.line, 0), y=c(y.zero.to.line, 0), col="blue")

# color the graph on the right side of the line red
x.line.to.20 <- seq(from=f.simple, to=20, by=0.1)
y.line.to.20 <- df(x.line.to.20, df1=1, df2=7)
polygon(x=c(x.line.to.20, f.simple), y=c(y.line.to.20, 0), col="red")

pf(f.simple, df1=1, df2=7) # the area under the curve that is blue

1-pf(f.simple, df1=1, df2=7) # the area under the curve that is red

# lastly, let's compare this with the original regression
summary(simple.regression)


# Now let's do multiple regression by adding an extra term, tail length

# STEP 1: Draw a graph of the data to make sure the relationship make sense
# This graph is more complex because it shows the relationships between all
# of the columns in "mouse.data".
plot(mouse.data)

# STEP 2: Do the regression
multiple.regression <- lm(size ~ weight + tail, data=mouse.data)

# STEP 3: Look at the values
summary(multiple.regression)


ss.multiple <- sum(multiple.regression$residuals^2)

(ss.mean - ss.multiple) / ss.mean

f.multiple <- ((ss.mean - ss.multiple) / (3 - 1)) / 
  (ss.multiple / (nrow(mouse.data) - 3))

f.multiple  

# Draw the correct distribution curve
x <- seq(from=0, to=20, by=0.1)
y <- df(x, df1=2, df2=6)
plot(x, y, type="l")

# Now draw a verticle line for this test
abline(v=f.multiple, col="red")

# color the graph on the left side of the line blue
x.zero.to.line <- seq(from=0, to=f.multiple, by=0.1)
y.zero.to.line <- df(x.zero.to.line, df1=2, df2=6)
polygon(x=c(x.zero.to.line, 0), y=c(y.zero.to.line, 0), col="blue")

# color the graph on the right side of the line red
x.line.to.20 <- seq(from=f.multiple, to=20, by=0.1)
y.line.to.20 <- df(x.line.to.20, df1=2, df2=6)
polygon(x=c(x.line.to.20, f.multiple), y=c(y.line.to.20, 0), col="red")


pf(f.multiple, df1=2, df2=6) ## the area under the curve that is blue

1-pf(f.multiple, df1=2, df2=6) ## the area under the curve that is red

# let's compare this with original regression
summary(multiple.regression)
 
# For the tail data calculations

f.simple.v.multiple <- ((ss.simple - ss.multiple) / (3-2)) / 
  (ss.multiple / (nrow(mouse.data) - 3))

1-pf(f.simple.v.multiple, df1=1, df2=6)

summary(multiple.regression)

# Including the tail data it makes a significant difference in this multi linear regression with compared to the simple linear regression. 
