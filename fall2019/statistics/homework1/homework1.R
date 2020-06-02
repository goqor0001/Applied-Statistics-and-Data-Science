print("first three observations of mtcars")
head(mtcars, 3)
print("last three observations of mtcars")
tail(mtcars, 3)

print("car with max horsepower")
mtcars[which.max(mtcars$hp),]

print("frequency of table with number of gears")
barplot(table(mtcars$gear)/nrow(mtcars))

print("barplot of gears")
barplot(mtcars$gear)

print("Line Graph of gear variable")
barplot(table(mtcars$gear))
plot(table(mtcars$gear))

print("Polygon Plot of gear variable")
plot(table(mtcars$gear), type="l")
