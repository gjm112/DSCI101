####################################
#Basics of R
####################################

#Big note!
# R starts indexing at 1 NOT ZERO!!!!!
# Case MaTtErs!

####################################
#Assignment
####################################
#Preferred syntax
height <- 442

#These also work
442 -> height
height = 442

print(height)
#don't need print in R
height

####################################
#Comments
####################################
#This is how you make a comment!

####################################
#Variables
####################################
#Default to numeric
height <- 442
class(as.integer(height))

height <- 442.0
class(height)

#Character
height <- "really tall"
class(height)

#Can force it to be an integer
height <- as.integer(442)
class(height)
object.size(height)


####################################
#R is case sensitive
####################################
#These are all different objects
name <- "greg" 
Name <- "steve"
NAME <- "mary"

####################################
#Loops and conditional statements
####################################
#Note: x += 1 doesn't work in R!

####################################
#While loops
####################################
#Indentations don't matter!
#But they sure do look nicer!
x <- 1
while (x <= 100){
  x <- x + 1
  print(x)
}
cat("Last x is", x)
print(paste0("Last x is ",x))

#Another example
x <- 1
while (x < 5){
  print("Hello")
  x <- x*2
}

cat("last value of x = ",x)
####################################
#For
####################################
#Consecutive indexes
for (i in 1:10){
  print(i)
}

for (i in 10:1){
  print(i)
}

#Don't need to be consecutive!
for (i in c(2, 3, 5, 7, 11)){
  print(i)
}



####################################
#Conditional Statements
####################################
#If
sky <- "sunny"
if (sky == "sunny"){
  print("Leave your umbrella at home!")
}

sky <- "cloudy"
if (sky == "sunny"){
  print("Leave your umbrella at home!")
}

#Ifelse

sky <- "cellphone"
if (sky == "sunny"){
  print("Leave your umbrella at home!")
} else {
  print("Bring your umbrella")
}

#Nested
sky <- "sunny"
if (sky == "sunny") {
  print("Leave your umbrella at home!")
} else {
  if (sky == "cloudy") {
    print("Bring your umbrella")
  } else {
    if (sky == "snowing") {
      print("Grab your parka")
    } else {
      print("Your guess is as good as mine")
    }
  }
}

#But we can use elseif 
sky <- "dsfsad"
if (sky == "sunny") {
  print("Leave your umbrella at home!")
} else if (sky == "cloudy") {
  print("Bring your umbrella")
} else if (sky == "snowing") {
  print("Grab your parka")
} else {
  print("Your guess is as good as mine")
}
  
#There is also the function ifelse
sky <- "sunny"
#ifelse(test,yes,no)
ifelse(sky == "sunny","Sunny","not sunny")
#No print statement needed!

####################################
#Conditional Statements
####################################
#Booleans
#TRUE and FALSE
#Need to be all capital letters!
a <- TRUE
b <- FALSE
#TRUE is a 1; FALSE is a 0
4 + a

d <- 0
if (d == FALSE){
  print("d is FALSE")
}

#also works
d <- TRUE
if (d){
  print("d is TRUE")
}


#Also works
d <- 1
if (d){
  print("d is TRUE")
}

#Integers
a <- as.integer(37)
class(a)

#Floating Points / Numeric
b <- 37
class(b)

#Complex numbers
#We won't really use these this semester.  Or ever?
z <- 1 + 2i
class(z)


#Operations
#Add
3 + 3
#Subtract
6 - 3
#multiply
4 * 3
#divide
6 / 2
#floored quotient
floor(7 / 2)
#ceiling quotient
ceiling(7 / 2)
#remainder or modulus
7 %% 2
#absolute value
abs(-5)
#Complex numbers
4 + 7i
class(4 + 7i)
#Complex(length,real,imaginary)
#This will recycle as needed
complex(3,4,7:5)
#exponents
3^4
3**4

#Convert to different types
#Convert numeric to integer
a <- 5
class(a)
a
a <- as.integer(a)
class(a)

#Convert character to numberic
#Works if the character is a number
a <- "5"
class(a)
a <- as.numeric(a)
a
class(a)

#Convert a character to a numeric
#This doesn't work
a <- "five"
class(a)
a <- as.numeric(a)
a
class(a)

#Convert a numeric to a character
a <- 5
class(a)
a <- as.character(a)
a
class(a)



#Comparisons
# > Greater than
# < Less than
# >= Greater than or equal
# <= Less than or equal
# != Not equal

a <- 1
b <- 2
c <- 3
if (b >= a & b <= c){
  print("b is between a and c")
}

a <- 1
b <- 2
c <- 3
if (!(b < a | b > c)){
  print("b is between a and c")
}

#floating points numbers are inexact decimal values 
a <- 2.1 + 4.2
a == 6.3
a
class(a)

#Also numeric
a <- 3e3
class(a)

#Opertations
a <- 1/2 
b <- 10
#Square root
sqrt(a)
#Trig
sin(a)
cos(a)
tan(a)
#Log base e
log(a)
#log base b
log(a, b)
#exponetiate
exp(a)

#Functions
#This function will return the larger of the two input numbers
find_max <- function(a, b) {
  if (a > b) {
    return(a)
  } else {
    return(b)
  }
}

find_max(1,2)
find_max(5,3)


#Exercise!
#Write a function that will compute the sum of n terms of the alternating sequence from i = 1 to n  (-1)^(i+1)/i





