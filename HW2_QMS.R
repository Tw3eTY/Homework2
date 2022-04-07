## 1st problem ----
Bet <- 1
Money <- 100
while(Money > 0){
  cointoss <- sample (c("win", "loss"), 1, prob = c(0.486,0.514))
  if (cointoss =='win'){
    Money <- Money + Bet
    Bet <- 1
  } else {
  Money <- Money - Bet
  Bet <- min(Money, Bet*2)
  }
  print(Money)
}


## 5.2.4 Exercises ----
## I. Find all flights that 
library(nycflights13)
library(tidyverse)
## 1.Had an arrival delay of two or more hours

(first <- filter(flights, arr_delay >= 120))

## 2.Flew to Houston (IAH or HOU)

second <- filter(flights, dest == "HOU" | dest == "IAH")
  
## 3.Were operated by United, American, or Delta

third <-filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
## *Not sure if that's what was required*

## 4.Departed in summer (July, August, and September)

fourth <- filter(flights, month == 7 | month == 8 | month == 9)

## 5.Arrived more than two hours late, but didn't leave late

fifth <- filter(flights, arr_delay > 120 & dep_delay <= 0)

## 6.Were delayed by at least an hour, but made up over 30 minutes in flight

sixth <- filter(flights, sched_dep_time + 60 <= dep_time & (sched_arr_time - sched_dep_time) > (arr_time - dep_time) + 30)

## 7.Departed between midnight and 6am (inclusive)

seventh <- filter(flights, between(dep_time, 0, 360))
?between
##II. Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
  
##III.  How many flights have a missing dep_time? What other variables are missing? What might these rows represent?

MDT <- filter(flights, dep_time == 'NA')
  
##IV.  Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)


## 5.3.1 Exercises ----
## 1. How could you use arrange() to sort all missing values to the start?
## (Hint: use is.na()).

eight <- arrange(flights, desc(is.na(dep_time)))
                 
## 2.Sort flights to find the most delayed flights. Find the flights that left earliest.

nineth <- arrange(flights, desc(dep_delay))

tenth <- arrange(flights, dep_time)

## 3.Sort flights to find the fastest (highest speed) flights.

eleventh <- arrange(flights, desc(distance/air_time))

## 4.Which flights travelled the farthest? Which travelled the shortest?

twelfth <- arrange(flights, desc(distance))
## The ones flying from JFK to HNL
thirteenth <- arrange(flights, distance)
## The shortest one is EWR to LGA once, otherwise EWR to PHL, reoccuring
## 5.4.1 Exercises ----
## 1.Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.

one <- select(flights, dep_time, dep_delay, arr_time, arr_delay)
two <- select(flights, (dep_time:arr_delay, -(sched_dep_time, sched_arr_time)))

## 2.What happens if you include the name of a variable multiple times in a select() call?
  
three <- select(flights, dep_time, arr_delay, dep_time, arr_time, dep_time)
## It selects the variable only once

## 3.What does the any_of() function do? Why might it be helpful in conjunction with this vector?
  
  vars <- c("year", "month", "day", "dep_delay", "arr_delay")
  four <- select(flights, any_of(vars))
## any_of ignores any missing data in the give columns, this doesn't remove it (Google)

## 4.Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
  
  select(flights, contains("TIME"))
  

  
## 5.5.2 Exercises ----
##Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they're not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
  
  mutate(flights, dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100))
  ## Didn't understand the question, looked it up in google
  
##Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
  
   transmute(flights, air_time, timeinair = arr_time - dep_time)
   ## Don't know how to fix
  
##Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
   
   ## I expect dep_time = sched_dep_time + dep_delay
   transmute(flights, dep_time, dep_time2 = sched_dep_time + dep_delay)
    
##Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
  
   filter(flights, min_rank(desc(dep_delay))<=10)
   ##Why doesn't this work
   
##What does 1:3 + 1:10 return? Why?
   
1:3 + 1:10   
# It produces 10 lenght vector ( 2  4  6  5  7  9  8 10 12 11 ) and a warning message "In 1:3 + 1:10 : longer object length is not a multiple of shorter object length"
    
##What trigonometric functions does R provide?
   
   ?Trig
   
   ## It provides cos(x)
#   sin(x)
#   tan(x)
   
#   acos(x)
#   asin(x)
#   atan(x)
#   atan2(y, x)
   
#   cospi(x)
#   sinpi(x)
#   tanpi(x)
   