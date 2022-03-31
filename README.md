# Homework2
##Problem1 (Solved in class, redone by me)
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE.

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



##Problem2
##                                                                     5.2.4
##I. Find all flights that

## 1.Had an arrival delay of two or more hours

(first <- filter(flights, arr_delay >= 120))

## 2.Flew to Houston (IAH or HOU)

second <- filter(flights, dest == "HOU" | dest == "IAH")
  
## 3.Were operated by United, American, or Delta

third <-filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
## *Not sure if that's what was required*

## 4.Departed in summer (July, August, and September)

fourth <- filter(flights, month == 7 | month == 8 | month == 9)

## 5.Arrived more than two hours late, but didn’t leave late

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
