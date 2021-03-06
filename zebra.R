#                           The Zebra Problem
#
#    1.  Five people have five different pets, smoke five different
#        brands of cigarettes, have five different favorite drinks and
#        live in five different houses.
#    2.  The Englishman lives in the red house.
#    3.  The Spaniard has a dog.
#    4.  The Ukranian drinks tea.
#    5.  The Norwegian lives in the leftmost house.
#    6.  The Japanese smokes Parliaments.
#    7.  The Norwegian lives next to the blue house.
#    8.  Coffee is drunk in the green house.
#    9.  The snail owner smokes Old Gold.
#    10. The inhabitant of the yellow house smokes Kools.
#    11. The Lucky Strikes smoker drinks orange juice.
#    12. Milk is drunk in the middle house.
#    13. The green house is immediately to the right of the ivory house.
#    14. The Chesterfield smoker lives next door to the fox owner.
#    15. The Kools smoker lives next door to where the horse is kept.
#
#    Given these conditions, determine who owns the zebra and who drinks
#    water.


# zebra puzzle:
colour      <- c("red","green","ivory","yellow","blue")                   # 1
number      <- c("one","two","three","four","five")                        # 2
nationality <- c("english","norwegian","spanish","ukranian","japanese")     # 3
pet         <- c("dogs","snails","fox","horse","zebra")                      # 4
drink       <- c("tea","coffee","milk","orange","water")                      # 5
smoke       <- c("parliaments","oldgold","kools","luckystrike","chesterfield") # 6

index <- c(colour=1, number=2, nationality=3,pet=4,drink=5,smoke=6)


source("setup.R")

filename <- "zebra.cnf"
write("# comment line",file=filename,append=FALSE)





# clue 2: the englishman lives in the red house: colour = #1[red=1], nationality = #3[Brit=1]
constraint(x13[1, 1])

# clue 3: The spaniard has a dog; nationality = #3[spanish=3], pet= #4[dog=1]
constraint(x34[3, 1])

# clue 4: the ukranian drinks tea; nationality = #3[ukranian=4], drink = #5[tea=1]
constraint(x35[4, 1])

# clue 5: the norwegian lives in the first house: number = #2[first=1], nationality = #3[norwegian=2]
constraint(x23[1, 4])

# clue 6: The japanese smokes parliaments: nationality = #3[japanese=5], smokes = #6[parliaments=1]
constraint(x36[5,1])

# clue 7: the norwegian lives next to the blue house: colour = #1[blue=5]; number = #2; nationality = #3[norwegian=2];
# e.g. "norwegian at house number 117" -> x23[117,2] and "house number 118 is blue" -> x12[5,118]
# that is, *exactly one* of the following eight lines must be true:

## (x23[1,2] & x12[5,2])    norwegian@#1, blue@#2
## (x23[2,2] & x12[5,1])    norwegian@#2, blue@#1
## (x23[2,2] & x12[5,3])    norwegian@#2, blue@#3
## (x23[3,2] & x12[5,2])    norwegian@#3, blue@#2
## (x23[3,2] & x12[5,4])    norwegian@#3, blue@#4
## (x23[4,2] & x12[5,3])    norwegian@#4, blue@#3
## (x23[4,2] & x12[5,5])    norwegian@#4, blue@#5
## (x23[5,2] & x12[5,4])    norwegian@#5, blue@#4

# However, because of the regularity constraints we may say that *at
# least* one of the above eight lines must be true [because the
# regularity conditions forbid more than one being true].
next_to_constraint( 
  c(x23[1,2] , x12[5,2]),  # norwegian@#1, blue@#2
  c(x23[2,2] , x12[5,1]),  # norwegian@#2, blue@#1
  c(x23[2,2] , x12[5,3]),  # norwegian@#2, blue@#3
  c(x23[3,2] , x12[5,2]),  # norwegian@#3, blue@#2
  c(x23[3,2] , x12[5,4]),  # norwegian@#3, blue@#4
  c(x23[4,2] , x12[5,3]),  # norwegian@#4, blue@#3
  c(x23[4,2] , x12[5,5]),  # norwegian@#4, blue@#5
  c(x23[5,2] , x12[5,4])   # norwegian@#5, blue@#4
)


#   clue 8:  coffee is drunk in the green house: colour = #1[green=2], drink = #5[coffee=2]
constraint(x15[2,2])

# clue 9:  The snail owner smokes Old Gold: pet = #4[snails=2], smokes = #6[oldgold=2]
constraint(x46[2,2])

# clue 10: The inhabitant of the yellow house smokes Kools:  colour = #1[yellow=4], smokes = #6[kools=3]
constraint(x16[4,3])

# clue 11: The Lucky Strikes smoker drinks orange juice: drink = #5[orange=4], smokes = #6[lucky=4]
constraint(x56[4,4])

# clue 12: Milk is drunk in the middle house: number = #2[three=3], drink = #5[milk=3]
constraint(x25[3,3])



# clue 13: The green house is on the left of the ivory house: colour = #1[green=2, ivory=3], number = #2
# that is, *exactly one* of the following eight lines must be true:

## (x12[2,1] & x12[3,2])    green@#1, ivory@#2
## (x12[2,2] & x12[3,1])    green@#2, ivory@#1
## (x12[2,2] & x12[3,3])    green@#2, ivory@#3
## (x12[2,3] & x12[3,2])    green@#3, ivory@#2
## (x12[2,3] & x12[3,4])    green@#3, ivory@#4
## (x12[2,4] & x12[3,3])    green@#4, ivory@#3
## (x12[2,4] & x12[3,5])    green@#4, ivory@#5
## (x12[2,5] & x12[3,4])    green@#5, ivory@#4

next_to_constraint(
 c(x12[2,1] , x12[3,2]),   # green@#1, ivory@#2
 c(x12[2,2] , x12[3,1]),   # green@#2, ivory@#1
 c(x12[2,2] , x12[3,3]),   # green@#2, ivory@#3
 c(x12[2,3] , x12[3,2]),   # green@#3, ivory@#2
 c(x12[2,3] , x12[3,4]),   # green@#3, ivory@#4
 c(x12[2,4] , x12[3,3]),   # green@#4, ivory@#3
 c(x12[2,4] , x12[3,5]),   # green@#4, ivory@#5
 c(x12[2,5] , x12[3,4]))   # green@#5, ivory@#4



# clue  14: The Chesterfield smoker lives next door to the fox owner: number = #2, pets = #4[fox=3], smokes = #6[chesterfield=5]
# that is, *exactly one* of the following eight lines must be true:

## (x24[1,3] & x26[2,5])    fox@#1, chesterfield@#2
## (x24[2,3] & x26[1,5])    fox@#2, chesterfield@#1
## (x24[2,3] & x26[3,5])    fox@#2, chesterfield@#3
## (x24[3,3] & x26[2,5])    fox@#3, chesterfield@#2
## (x24[3,3] & x26[4,5])    fox@#3, chesterfield@#4
## (x24[4,3] & x26[3,5])    fox@#4, chesterfield@#3
## (x24[4,3] & x26[5,5])    fox@#4, chesterfield@#5
## (x24[5,3] & x26[4,5])    fox@#5, chesterfield@#4
                     
next_to_constraint(  
    c(x24[1,3] & x26[2,5]), #     fox@#1, chesterfield@#2
    c(x24[2,3] & x26[1,5]), #     fox@#2, chesterfield@#1
    c(x24[2,3] & x26[3,5]), #     fox@#2, chesterfield@#3
    c(x24[3,3] & x26[2,5]), #     fox@#3, chesterfield@#2
    c(x24[3,3] & x26[4,5]), #     fox@#3, chesterfield@#4
    c(x24[4,3] & x26[3,5]), #     fox@#4, chesterfield@#3
    c(x24[4,3] & x26[5,5]), #     fox@#4, chesterfield@#5
    c(x24[5,3] & x26[4,5])) #     fox@#5, chesterfield@#4
                       


# clue 15: The Kools smoker lives next door to where the horse is kept. number = #2, pets = #4[horse=4], smokes = #6[kools=3]
# that is, *exactly one* of the following eight lines must be true:

## (x24[1,4] & x26[2,3])    horse@#1, kools@#2
## (x24[2,4] & x26[1,3])    horse@#2, kools@#1
## (x24[2,4] & x26[3,3])    horse@#2, kools@#3
## (x24[3,4] & x26[2,3])    horse@#3, kools@#2
## (x24[3,4] & x26[4,3])    horse@#3, kools@#4
## (x24[4,4] & x26[3,3])    horse@#4, kools@#3
## (x24[4,4] & x26[5,3])    horse@#4, kools@#5
## (x24[5,4] & x26[4,3])    horse@#5, kools@#4

next_to_constraint(
    c(x24[1,4] & x26[2,3]), #     horse@#1, kools@#2
    c(x24[2,4] & x26[1,3]), #     horse@#2, kools@#1
    c(x24[2,4] & x26[3,3]), #     horse@#2, kools@#3
    c(x24[3,4] & x26[2,3]), #     horse@#3, kools@#2
    c(x24[3,4] & x26[4,3]), #     horse@#3, kools@#4
    c(x24[4,4] & x26[3,3]), #     horse@#4, kools@#3
    c(x24[4,4] & x26[5,3]), #     horse@#4, kools@#5
    c(x24[5,4] & x26[4,3])) #     horse@#5, kools@#4

