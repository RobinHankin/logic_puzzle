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


library("abind")

filename <- "zebra.cnf"
write("# comment line",file=filename,append=FALSE)
condition <- function(v){write(c(v,0),file="zebra.cnf",ncolumns=1000,append=TRUE)}


# Einstein puzzle:
colour <- c("red","green","white","yellow","blue")               # 1
number <- c("one","two","three","four","five")                   # 2
nationality <- c("Brit","Norwegian","Swede","Dane","German")     # 3
pet <- c("dogs","birds","cats","horses","fish")                  # 4
drink <- c("tea","coffee","milk","beer","water")                 # 5
smoke <- c("Pall Mall","Dunhill","Blends","BlueMaster","Prince") # 6

# zebra puzzle:
colour      <- c("red","green","white","yellow","blue")                   # 1
number      <- c("one","two","three","four","five")                        # 2
nationality <- c("english","norwegian","spanish","ukranian","japanese")     # 3
pet         <- c("dogs","snails","fox","horse","zebra")                      # 4
drink       <- c("tea","coffee","milk","orange","water")                      # 5
smoke       <- c("parliaments","oldgold","kools","luckystrike","chesterfield") # 6

index <- c(colour=1, number=2, nationality=3,pet=4,drink=5,smoke=6)


f <- 1:5  # "five"
tf <- 1:25


f <- function(n,d){matrix(25*(n-1) + 1:25, 5,5,dimnames=d)}

x12 <- f(1, list(colour=colour, number=number))
x13 <- f(2, list(colour=colour, nationality=nationality))
x14 <- f(3, list(colour=colour, pet=pet))
x15 <- f(4, list(colour=colour, drink=drink))
x16 <- f(5, list(colour=colour, smoke=smoke))

x23 <- f(6, list(number=number, nationality=nationality))
x24 <- f(7, list(number=number, pet=pet))
x25 <- f(8, list(number=number, drink=drink))
x26 <- f(9, list(number=number, smoke=smoke))

x34 <- f(10, list(nationality=nationality, pet=pet))
x35 <- f(11, list(nationality=nationality, drink=drink))
x36 <- f(12, list(nationality=nationality, smoke=smoke))

x45 <- f(13, list(pet=pet, drink=drink))
x46 <- f(14, list(pet=pet, smoke=smoke))

x56 <- f(15, list(drink=drink, smoke=smoke))

A <- abind(x12, x13, x14, x15, x16, x23, x24, x25, x26, x34, x35, x36, x45, x46, x56,along=3)
L <- list(
    x12=x12, x13=x13, x14=x14, x15=x15, x16=x16,
    x23=x23, x24=x24, x25=x25, x26=x26, x34=x34,
    x35=x35, x36=x36, x45=x45, x46=x46, x56=x56
)

dimnames(A) <-
    list(NULL,NULL,pair=c("x12", "x13", "x14", "x15", "x16", "x23",
                "x24", "x25", "x26", "x34", "x35", "x36",
                "x45", "x46", "x56"))


desc <- function(n){
    jj <- c(which(A==n,arr.ind=TRUE))
    as.vector(unlist(dimnames(L[[jj[3]]][jj[1],jj[2],drop=FALSE])))
}

exactly_one <- function(x){
    condition(x)   # it's one of them...
    ind <- which(upper.tri(outer(seq_along(x),seq_along(x))),arr.ind=TRUE)
    for(i in seq_len(nrow(ind))){
        condition(-x[ind[i,]]) # ... but not more than one
    }
    return(0)
}

for(i in seq_along(L)){
    m <- L[[i]]
    for(i in seq_along(nrow(m))){
        exactly_one(m[i,])
        exactly_one(m[,i])
    }
}

for(i in seq_len(nrow(m1))){
    for(j in seq_len(nrow(m1))){
        for(k in seq_len(nrow(m1))){
            condition(c(-x12[i,j],-x13[i,k],x23[j,k]))
            condition(c(-x12[i,j],-x14[i,k],x24[j,k]))
            condition(c(-x12[i,j],-x15[i,k],x25[j,k]))
            condition(c(-x12[i,j],-x16[i,k],x26[j,k]))
            condition(c(-x13[i,j],-x14[i,k],x34[j,k]))
            condition(c(-x13[i,j],-x15[i,k],x35[j,k]))
            condition(c(-x13[i,j],-x16[i,k],x36[j,k]))
            condition(c(-x14[i,j],-x15[i,k],x45[j,k]))
            condition(c(-x14[i,j],-x16[i,k],x46[j,k]))
            condition(c(-x15[i,j],-x16[i,k],x56[j,k]))
            condition(c(-x23[i,j],-x24[i,k],x34[j,k]))
            condition(c(-x23[i,j],-x25[i,k],x35[j,k]))
            condition(c(-x23[i,j],-x26[i,k],x36[j,k]))
            condition(c(-x24[i,j],-x25[i,k],x45[j,k]))
            condition(c(-x24[i,j],-x26[i,k],x46[j,k]))
            condition(c(-x25[i,j],-x26[i,k],x56[j,k]))
            condition(c(-x34[i,j],-x35[i,k],x45[j,k]))
            condition(c(-x34[i,j],-x36[i,k],x46[j,k]))
            condition(c(-x35[i,j],-x36[i,k],x56[j,k]))
            condition(c(-x45[i,j],-x46[i,k],x56[j,k]))
        } # k loop closes
    } # j loop closes
}  # i loop closes


# The Englishman lives in the red house: colour = #1[red=1], nationality = #3[Brit=1]
constraint(x13[1, 1])
 
# The Swede/spaniard keeps dogs as pets; nationality = #3[swede=3], pet= #4[dog=1]
constraint(x34[3, 1])
 
# The Dane/ukranian drinks tea; nationality = #3[Dane=4], drink = #5[tea=1]
constraint(x35[4, 1])

# The green house is on the left of the white house: colour = #1[green=2, white=3], number = #2
# that is, *exactly one* of the following eight lines must be true:

## (x12[2,1] & x12[3,2])    green@#1, white@#2
## (x12[2,2] & x12[3,1])    green@#2, white@#1
## (x12[2,2] & x12[3,3])    green@#2, white@#3
## (x12[2,3] & x12[3,2])    green@#3, white@#2
## (x12[2,3] & x12[3,4])    green@#3, white@#4
## (x12[2,4] & x12[3,3])    green@#4, white@#3
## (x12[2,4] & x12[3,5])    green@#4, white@#5
## (x12[2,5] & x12[3,4])    green@#5, white@#4

M <- as.matrix(expand.grid(
    c(x12[2,1] & x12[3,2]),
    c(x12[2,2] & x12[3,1]),
    c(x12[2,2] & x12[3,3]),
    c(x12[2,3] & x12[3,2]),
    c(x12[2,3] & x12[3,4]),
    c(x12[2,4] & x12[3,3]),
    c(x12[2,4] & x12[3,5]),
    c(x12[2,5] & x12[3,4])
))

for(i in seq_len(nrow(M))){ constraint(M[i,]) }


# The green house's owner drinks coffee: colour = #1[green=2], drink = #5[coffee=2]
constraint(x15[2,2])

# The person who smokes Pall Mall/parliaments rears birds/snails: pets = #4[birds=2], smokes = #6[Pall Mall=1]
constraint(x46[2, 1])

# The owner of the yellow house smokes Dunhill/oldgold:  colour = #1[yellow=4], smokes = #6[Dunhill=2]
constraint(x16[4, 3])

# The man living in the center house drinks milk: number = #2[three=3], drink = #5[milk=3]
constraint(x25[3, 3])

# The Norwegian lives in the first house: number = #2[first=1], nationality = #3[Norwegian=2]
constraint(x23[1, 4])
 
# The man who smokes blends lives next to the one who keeps cats:

# number = #2[abs(x-y)==1], pets = #4[cats=3] smokes = #6[blends = 3]
# 
# # So it's *either* (cats@no. 1 and blends@no. 2) or (cats@no. 2 and blends@ no. 1) 
# #         *or*     (cats@no. 2 and blends@no. 3) or (cats@no. 3 and blends@ no. 2) 
# #         *or*     (cats@no. 3 and blends@no. 4) or (cats@no. 4 and blends@ no. 3) 
# #         *or*     (cats@no. 4 and blends@no. 5) or (cats@no. 5 and blends@ no. 4) 
# #         *or*     (cats@no. 5 and blends@no. 6) or (cats@no. 6 and blends@ no. 5) 
# 
# 
# # set number_of_cat_keeper  = sum{i in 1..6} i*x24[i , 3] ; 
# # var number_of_blend_smoker = sum{i in 1..6} i*x26[i , 3] ; 
# 
# 
# 
# #  add_constraint(
# #  x24[1,3]*x26[2,3] + x24[2,3]*x26[1,3] +
# #  x24[2,3]*x26[3,3] + x24[3,3]*x26[2,3] +
# #  x24[3,3]*x26[4,3] + x24[4,3]*x26[3,3] +
# #  x24[4,3]*x26[5,3] + x24[5,3]*x26[4,3] +
# #  x24[5,3]*x26[6,3] + x24[6,3]*x26[5,3] >= 1)
# 
# 
# 
# 
