# This is file setup.R which defines generic functions for the zebra
# and einstein puzzles.  It is intended to be called (via "source()")
# by zebra.R or einstein.R.


library("abind")

constraint <- function(v){write(c(v,0),file="zebra.cnf",ncolumns=1000,append=TRUE)}


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
    constraint(x)   # it's one of them...
    ind <- which(upper.tri(outer(seq_along(x),seq_along(x))),arr.ind=TRUE)
    for(i in seq_len(nrow(ind))){
        constraint(-x[ind[i,]]) # ... but not more than one
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

for(i in seq_len(5)){
    for(j in seq_len(5)){
        for(k in seq_len(5)){
            constraint(c(-x12[i,j],-x13[i,k],x23[j,k]))
            constraint(c(-x12[i,j],-x14[i,k],x24[j,k]))
            constraint(c(-x12[i,j],-x15[i,k],x25[j,k]))
            constraint(c(-x12[i,j],-x16[i,k],x26[j,k]))
            constraint(c(-x13[i,j],-x14[i,k],x34[j,k]))
            constraint(c(-x13[i,j],-x15[i,k],x35[j,k]))
            constraint(c(-x13[i,j],-x16[i,k],x36[j,k]))
            constraint(c(-x14[i,j],-x15[i,k],x45[j,k]))
            constraint(c(-x14[i,j],-x16[i,k],x46[j,k]))
            constraint(c(-x15[i,j],-x16[i,k],x56[j,k]))
            constraint(c(-x23[i,j],-x24[i,k],x34[j,k]))
            constraint(c(-x23[i,j],-x25[i,k],x35[j,k]))
            constraint(c(-x23[i,j],-x26[i,k],x36[j,k]))
            constraint(c(-x24[i,j],-x25[i,k],x45[j,k]))
            constraint(c(-x24[i,j],-x26[i,k],x46[j,k]))
            constraint(c(-x25[i,j],-x26[i,k],x56[j,k]))
            constraint(c(-x34[i,j],-x35[i,k],x45[j,k]))
            constraint(c(-x34[i,j],-x36[i,k],x46[j,k]))
            constraint(c(-x35[i,j],-x36[i,k],x56[j,k]))
            constraint(c(-x45[i,j],-x46[i,k],x56[j,k]))
        } # k loop closes
    } # j loop closes
}  # i loop closes

next_to_constraint <- function(...){
    M <- as.matrix(expand.grid(as.list(...)))
    for(i in seq_len(nrow(M))){ constraint(M[i,]) }
    return(0)
}
          
