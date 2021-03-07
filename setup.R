# This is file setup.R which defines generic functions for the zebra
# and einstein puzzles.  It is intended to be called (via "source()")
# by zebra.R or einstein.R.

# In this file we define a bunch of functions, and call them at the
# end.

library("abind")

constraint <- function(v){
    write(c(v,0),file=filename,ncolumns=length(v)+1,append=TRUE)
}

desc <- function(n){
    jj <- c(which(A==n,arr.ind=TRUE))
    out <- L[[jj[3]]][jj[1],jj[2],drop=FALSE]
    c(rownames(out),colnames(out))
}

exactly_one <- function(x){
    constraint(x)   # it's one of them...
    ind <- which(upper.tri(outer(seq_along(x),seq_along(x))),arr.ind=TRUE)
    for(i in seq_len(nrow(ind))){
        constraint(-x[ind[i,]]) # ... but not more than one
    }
    return(0)
}

logical_implication <- function(x){ # A & B -> C
#    A <- x[1]
#    B <- x[2]
#    C <- x[3]
    constraint(c(-x[1],-x[2],x[3]))
    return(0)
}

next_to_constraint <- function(...){
    M <- as.matrix(expand.grid(...))
    for(i in seq_len(nrow(M))){ constraint(M[i,]) }
    return(0)
}
          
show <- function(select, filename = "zebra_solution.txt"){
    a <- as.vector(unlist(read.table(filename)[-1]))
    out <- list()
    a <- a[a>0]
    out <- matrix("",length(a),2)
    for(i in seq_along(a)){
        out[i,] <- desc(a[i])
    }
    if(!missing(select)){
        out <- out[apply(out,1,function(p){select %in% p}),]
        out <- noquote(apply(out,1,function(x){x[x!=select]}))
    }
    return(out)
}
    

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


## in the above lines, we create 15 variables with names "xmn",
## m=12345,n=123456, m<n.  These variables are 5x5 matrices and xmn
## has rows corresponding to variable 'm' and columns corresponding to
## variable 'n' [see zebra.R or einstein.R].  Taking zebra.R as an
## example, we have x35 has rows for nationality (variable number 3)
## and columns for drink (variable number 5).

## > x35
##            drink
## nationality tea coffee milk orange water
##   english   251    256  261    266   271
##   norwegian 252    257  262    267   272
##   spanish   253    258  263    268   273
##   ukranian  254    259  264    269   274
##   japanese  255    260  265    270   275
## > 


## Taking the top-left entry, x35[1,1] as an example, this is equal to
## 251.  So in the resulting .cnf file (zebra.cnf in the case of
## zebra.R), variable 251 corresponds to the truth of proposition
## "the englishman drinks tea", and 269 corresponds to the ukranian
## drinking orange.

A <- abind(
    x12, x13, x14, x15, x16,
         x23, x24, x25, x26,
              x34, x35, x36,
                   x45, x46,
                        x56,
    along=3)


L <- list(
    x12=x12, x13=x13, x14=x14, x15=x15, x16=x16,
             x23=x23, x24=x24, x25=x25, x26=x26,
                      x34=x34, x35=x35, x36=x36,
                               x45=x45, x46=x46,
                                        x56=x56
)

dimnames(A) <-
    list(NULL,NULL,
         pair=
             c("x12", "x13", "x14", "x15", "x16",
                      "x23", "x24", "x25", "x26",
                             "x34", "x35", "x36",
                                    "x45", "x46",
                                           "x56"
               ))

for(i in seq_along(L)){
    m <- L[[i]]
    for(j in seq_len(nrow(m))){
        exactly_one(m[j,])
        exactly_one(m[,j])
    }
}

for(i in seq_len(5)){
    for(j in seq_len(5)){
        for(k in seq_len(5)){
            logical_implication(c(x12[i,j],x13[i,k],x23[j,k]))
            logical_implication(c(x12[i,j],x14[i,k],x24[j,k]))
            logical_implication(c(x12[i,j],x15[i,k],x25[j,k]))
            logical_implication(c(x12[i,j],x16[i,k],x26[j,k]))
            logical_implication(c(x13[i,j],x14[i,k],x34[j,k]))
            logical_implication(c(x13[i,j],x15[i,k],x35[j,k]))
            logical_implication(c(x13[i,j],x16[i,k],x36[j,k]))
            logical_implication(c(x14[i,j],x15[i,k],x45[j,k]))
            logical_implication(c(x14[i,j],x16[i,k],x46[j,k]))
            logical_implication(c(x15[i,j],x16[i,k],x56[j,k]))
            logical_implication(c(x23[i,j],x24[i,k],x34[j,k]))
            logical_implication(c(x23[i,j],x25[i,k],x35[j,k]))
            logical_implication(c(x23[i,j],x26[i,k],x36[j,k]))
            logical_implication(c(x24[i,j],x25[i,k],x45[j,k]))
            logical_implication(c(x24[i,j],x26[i,k],x46[j,k]))
            logical_implication(c(x25[i,j],x26[i,k],x56[j,k]))
            logical_implication(c(x34[i,j],x35[i,k],x45[j,k]))
            logical_implication(c(x34[i,j],x36[i,k],x46[j,k]))
            logical_implication(c(x35[i,j],x36[i,k],x56[j,k]))
            logical_implication(c(x45[i,j],x46[i,k],x56[j,k]))
        } # k loop closes
    } # j loop closes
}  # i loop closes

