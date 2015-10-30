# R-course
x <- c(4,7,NA, 7)
y <- c(5,7,2,9,5,9,NA,6,9)

# compute sample variance removing missing values
computeSampleVar <- function(x, na.rm = TRUE){
  if(na.rm==TRUE){
    x <- na.omit(x) # version 1
    x <- x[! x %in% NA] # version 2 
    x <- x[!is.na(x)] # version 3
    return()
  }
}

computeSampleVar <- function(x, na.rm = TRUE){
  if(na.rm==TRUE){
    x <- na.omit(x)
    sv <- sum((x-mean(x))^2)/length(x)
    return(sv)
  } else {
   if (any(is.na(x))) {
     warning ("can´t compute sample var b/c of NAs")
     return(NA)
   }else{
     sv <- sum((x-mean(x))^2)/length(x)
     return(sv)
   }
  }
}

computeSampleVar(x)

x[4] <- NA

# binomial test: 
# if participant had a tendency to yes-answers
binom.test(x=c(23,15))

goodVP <- function(x){
  if(binom.test(table(x)) $p.value < .05){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

test <- sample(c("yes", "no"), size=30, replace=TRUE)
table(test) # useful only if you have a factor 
            # or sthg similar to a factor

load("simdata.rData")

# decide for each VP
sim$keep <- ave(sim$answer, sim$vp, FUN=goodVP)
sim <- subset(sim, sim$keep == TRUE)

# version2: tapply

# scope of variables
x1 <- 1

test <- function(){
  print(x1) # global environment
  x1 <- 3 # create new val in local env w same name
  print(x1)
  
  a <- 4 # create new val in local env w different name
  print(a)
  
  return("shalala")
}

#everything you want to keep: return it!!

print(x1)
test()



