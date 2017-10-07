make.terrain <- function(n){
    #length of sides (always odd)
    #will has n^2+1, but this doesn't always result in odd numbers, 
    #which makes diamond step hard
    sides <- (2^n)+1
    terrain <- matrix(nrow=sides, ncol=sides)
    rand.corner <- rnorm(4,20,35)
    terrain[1,1] <- rand.corner[1]
    terrain[sides,1] <- rand.corner[2]
    terrain[sides,sides] <- rand.corner[3]
    terrain[1,sides] <- rand.corner[4]
    return(terrain)
}

# Define the functions we'll use
diamond.step <- function(mat,sd){
    sides <- sqrt(length(mat))
    corners <- c(mat[1,sides],mat[1,1],mat[sides,sides],mat[sides,1])
    corner.mean <- mean(corners) + rnorm(1,0,sd)
    mid.col <- median(1:ncol(mat))
    mat[mid.col,mid.col] <- corner.mean
    return(mat)
}
square.step <- function(mat,sd){
    sides <- ncol(mat)
    row.center <- median(1:sides)
    
    top.center <- c(mat[1,1],mat[1,sides], mat[row.center, row.center])
    mat[1,row.center] <- mean(top.center) + rnorm(1,0,sd)
   
    bottom.center <- c(mat[sides,1],mat[sides,sides], mat[row.center, row.center]) 
    mat[sides, row.center] <- mean(bottom.center) + rnorm(1,0,sd)

    left.center <- c(mat[sides,1],mat[1,1], mat[row.center, row.center]) 
    mat[row.center, 1] <- mean(left.center) + rnorm(1,0,sd)

    right.center <- c(mat[1,sides],mat[sides,sides], mat[row.center, row.center]) 
    mat[row.center, sides] <- mean(right.center) + rnorm(1,0,sd)
    return(mat)
}

# Put it all together
diamond.square.step <- function(n,sd){
    terrain <- make.terrain(n)
    terrain.size <- ncol(terrain)-1
    for (i in 2^(n: 1)){
        spacing <- seq(1, terrain.size, by=i)
        for(j in spacing){
            for(k in spacing){
                #cat(k,":",(k+i), "    ", j,":",(j+i),"\n")
                terrain[k:(k+i),j:(j+i)] <- diamond.step(terrain[k:(k+i),j:(j+i)],sd)
                terrain[k:(k+i),j:(j+i)] <- square.step(terrain[k:(k+i),j:(j+i)],sd)
            }
        }
        sd <- sd * .25
    }
        return(terrain) 
}

terry <- diamond.square.step(6,20)
image(terry,col=terrain.colors(3))
dev.off()