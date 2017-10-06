make.terrain <- function(n){
    #length of sides (always odd)
    #will has n^2+1, but this doesn't always result in odd numbers, 
    #which makes diamond step hard
    dim <- (2^n)+1
    terrain <- matrix(nrow=dim, ncol=dim)
    rand.corner <- abs(rnorm(4,0,10))
    #subset the corners and give heights (there has to be a cool guy way to do this)
    terrain[1,1] <- rand.corner[1]
    terrain[dim,1] <- rand.corner[2]
    terrain[dim,dim] <- rand.corner[3]
    terrain[1,dim] <- rand.corner[4]
    return(terrain)
}

# Define the functions we'll use
diamond.step <- function(mat){
    dim <- sqrt(length(mat))
    corners <- c(mat[1,dim],mat[1,1],mat[dim,dim],mat[dim,1])
    corner.mean <- mean(corners)
    # Find the center of mat
    half <- median(1:ncol(mat))
    # Set the center of mat
    mat[half,half] <- corner.mean
    return(mat)
}
square.step <- function(mat){
    # Find the center of the top edge of the matrix 
    dim <- ncol(mat)
    row.center <- median(1:dim)
    # Set that center to be the average of the: 
    # top-left, top-right, and center cells
    top <- c(mat[1,1],mat[1,dim], mat[row.center, row.center])
    mat[1,row.center] <- mean(top) 
    # ...do it all again for the bottom, left, and right edges return(mat)
    bottom <- c(mat[dim,1],mat[dim,dim], mat[row.center, row.center]) 
    mat[dim, row.center] <- mean(bottom)

    left <- c(mat[dim,1],mat[1,1], mat[row.center, row.center]) 
    mat[row.center, 1] <- mean(left)

    right <- c(mat[1,dim],mat[dim,dim], mat[row.center, row.center]) 
    mat[row.center, dim] <- mean(right)
    return(mat)
}

# Put it all together
proceed.gen <- function(n){
    terrain <- make.terrain(n)
    terrain.size <- ncol(terrain)-1
    for (i in 2^(n: 1)){ #...hmmmm. How might this help?... 
        # Loop(s) through all the subsets of the matrix
        spacing <- seq(1, terrain.size, by=i)
        for(j in spacing){
            for(k in spacing){
                #cat(k,":",(k+i), "    ", j,":",(j+i),"\n")
                terrain[k:(k+i),j:(j+i)] <- diamond.step(terrain[k:(k+i),j:(j+i)])
                terrain[k:(k+i),j:(j+i)] <- square.step(terrain[k:(k+i),j:(j+i)])
            }
        }
    }
                
        return(terrain) 
}
proceed.gen(3)

gus <- make.terrain(3)
gus <- diamond.step(gus[1:9,1:9])
gus <- square.step(gus[1:9,1:9])
gus[1:5,1:5] <- diamond.step(gus[1:5,1:5])
gus[1:5,1:5] <- square.step(gus[1:5,1:5])
gus[1:3,1:3] <- diamond.step(gus[1:3,1:3])
gus[1:3,1:3] <- square.step(gus[1:3,1:3])
gus[1:5,1:5] <- diamond.step(gus[1:5,1:5])
gus[1:5,1:5] <- square.step(gus[1:5,1:5])



1 : 9
1 : 5
5 : 9
1 : 3
3 : 5
5 : 7
7 : 9



1 : 9      1 : 9
1 : 5      1 : 5
5 : 9      1 : 5
1 : 5      5 : 9
5 : 9      5 : 9
1 : 3      1 : 3
3 : 5      1 : 3
5 : 7      1 : 3
7 : 9      1 : 3
1 : 3      3 : 5
3 : 5      3 : 5
5 : 7      3 : 5
7 : 9      3 : 5
1 : 3      5 : 7
3 : 5      5 : 7
5 : 7      5 : 7
7 : 9      5 : 7
1 : 3      7 : 9
3 : 5      7 : 9
5 : 7      7 : 9
7 : 9      7 : 9