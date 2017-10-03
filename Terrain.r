make.terrain <- function(n){
    #length of sides (always odd)
    #will has n^2+1, but this doesn't always result in odd numbers, 
    #which makes diamond step hard
    dim <- (2^n)+1
    #make sure that only squares can be made
    terrain <- matrix(nrow=dim, ncol=dim)
    #make a list of random heights for corners
    rcorner <- abs(rnorm(4,0,10))
    #subset the corners and give heights (there has to be a cool guy way to do this)
    terrain[1,1] <- rcorner[1]
    terrain[dim,1] <- rcorner[2]
    terrain[dim,dim] <- rcorner[3]
    terrain[1,dim] <- rcorner[4]
    return(terrain)
}

# Define the functions we'll use
diamond.step <- function(mat){
    dim <- sqrt(length(mat))
    # Calculate mean of corners
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

    for (i in 2^(n: 1)){ #...hmmmm. How might this help?... 
        # Loop(s) through all the subsets of the matrix
        terrain[] <- diamond.step(terrain[]) 
        terrain[] <- square.step(terrain[])

                
        return(terrain) 
}
proceed.gen(3)