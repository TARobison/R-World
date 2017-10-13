setup.plants <- function(repro, survive, comp.mat, names=NULL){ 
    if(is.null(names)) 
        names <- letters[seq_along(repro)]
    if(length(repro) != length(survive)) 
        stop("Reproduction and survival parameters needed for all species") 
    if(nrow(comp.mat) != length(repro))
        stop("Comptetion matrix dimensions should be of equal length to the number of species")
    repro <- setNames(repro, names) 
    survive <- setNames(survive, names)
    colnames(comp.mat) <- names
    rownames(comp.mat) <- names
    return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}


comp.mat <- matrix(c(.7,.3,.3,.7),2,2)
setup.plants(c(.4,.6), c(.6,.6),comp.mat)

survive <- function(cell, info){  
    if(cell == NA)
        cell <- NA
    if(runif(1) <= info$survive[plant]){ 
    #The plant survived! so do something...
        cell <- cell
    }else{
        cell <- ''
    }
    return(survive)
}

plant.timestep <- function(plants, terrain, info){
    for(i in 1:nrow(terrain)){
        for(j in 1: ncol(terrain)){
            plants[i,j] <- survive(plants[i,j], info)
        }   
    }
    for(i in 1:nrow(terrain)){
        for(j in 1: ncol(terrain)){
            plants[i,j] <- reproduce(i, j, plants, info)
        }
    }
    return(plants)
}

run.plant.ecosystem <- function(terrain, comp.mat, timesteps=50, seed.fracs=c(.1, .1), repro=c(.4, .6), survive=c(.6,.6),names=NA){ 
    plants <- array('', dim=c(ncol(terrain),nrow(terrain), timesteps))
    info <- setup.plants(repro, survive, comp.mat, names) 
    random.x.val <- sample(1:ncol(terrain),length(terrain)*.1)
    random.y.val <- sample(1:ncol(terrain),length(terrain)*.1)
    for(i in 1:length(random.x.val)){
        choose.species <- sample(1:2,1)
        if(choose.species == 1)
            plants[random.x.val[i],random.y.val[i],1] <- info$names[[1]]
        else
            plants[random.x.val[i],random.y.val[i],1] <- info$names[[2]]
    }
    for(i in seq_len(dim(plants)[3])){ 
        plants[,,i][is.na(terrain)] <- NA
    }
    # Perform each timestep of the simulation 
    for(i in seq(2,timesteps+1)){ 
        #...why timesteps+1 ?... 
        plants[,,i] <- plant.timestep(plants, terrain, info)
    }
    return(plants)
}

run.plant.ecosystem(terrain, comp.mat) 
reproduce <- function(row, col, plants, info){
    possible.locations <- as.matrix(expand.grid(row+c(-1, 0, 1), col+c(-1, 0, 1)))
     #...now filter out which ones are not water-logged and reproduce there... 
     #...being careful to check you do have somewhere to reproduce to!... 
     return(plants)
}

fight <- function(names, something, something){
    sample(species_names, 1, prob=comp.mat[row,column])
}