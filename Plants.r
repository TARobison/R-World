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
    if(is.na(cell)){
        cell <- NA
    }
    else{
        if(cell == '')
            cell <- ''
        else if(runif(1) <= info$survive[cell]){ 
            cell <- cell
        }else{
            cell <- ''
        }
    }
    return(cell)
}      

plant.timestep <- function(plants, terrain, info){
    for(i in 1:nrow(terrain)){
        for(j in 1: ncol(terrain)){
            plants[i,j] <- survive(plants[i,j], info)
        }   
    }
    for(i in 1:nrow(terrain)){
        for(j in 1: ncol(terrain)){
            plants <- reproduce(i, j, plants, info)
        }
    }
    return(plants)
}

run.plant.ecosystem <- function(terrain, comp.mat, timesteps=50, seed.fracs=c(.1, .1), repro=c(.4, .8), survive=c(.8,.65),names=NULL){ 
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
    for(i in seq(2,timesteps)){ 
        plants[,,i] <- plant.timestep(plants[,,i-1], terrain, info)
    }
    return(plants)
}
run.plant.ecosystem(terrain, comp.mat) 

reproduce <- function(row, col, plants, info){
    cell <- plants[row,col]
    possible.locations <- as.matrix(expand.grid(row+c(-1, 0, 1), col+c(-1, 0, 1)))
    possible.locations <- na.omit(possible.locations)
    possible.locations <- actually.possible(row,col,possible.locations,plants)
    if(is.na(cell))
        cell <- NA
    else{
        if(cell == '')
            cell <- ''
        else if(runif(1) <= info$repro[cell]){
            random.draw <- sample(1:nrow(possible.locations),1)
            new.row <- possible.locations[random.draw,1]
            new.col <- possible.locations[random.draw,2]
            if(plants[new.row,new.col] != ''){
                fight(plants, comp.mat, cell, info)
            }else
                plants[new.row,new.col] <- cell
            return(plants)
        }else{
            cell <- ''
        }
    }
    return(plants)
}


actually.possible <- function(row,col,possible.locations,plants){
    for(i in nrow(possible.locations):1){
        if(possible.locations[i,1] < 1 | possible.locations[i,1] > nrow(plants) | possible.locations[i,2] < 1 | possible.locations[i,2] > nrow(plants)){
            possible.locations <- possible.locations[-i,]
        }
    }
    return(possible.locations)
}

fight <- function(plants, comp.mat, cell, info){
    cell <- sample(info$names, 1, prob=comp.mat[1,])
    return(cell)
}
