setup.plants <- function(repro, survive, comp.mat, names=NULL){ 
    if(is.null(names)) 
        names <- letters[seq_along(repro)]
    if(length(repro) != length(survive)) 
        stop("Reproduction and survival parameters needed for all species")
#...some more tests... 
    if(length(rownum(comp.mat)) | length(colnum(comp.mat)) != length(repro))
        stop("Comptetion matrix dimensions should be of equal length to the number of species")
repro <- setNames(repro, names) 
survive <- setNames(survive, names)
colnames(comp.mat) <- names
rownames(com.mat) <- names
#...what does the line above do? Do you want more like it? 
return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}

survive <- function(cell, info){ 
#...some code to check whether cell is empty or has water... 
if(cell == NA)
    cell <- NA
if(runif(1) <= info$survive[plant]) 
#The plant survived! so do something...
}

plant.timestep <- function(plants, terrain, info){
    for(i in 1:nrow(terrain)){
        for(j in 1: ncol(terrain)){
            plants[i,j] <- survive(plants[i,j], info)
        }   
    }
    return(plants)
}

run.plant.ecosystem <- function(terrain, timesteps=50, seed.fracs=c(.1, .1), repro=c(.4, .6) 
# Setup plant array # 

Setup the plants info info <- setup.plants(some, arguments, go, here) 
# Randomly distribute plants across the terrain 
# ...
# Fill NAs in wherever there is water 
for(i in seq_len(dim(plants)[3])){ plants[,,i][is.na(terrain)] <- NA
}
# Perform each timestep of the simulation 
for(i in seq(2,timesteps+1)){ 
    #...why timesteps+1 ?... 
    plants[,,i] <- plant.timestep(some, arguments, go, here)
} return(plants)
}