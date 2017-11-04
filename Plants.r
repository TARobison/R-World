#' @title Generate plants to exist, reproduce, and compete within the terrain
#'
#' @description Simulates plants on a terrain which is built in
#' terrain.r. Plants survive and reproduce in terrain that has a height 0 <= 
#' Ensure that you have the same length of vectors for params repro, survive and names!!
#' 
#' @param repro The rate of reproduction of the listed plants. (default: .4,.8)
#'          Is a vector of length equal to the number of listed plants
#' @param survive The rate of survival of the listed plants. (default: .8, .65)
#'          Is a vector of length equal to the number of listed plants
#' @param names The names of listed plants. (defaul=NULL)
#'          If no names are provided, plants will be assigned a letter in the alphabet 
#'          Is a vector of length equal to the number of listed plants
#' @param timesteps The number of 'turns' that you want the simulation to exectute. (defaul: 50)
#' @param terrain The matrix that simulates landscape. Generated from make.terrain
#' @return a plant matrix; empty cells contain only '', waterlogged cells contain Na,  
#'          and cells containing plants will have the indicated (or assigned) name. 
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats setNames
#' @examples 
#' plants <- make.plants(make.terrain(6,15), c(.7,.85), repro=c(.95,.55), names=NULL, 50)
#' @export

make.plants <- function(terrain, survive=c(.6,.85), repro=c(.95,.45),names=NULL, timesteps=50){

    comp.mat <- matrix(c(survive, rev(survive)), length(survive),length(survive))

    setup.plants <- function(repro, survive, comp.mat, names){ 
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
    
    survival <- function(cell, info){  
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
                plants[i,j] <- survival(plants[i,j], info)
            }   
        }
        for(i in 1:nrow(terrain)){
            for(j in 1: ncol(terrain)){
                plants <- reproduce(i, j, plants, info)
            }
        }
        return(plants)
    }

    run.plant.ecosystem <- function(terrain, comp.mat, survive, repro, names, timesteps){ 
        plants <- array('', dim=c(ncol(terrain),nrow(terrain), timesteps))
        info <- setup.plants(repro, survive, comp.mat, names) 
        random.x.val <- sample(1:ncol(terrain),length(terrain)*.1,replace=TRUE)
        random.y.val <- sample(1:ncol(terrain),length(terrain)*.1,replace=TRUE)
        #
        for(i in 1:length(random.x.val)){
            plants[random.x.val[i],random.y.val[i],1] <- sample(info$names,1)
        }
        for(i in seq_len(dim(plants)[3])){ 
            plants[,,i][is.na(terrain)] <- NA
        }
        for(i in seq(2,timesteps)){ 
            plants[,,i] <- plant.timestep(plants[,,i-1], terrain, info)
        }
        return(plants)
    } 

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
    plant.plot <- run.plant.ecosystem(terrain, comp.mat, survive, repro, names, timesteps)
    return(plant.plot)
}
make.plants(make.terrain(3,15))