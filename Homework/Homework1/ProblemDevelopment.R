##### Phase 1 : Gather Simulated Gamma Data
#-------------------------------------------------------------------------------------------------------------------
### Build a function named "sim_gam" that has formal arguments for sample size (n), the vector of shapes (length M), the vector of scales (length L)
# also include an arguement that will allow the user to add a random seed number for reproducible simulation, but ignore if not specified
# Then uses these arguments to generate a 3-dimensional array of simulated values from a gamma distribution 
# The size the indexing in each dimensions should be (n) by (M) by (L)
# Your array should also have the "dimnames" attribute of the array added to show labels for the shape and scale dimensions

sim_gam <- function(n, shape_vec, scale_vec, seed=NULL){
  params <- expand.grid(shape=shape_vec,scale=scale_vec)
  if(!is.null(seed) & is.numeric(seed)) set.seed(seed)
  gam_array <- mapply(rgamma,
                          shape=params$shape,
                          scale=params$scale,
                          MoreArgs = list(n=n),
                          SIMPLIFY = "array")
  dim(gam_array) <- c(n, length(shape_vec), length(scale_vec))
  dimnames(gam_array) <- list(NULL,paste0("shape",shape_vec),paste0("scale",scale_vec))
  return(gam_array)
}

gam_array5 <- sim_gam(5,shape_vec=c(0.1,1,10,100), scale_vec=c(0.1,1,10,100) , seed=12345)

#--------------------------------------------------------------------------------------------------------
### Use your gamma simulation function to create a list of arrays called "gam_arrays_list"
# all arrays should include results for shape_vec=c(0.1,1,10,100) and scale_vec=c(0.1,1,10,100)
# each array will represent a different sample size: n=25,50,75,100,500,1000
gam_arrays_list <- lapply(c(25,50,75,100,500,1000), function(n){
  sim_gam(n,shape_vec=c(0.1,1,10,100), scale_vec=c(0.1,1,10,100) , seed=12345)
})

gam_arrays_list[[1]][1:5,,]

#--------------------------------------------------------------------------------------------------------
##### Phase 2 : Compute with the data you simulated
#-------------------------------------------------------------------------------------------------------------------
### Next build the following two functions that the output array from the gamma data simulator above, as their inputs:
# 1. A function named "gam_means" that returns an (M) by (L) matrix of means for each simulated shape and scale parameter pair.
# 2. A function named "gam_CIs" that returns an (2) by (M) by (L) array containing the lower and upper confidence bounds for the 95% confidence intervals for the means of each simulated gamma sample. 

# hint 1: check out the apply() function help documentation
# hint 2: You will need a function that can take in a sample vector and return the CI as a vector of c(lower.val, upper.val). The t.test() function may contain helpful results.

gam_means <- function(gam_array){
  apply(gam_array, MARGIN=c(2,3), FUN=mean)
}

ci_helper <- function(vector){
  t.test(vector)$conf.int[1:2]
}

gam_CIs <- function(gam_array){
  apply(gam_array, MARGIN=c(2,3), FUN=ci_helper)
}

# checking if they work
gam_means(gam_array5)
gam_CIs(gam_array5)

### Apply your gam_means() function to the list of arrays called "gam_arrays_list". Store the results as list of means matrices
gam_sim_means <- lapply(gam_arrays_list, gam_means)
gam_sim_means

### Apply your gam_CIs() function to the list of arrays called "gam_arrays_list". Store the results as list of CI arrays
gam_sim_CIs <- lapply(gam_arrays_list, gam_CIs)
gam_sim_CIs[[6]]
