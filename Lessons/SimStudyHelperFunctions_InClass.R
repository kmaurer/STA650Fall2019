# function for simulating data of different sizes...

#' Data simulation function for computational experiments on variable selection methods
#'
#' @param n an integer for the number of rows of data to simulate
#' @param p the number of input variables to simulate
#' @param q the number of input variables to linearly relate to response
#' @param b the magnitude of the linear relationships between inputs and response
#' @param sd_y sd for error around the mean for the response  
#' @param sd_x sd for the simulated input variables
#'
#' @return a (n) by (p+1) data frame with columns y, x1, x2,... xP
#' @export
#'
#' @examples
#' 
#' make_sim_data(n=6, p=3, q=1)
make_sim_data <- function(n=100, p=20, q=10, b=0.1, sd_y=1, sd_x=1){
  X <- sapply(1:p, function(i) rnorm(n, 0, sd=sd_x))
  colnames(X) <- paste0("x",1:p)
  beta = c(rep(b, q), rep(0, p-q))
  y = (X %*% beta)[,1] + rnorm(n,0, sd_y)
  sim_data <- data.frame(y,X)
  return(sim_data)
}

# function for choosing with stepwise and fitting a regression
step_var_mod <- function(df){
  step_selected <- step(lm(y ~ . , data=df), trace = FALSE)
  return(step_selected)
}

# function for choosing with lasso and fitting regression
lasso_var_mod <- function(df){
  cv.out <- cv.glmnet(x= as.matrix(x=df[,-which(names(df)=="y")]),
                      y=df$y, alpha=1,type.measure="deviance")
  lasso_mod <- glmnet(x= as.matrix(x=df[,-which(names(df)=="y")]),
                      y=df$y, alpha=1, lambda=cv.out$lambda.1se)
  lasso_vars <- names(lasso_mod$beta[,1])[which(lasso_mod$beta[,1] != 0)]
  if (length(lasso_vars) == 0) lasso_vars <- names(lasso_mod$beta[,1])[1]
  lasso_selected <- lm( formula(paste0("y ~ 1 + ",paste(lasso_vars, collapse = " + "))), data=df )
  return(lasso_selected)
}

# function for finding number of variables included
select_var_count <- function(lin_mod){
  length(coef(lin_mod))-1
}

# function for finding 10-fold cross validated RMSE
select_cv_rmse <- function(lin_mod){
  cv_result <- train(formula(lin_mod),
                     data = lin_mod$model,
                     method="lm",
                     trControl=trainControl(method="cv",number=10),
                     tuneGrid=data.frame(intercept=TRUE))
  return(cv_result$results$RMSE)
}

# function for finding 10-fold cross validated RMSE
select_boot_rmse <- function(lin_mod){
  mean(sapply(1:10, function(x){
    boot_idx <- sample(1:nrow(lin_mod$model),replace=T)
    bootmod <- lm(formula(lin_mod), lin_mod$model[boot_idx,])
    mean((lin_mod$model$y[-boot_idx] - predict(bootmod, newdata=lin_mod$model[-boot_idx,]))^2)
  }))
}

# function to running a single trial with a single dataset
run_trial <- function(selection_alg, df, boot=FALSE){
  start_time = Sys.time()
  tmp_mod <- selection_alg(df)
  end_time = Sys.time()
  if(boot==TRUE){
    results <- data.frame(var_count = select_var_count(tmp_mod),
               rmse = select_boot_rmse(tmp_mod),
               time = difftime(end_time, start_time, units="secs"))
  }else{
    results <- data.frame(var_count = select_var_count(tmp_mod),
                          rmse = select_cv_rmse(tmp_mod),
                          time = difftime(end_time, start_time, units="secs"))
  }
  return(results)
}

# make into a function of n_sims, n, p and q
sim_var_select <- function(n_sim=10, n=100, p=10, q=5, var_select_ftn=step_var_mod, boot=FALSE){
  results <- NULL
  for(i in 1:n_sim){
    sim_data <- make_sim_data(n=n, p=p,q=q)
    results <- rbind(results, run_trial(var_select_ftn, sim_data, boot=boot))
  }
  return(results)
}