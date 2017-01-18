# writes out items response data for parscale. 

write.parscal <- function(dat, id_var, theta_var, group_var = NULL, na = 9, ...){
  require(gdata)
  #... should include file=" ", sep = " ")
  dat[is.na(dat)] <- na
  input_names <- c(id_var, theta_var, group_var)
   
  i_names <- colnames(dat)[!(colnames(dat) %in% input_names)]  
  ir_dat <- dat[, input_names]
  

  ir_dat$responses <- do.call(paste0, dat[, i_names])
  info_test <- write.fwf(ir_dat, formatInfo = T, rownames = F, colnames = F, ...)
  return(info_test)
}
