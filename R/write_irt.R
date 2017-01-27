#' Write item response data to a fixed width file.
#' 
#' @param dat \code{data.frame} of item responses
#' @param id_var \code{character} name of the id variable in \code{dat}
#' @param group_var \code{character} (or vector of) name(s) of grouping variable in \code{dat}
#' @param na integer to replace NA
#' @param ... takes arguement from \code{write.fwf}
#'  
#' @return A \code{data.frame} of item responses.
#' 
#' @examples
#' df <- rbind.data.frame(c(1, NA, 1, 0, NA, 0, 0, NA, 1, 0, 1),
#'                        c(1, 1, NA, 1, 1, NA, 1, 0, NA, 0, 2),
#'                        c(1, NA, 0, 1, NA, 1, 1, NA, 1, 1, 3),
#'                        c(1, NA, 0, 0, NA, 1, 1, NA, 0, 0, 4),
#'                        c(1, NA, 0, 0, NA, 1, 0, NA, 0, 1, 5),
#'                        c(NA, 0, 0, NA, 0, 0, NA, 0, 0, NA, 6),
#'                        c(0, 1, NA, 0, 0, NA, 0, 1, NA, 0, 7),
#'                        c(NA, 1, 0, NA, 1, 1, NA, 1, 1, NA, 8),
#'                        c(1, 1, NA, 1, 0, NA, 0, 0, NA, 0, 9),
#'                        c(1, NA, 2, 1, NA, 1, 1, NA, 1, 1, 10),
#'                        c(NA, 1, 1, NA, 0, 1, NA, 0, 1, NA, 11),
#'                        c(1, NA, 0, 1, NA, 0, 0, NA, 1, 1, 12),
#'                        c(NA, 1, 0, NA, 1, 0, NA, 0, 0, NA, 13),
#'                        c(NA, 1, 0, NA, 2, 0, NA, 1, 0, NA, 14),
#'                        c(NA, 1, 1, NA, 2, 1, NA, 1, 1, NA, 15),
#'                        c(NA, 1, 0, NA, 1, 1, NA, 1, 0, NA, 16),
#'                        c(1, 1, NA, 0, 0, NA, 1, 1, NA, 1, 17),
#'                        c(1, 0, NA, 0, 1, NA, 1, 0, NA, 0, 18),
#'                        c(1, 1, NA, 1, 2, NA, 1, 0, NA, 1, 19),
#'                        c(1, NA, 2, 1, NA, 0, 0, NA, 0, 0, 20))
#' colnames(df) <- c(paste0("i", seq(10)), "id")
#' write.irt(dat = df, id_var = "id", sep = "  ")
#' \dontrun{
#'   write.irt(df, id_var = "id", na = 8, sep = "  ", file = "my_irt_data.txt")
#' }
write.irt <- function(dat, id_var = NULL, theta_var = NULL, group_var = NULL, na = 9, ...){
  #... should include file=" ", sep = " ")
  dat[is.na(dat)] <- na
  input_names <- c(id_var, theta_var, group_var)
   
  i_names <- colnames(dat)[!(colnames(dat) %in% input_names)]  

  ir_dat <- dat[, input_names] 

  if (length(input_names) == 1){
    responses <- do.call(paste0, dat[, i_names])
    ir_dat <- cbind.data.frame(ir_dat, responses)
  } else {
    ir_dat$responses <- do.call(paste0, dat[, i_names])
  }

  info_test <- write.fwf(ir_dat, formatInfo = T, rownames = F, colnames = F, ...)
  return(info_test)
}
