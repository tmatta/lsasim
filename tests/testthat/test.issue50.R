param <- NAEPirtparams::parameters
item_par <- param[param$level %in% 8 & param$subject %in% "Mathematics" & param$year %in% 2015, ]
item_par$item <- sample(1:1e5, nrow(item_par))
item_par <- item_par[1:32,]
# this gives the error I share above about invalid `times` argument
block <- lsasim::block_design(n_blocks = 10, item_parameters = item_par)
