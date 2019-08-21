clusterMessage <- function(n_obs, resp_labels, cluster_labels, n_levels, type)
{
  # This function prints messages about the cluster scheme before generating questionnaire responses. All arguments are from cluster_gen except for "type", which is numeric and changes the way the first line is printed.
  if (type == 1)
  {
    # Comma-separated multiple questionnaires
    message("Generating questionnaires for ",
            paste(cluster_labels, collapse = ", "))
  }
  else
  {
    # Questionnaires only for the lowest level
    message("Generating questionnaires for ", resp_labels[n_levels - 1])  
  }
  for (l in 1:(length(n_obs) - 1)) # Final row of messages is different
  {
    n_obs_print <- n_obs
    if (class(n_obs) == "list")
    {
      n_obs_print[[l]] <- paste("between", n_obs[[l]][1], "and", n_obs[[l]][2])
      n_obs_print[[l + 1]] <- paste("between", n_obs[[l + 1]][1], "and", n_obs[[l + 1]][2])
    }
    if (l == 1) message("Top level: ", n_obs_print[l], " ", cluster_labels[l])
    if (l < length(n_obs) - 1)
    {
      message("Each ", cluster_labels[l], " sampled ", n_obs_print[l + 1], " ",
              cluster_labels[l + 1])
    }
  }
  # Final row of messages
  message("Each ", cluster_labels[n_levels - 1], " sampled ",
          n_obs_print[n_levels], " ", resp_labels[n_levels - 1])

  message("Total respondents: ",
          paste0(prod(unlist(n_obs)), " (",
                 paste(unlist(n_obs), collapse = " * "), ")"))
}