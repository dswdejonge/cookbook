#' Add WoRMs taxonomy
#'
#' This function collects WoRMS entries from their database using the reported taxa.
#' @references worrms R-package
#' @param species_names Character vector with taxa names.
#' @param fuzzy Uses the TAXAMATCH algorithm to fuzzy match species names.
#' @details Searches in batches of 50.
#' @export
collect_from_worms <- function(species_names, fuzzy = FALSE){
  worms <- list()
  if(fuzzy){
    func <- worrms::wm_records_taxamatch
  }else{
    func <- worrms::wm_records_names
  }
  i1 <- 1 ; i2 <- 50
  while(i1 <= length(species_names)){
    if(i2 > length(species_names)){i2 <- length(species_names)}
    collected_data <- func(species_names[i1:i2])
    worms <- c(worms, collected_data)
    i1 <- i1 + 50
    i2 <- i2 + 50
  }
  names(worms) <- species_names
  return(worms)
}


#' Get WoRMs taxonomy
#'
#' This function checks all reported taxa against the WoRMS databse.
#' @references worrms R-package
#' @param species_names Character vector with taxa names.
#' @details This function collects WoRMS entries based on the given species names, and reports the valid name
#' and rank that is found. It first finds all direct matches, and then does a fuzzy match for queries that do
#' not have a direct match against the database.
#' @export
get_worms_taxonomy <- function(species_names){
  # Get unique species names
  reported_species <- unique(species_names)

  # Collect exact matches
  worms <- collect_from_worms(reported_species)

  # Find index of complete matches and empty lists
  complete_matches <- which(lapply(worms,length) > 0)
  fuzzy_matches <- which(lapply(worms,length) == 0)

  if(length(fuzzy_matches) > 0){
    # Collect fuzzy matches
    new_records <- collect_from_worms(names(fuzzy_matches), fuzzy = T)

    # Add fuzzy matches to worms list
    worms[fuzzy_matches] <- new_records
  }

  # Find index of reported species without matches or with mulitple matches
  still_no_matches <- which(lapply(worms,length) == 0)
  multiple_matches <- which(lapply(worms, nrow) > 1)

  # Create an empty record for species without a (good) match
  empty_record <- worms[[complete_matches[1]]]
  empty_record[1,] <- NA
  empty_record <- dplyr::mutate(empty_record, hasNoMatch = 1)

  # If a reported species has multiple matches, only the accepted match is used
  # If there are multiple accepted names, it is assumed a no match and gets an empty record.
  if(length(multiple_matches) > 0){
    for(i in 1:length(multiple_matches)){
      record <- worms[[multiple_matches[i]]]
      # Only use accepted name if there is only 1
      accepted <- dplyr::filter(record, status == "accepted")
      if(nrow(accepted) == 1){
        worms[[multiple_matches[i]]] <- accepted
      # otherwise, take valid name if they are all the same
      }else if(length(unique(record$valid_name)) == 1){
        worms[[multiple_matches[i]]] <- record[1,]
      # if there are no accepted names, and multiple valid names, use empty record
      }else{
        worms[[multiple_matches[i]]] <- empty_record
      }
    }
  }

  # Reported species that cannot be fuzzy matches gets an empty record.
  if(length(still_no_matches) > 0){
    for(i in 1:length(still_no_matches)){
      worms[[still_no_matches[i]]] <- empty_record
    }
  }

  # Merge into tibble and return
  worms_df <- dplyr::bind_rows(worms, .id = "Query") %>%
    dplyr::mutate(isFuzzy = NA)
  worms_df[fuzzy_matches,"isFuzzy"] <- 1

  return(worms_df)
}

