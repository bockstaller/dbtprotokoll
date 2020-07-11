


#'Parse all plenary protocols from xml format to tibbles in parallel
#'
#'Uses the xml structure of a plenary protocols to create three tibbles for further data analysis with the help of parse_protocol().
#'
#'@param path A string containing the path to the xml files you want to parse
#'
#'@param start Name of the protocol you want to start with (optional)
#'
#'@param end Name of the protocol you want to end with (optional). Must succeed start in alphabetical order
#'
#'@return Three tibbles in a named list:
#'
#'"speakers": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"paragraphs": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"comments": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@examples
#'parse_protocols(start = "19001-data.xml", end = "19003-data.xml")
#'
#'@export
pparse_protocols <- function(path = "protokolle", start = NULL, end = NULL){
  protocols <- dir(path)
  protocols <- protocols[endsWith(protocols, ".xml")]

  #remove everything before start
  if(!is.null(start)){
    while(length(protocols) > 1 && protocols[1] != start){
      protocols <- protocols[2:length(protocols)]
    }
    if(protocols[1] != start){
      stop(start, " was not found in ", path, ".")
    }
  }

  #remove everything after end
  if(!is.null(end)){
    while(length(protocols) >= 1 && protocols[length(protocols)] != end){
      protocols <- protocols[1:length(protocols)-1]
    }
    if(length(protocols) == 0){
      stop(end, " was not found in ", path, ". Does it preceed ", start, "?")
    }
  }



  cluster = function (n = NULL)
  {
    if (is.null(n)) {
      n <- parallel::detectCores()
    }
    parallel::makeCluster(n, setup_strategy = "sequential", outfile="")
  }


  create_file_path <- function(protocol){
    return(stringr::str_c(path, "/", protocol))
  }



  paths <- lapply(protocols, create_file_path)

  cl <- cluster()
  ptm <- proc.time()
  parsed_protocols <- parallel::parLapply(cl, paths, dbtprotokoll::parse_protocol)
  print(proc.time() - ptm)
  parallel::stopCluster(cl)

  #merge protocols
  protocolstb <- list("speakers"=tibble::tibble(),
                      "paragraphs"=tibble::tibble(),
                      "comments"=tibble::tibble())

  ptm1 <- proc.time()
  for(protocol in parsed_protocols){
    for(j in 1:3){
      protocolstb[[j]] <- dplyr::bind_rows(protocolstb[[j]], protocol[[j]])
    }
  }
  print(proc.time() - ptm1)

  #tidy speakers
  protocolstb[[1]] <- dplyr::distinct(protocolstb[[1]])

  return(protocolstb)
}
