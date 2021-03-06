#'Parse a plenary protocol from xml format to tibbles
#'
#'Uses the xml structure of a plenary protocol to create three tibbles for further data analysis.
#'
#'@param path A string containing the path to the xml file you want to parse
#'@param check_schema A logical value indicating whether you want to check compatibility of the xml schema used in your xml file.
#
#'
#'@return Three tibbles in a named list:
#'
#'"speakers": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"paragraphs": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"comments": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@export
parse_protocol <- function(path, check_schema = TRUE){
  stopifnot("Please enter path as string" = is.character(path))
  print(stringr::str_c("Parsing: ", path))
  protocol <- xml2::read_xml(path)
  if(check_schema){
    path = stringr::str_c(system.file('data', package = 'dbtprotokoll'), "/dbtplenarprotokoll-schema.xsd")
    schema <- xml2::read_xml(path)
    stopifnot("XML Schema is not as expected." = xml2::xml_validate(protocol, schema))
  }

  speakers_roles <- split_roles(speaker_list(protocol))
  commenttb <- comment_list(protocol)
  paragraphtb <- paragraph_list(protocol)
  return(list("speakers"=speakers_roles[[1]],"paragraphs"=paragraphtb, "comments"=commenttb, "roles"=speakers_roles[[2]]))
}



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
#'@param instance_count Specifies the number of r instances that will be used to parse the protocols. The default is the machines core count.
#'
#'@param check_schema A logical value indicating whether you want to check compatibility of the xml schema used in your xml file.
#'
#'@return Three tibbles in a named list:
#'
#'"speakers": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"paragraphs": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"comments": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@export
parse_protocols <- function(path = "protokolle", start = NULL, end = NULL, instance_count = NULL, check_schema=TRUE){
  stopifnot("Please enter path as string" = is.character(path))
  protocols <- dir(path)
  if(identical(protocols, character(0))){
    stop(path, " was not found or is empty.")
  }
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

  cl <- cluster(instance_count)
  on.exit(parallel::stopCluster(cl))
  parsed_protocols <- parallel::parLapply(cl, paths, dbtprotokoll::parse_protocol, check_schema=check_schema)

  #merge protocols
  protocolstb <- list("speakers"=tibble::tibble(),
                      "paragraphs"=tibble::tibble(),
                      "comments"=tibble::tibble(),
                      "roles"=tibble::tibble())

  for(protocol in parsed_protocols){
    for(j in 1:4){
      protocolstb[[j]] <- dplyr::bind_rows(protocolstb[[j]], protocol[[j]])
    }
  }

  #tidy speakers
  protocolstb[[1]] <- clean_speakers(protocolstb[[1]])
  protocolstb[[4]] <- clean_roles(protocolstb[[4]])

  return(protocolstb)
}
