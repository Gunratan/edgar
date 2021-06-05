#' Retrieves filing information of a firm
#'
#' \code{getFilingInfo} retrieves filing information of a firm based on its name or cik.
#'
#' getFilingInfo function takes firm identifier (name or cik), filing year(s), quarter(s), 
#' and form type as input parameters from a user and provides filing information for the 
#' firm. The function automatically downloads master index for the input year(s) and 
#' the quarter(s) using \link[edgar]{getMasterIndex} function if it is not already 
#' been downloaded in the current working directory. By default, information of all 
#' the form types filed in all the quarters of the input year by the firm will be 
#' provided by this function.
#' 
#' @usage getFilingInfo(firm.identifier, filing.year, quarter, form.type)
#' 
#' @param firm.identifier CIK of a firm in integer format or full/partial 
#' name of a firm in character format. Suppress leading zeroes from CIKs.
#' 
#' @param filing.year vector of integer containing filing years.
#' 
#' @param quarter vector of one digit integer quarter number. By default, it is 
#' considered as all the quarters, quarter =c(1, 2, 3, 4).
#'
#' @param form.type vector of form types in character format. By default, it is kept
#' as all the available form types.
#' 
#' @return Function returns dataframe with filing information.
#'   
#' @examples
#' \dontrun{
#' 
#' info <- getFilingInfo('United Technologies', c(2005, 2006), 
#'                        quarter = c(1,2), form.type = c('8-K','10-K')) 
#' ## Returns filing information on '8-K' and '10-K' filed by the firm 
#' in quarter 1 and 2 of year 2005 and 2006.
#' 
#' info <- getFilingInfo(1067701, 2006) 
#' ## Returns all the filings information filed by the firm in all 
#' the quarters of year 2006.
#'}

getFilingInfo <- function(firm.identifier, filing.year, quarter = c(1, 2, 3, 4),
                          form.type = "ALL") {
  
    options(warn = -1)
  
    # Check the year validity
    if (!is.numeric(filing.year)) {
        cat("Error: Input year(s) is not numeric.")
        return()
    }
    
    # Create empty master index file and then updated it yearwise
    index.df <- data.frame()
    
    # Iterate thorugh each years
    for( year in filing.year ){
      
      yr.master <- paste0(year, "master.Rda")  ## Create specific year .Rda filename.
      
      filepath <- paste0("Master Indexes/", yr.master)
      
      if (!file.exists(filepath)) {
        getMasterIndex(year)  # download master index
      }
      
      load(filepath)  # Import master Index
      
      if((length(form.type) == 1) && (form.type == "ALL")){
        form.type <- unique(year.master$form.type)
      }
      
      
      # Check if user input CIK or firm name as a firm.identifier
      if (is.numeric(firm.identifier) | !is.na(as.numeric(firm.identifier))) {
        # firm.identifier is CIK and extract specific firm information
        year.master <- year.master[which(year.master$cik == firm.identifier 
                                         & year.master$form.type %in% form.type 
                                         & year.master$quarter %in% quarter), ]
        
      } else {
        
        # firm.identifier is firm name and extract specific firm information
        year.master <- year.master[grep(firm.identifier, year.master$company.name, ignore.case = TRUE), ]
        
        if(nrow(year.master)>0){
          year.master <- year.master[which(year.master$form.type %in% form.type & year.master$quarter %in% quarter), ]
        }
        
      }

      
      if(nrow(year.master)>0){
        
        year.master$filing.year <- year
        
        # Update main master index file
        index.df <- rbind(index.df, year.master)
      }
      
    }
  
    cat("Searching master indexes for filing information ...\n")
    
    if (nrow(index.df) == 0) {
      cat("No filing information found for given firm identifier and Form Type(s) in the mentioned year(s)/quarter(s).\n")
      return()
    }
    
    index.df <- index.df[order(index.df$cik, index.df$filing.year), ]
    
    index.df$edgar.link <- NULL  # Remove edgar link column
    
    rownames(index.df) <- 1:nrow(index.df)
    index.df$date.filed <- as.Date(as.character(index.df$date.filed), "%Y-%m-%d")
    
    return(index.df)
}
