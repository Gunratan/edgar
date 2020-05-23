#' Retrieves business descriptions from annual statements
#'
#' \code{getBusinessDescr} retrieves business description section from annual statements 
#' based on CIK number(s) and filing year(s).
#'
#' getBusinDescr function takes firm CIK(s) and filing year(s) as input parameters from 
#' a user and provides "Item 1" section extracted from annual statements along with 
#' filing information. The function imports annual filings (10-K statements) downloaded 
#' via \link[edgar]{getFilings} function; otherwise, it automates the downloading process 
#' if not already been downloaded. It then reads the downloaded statements, cleans HTML tags, 
#' and parse the contents. This function automatically creates a new directory with 
#' the name "Business descriptions text" in the current working directory and 
#' saves scrapped business description sections in this directory. It considers "10-K", 
#' "10-K405", "10KSB", and "10KSB40" form types as annual statements.
#' 
#'   
#' @usage getBusinDescr(cik.no, filing.year)
#' 
#' @param cik.no vector of firm CIK(s) in integer format. Suppress leading 
#' zeroes from a CIK number. cik.no = 'ALL' conisders all the CIKs.
#' 
#' @param filing.year vector of four digit numeric year
#' 
#' @return Function saves scrapped business description section from annual 
#' filings in "Business descriptions text" directory created in the current 
#' working directory. The output dataframe contains filing information and 
#' parsing status.
#'   
#' @examples
#' \dontrun{
#' 
#' output <- getBusinDescr(cik.no = c(1000180, 38079), filing.year = 2005)
#' ## saves scrapped "Item 1" section from 10-K filings for CIKs in 
#' "Business descriptions text" directory present 
#' in the working directory. Also, it provides filing information in 
#' the output datframe.
#' 
#' output <- getBusinDescr(cik.no = c(1000180, 38079), 
#'                         filing.year = c(2005, 2006))
#'}

getBusinDescr <- function(cik.no, filing.year) {
  
  f.type <- c("10-K", "10-K405","10KSB", "10KSB40")
  
  # Check the year validity
  if (!is.numeric(filing.year)) {
    cat("Please check the input year.")
    return()
  }
  
  output <- getFilings(cik.no = cik.no, form.type = f.type , filing.year, 
                       quarter = c(1, 2, 3, 4), downl.permit = "y")
  
  if (is.null(output)){
    cat("No annual statements found for given CIK(s) and year(s).")
    return()
  }
  
  cat("Extracting 'Item 1' section...\n")
  
  progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
  
  # Function for text cleaning
  CleanFiling2 <- function(text) {
    
    text <- gsub("[[:digit:]]+", "", text)  ## remove Alphnumerics
    
    text <- gsub("\\s{1,}", " ", text)
    
    text <- gsub('\"',"", text)
    
    #text <- RemoveStopWordsFilings(text)
    
    return(text)
  }
    
  new.dir <- paste0("Business descriptions text")
  dir.create(new.dir)
  
  output$extract.status <- 0
  
  output$company.name <- toupper(as.character(output$company.name))
  output$company.name <- gsub("\\s{2,}", " ",output$company.name)
  
  for (i in 1:nrow(output)) {
    
    f.type <- gsub("/", "", output$form.type[i])
    year <- output$filing.year[i]
    cname <- gsub("\\s{2,}", " ",output$company.name[i])
    cik <- output$cik[i]
    date.filed <- output$date.filed[i]
    accession.number <- output$accession.number[i]
    
    dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                            "/", cik, "/", cik, "_", f.type, "_", 
                            date.filed, "_", accession.number, ".txt")
    # Read filing
    filing.text <- readLines(dest.filename)
    
    # Extract data from first <DOCUMENT> to </DOCUMENT>
    tryCatch({
      filing.text <- filing.text[(grep("<DOCUMENT>", filing.text, ignore.case = TRUE)[1]):(grep("</DOCUMENT>", 
                                                                                                filing.text, ignore.case = TRUE)[1])]
    }, error = function(e) {
      filing.text <- filing.text ## In case opening and closing DOCUMENT TAG not found, cosnider full web page
    })
    
    # See if 10-K is in XLBR or old text format
    if (any(grepl(pattern = "<xml>|<type>xml|<html>|10k.htm", filing.text, ignore.case = T))) {
      
      doc <- XML::htmlParse(filing.text, asText = TRUE, useInternalNodes = TRUE, addFinalizer = FALSE)
      f.text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
                                 XML::xmlValue)
      
      f.text <- iconv(f.text, "latin1", "ASCII", sub = " ")
      
	  ## Free up htmlParse document to avoid memory leakage, this calls C function
      #.Call('RS_XML_forceFreeDoc', doc, package= 'XML')
      
    } else {
      f.text <- filing.text
    }
    
    # Preprocessing the filing text
    f.text <- gsub("\\n|\\t", " ", f.text)
    f.text <- gsub("\\s{2,}|\\/", " ", f.text)
    f.text <- gsub("^\\s{1,}", "", f.text)
    f.text <- gsub(" s ", " ", f.text)
    
    f.text <- gsub("Items", "Item", f.text, ignore.case = TRUE)
    f.text <- gsub("PART I", "", f.text, ignore.case = TRUE)
    f.text <- gsub("Item III", "Item 3", f.text, ignore.case = TRUE)
    f.text <- gsub("Item II", "Item 2", f.text, ignore.case = TRUE)
    f.text <- gsub("Item I|Item l", "Item 1", f.text, ignore.case = TRUE)
    f.text <- gsub(":|\\*", "", f.text, ignore.case = TRUE)
    f.text <- gsub("-", " ", f.text)
    f.text <- gsub("ONE", "1", f.text, ignore.case = TRUE)
    f.text <- gsub("TWO", "2", f.text, ignore.case = TRUE)
    f.text <- gsub("THREE", "3", f.text, ignore.case = TRUE)
    f.text <- gsub("1\\s{0,}\\.", "1", f.text)
    f.text <- gsub("2\\s{0,}\\.", "2", f.text)
    f.text <- gsub("3\\s{0,}\\.", "3", f.text)
    
    # Check for empty Lines and delete it
    empty.lnumbers <- grep("^\\s*$", f.text)
    
    if (length(empty.lnumbers) > 0) {
      f.text <- f.text[-empty.lnumbers]  ## Remove all lines only with space
    }
    
    
    # Cobine lines with ITEM only in one line and attach 1. Business from another line
    item.lnumbers <- grep("^ITEM\\s{0,}\\d{0,}\\s{0,}$|^ITEM\\s{0,}1 and 2\\s{0,}$", f.text, ignore.case = T)
    
    f.text[item.lnumbers + 1] <- paste0(f.text[item.lnumbers], " ", f.text[item.lnumbers + 1])
    
    
    # Get BUSINESS DESCRIPTION
    startline <- grep("^Item\\s{0,}1\\s{0,}Business\\s{0,}\\.{0,1}\\s{0,}$|^Item\\s{0,}1\\s{0,}DESCRIPTION OF BUSINESS\\s{0,}\\.{0,1}\\s{0,}$", 
                      f.text, ignore.case = T)
    
    endline <- grep("^Item\\s{0,}2\\s{0,}Properties\\s{0,}\\.{0,1}\\s{0,}$|Item\\s{0,}2\\s{0,}DESCRIPTION OF PROPERTY\\s{0,}\\.{0,1}\\s{0,}$|^Item\\s{0,}2\\s{0,}REAL ESTATE\\s{0,}\\.{0,1}\\s{0,}$", 
                    f.text, ignore.case = T)
    
    # Check for (ITEMS 1 and 2. BUSINESS AND PROPERTIES) and (ITEM 3. LEGAL PROCEEDINGS)
    if (length(startline) == 0 && length(endline) == 0) {
      
      startline <- grep("^Item\\s{0,}1 and 2\\s{1,}Business AND PROPERTIES\\s{0,}\\.{0,1}\\s{0,}$|^Item\\s{0,}1 and 2\\s{1,}Business and Description of Property\\s{0,}\\.{0,1}\\s{0,}$", 
                        f.text, ignore.case = T)
      
      endline <- grep("^Item\\s{0,}3\\s{1,}LEGAL PROCEEDINGS\\s{0,}\\.{0,1}\\s{0,}$|^Item\\s{0,}3\\s{1,}LEGAL matters\\s{0,}\\.{0,1}\\s{0,}$", 
                      f.text, ignore.case = T)
    }
    
    product.descr <- NA
    
    if (length(startline) != 0 && length(endline) != 0) {
      
      if (length(startline) == length(endline)) {
        for (l in 1:length(startline)) {
          product.descr[l] <- paste(f.text[startline[l]:endline[l]], collapse = " ")
        }
      } else {
        startline <- startline[length(startline)]
        endline <- endline[length(endline)]
        product.descr <- paste(f.text[startline:endline], collapse = " ")
      }
      
      product.descr <- gsub("\\s{2,}", " ", product.descr)
      words.count <- stringr::str_count(product.descr, pattern = "\\S+")
      product.descr <- product.descr[which(words.count == max(words.count))]
      product.descr <- gsub("\\. Item 2 .*", ".", product.descr)
      
      
      # product.descr <- gsub(" co\\.| inc\\.| ltd\\.| llc\\.| comp\\.", " ", product.descr, ignore.case = T)
      
      # product.descr2 <- unlist(strsplit(product.descr, "\\. "))
      # product.descr2 <- paste0(product.descr2, ".")
      #product.descr <- CleanFiling3(product.descr)
      
      header <- paste0("CIK: ", cik, "\n", "Company Name: ", cname, "\n", 
                       "Form Type : ", f.type, "\n", "Filing Date: ", date.filed, "\n",
                       "Accession Number: ", accession.number)  
      product.descr <- paste0(header, "\n\n\n", product.descr)
      
    }
    
    if( (!is.na(product.descr)) & (max(words.count)>100)){
      filename2 <- paste0(new.dir, '/',cik, "_", f.type, "_", date.filed, 
                          "_", accession.number, ".txt")
      
      writeLines(product.descr, filename2)
      output$extract.status[i] <- 1
    }
    
    
    # update progress bar
    setTxtProgressBar(progress.bar, i)
  }
  
  ## convert dates into R dates
  output$date.filed <- as.Date(as.character(output$date.filed), "%Y-%m-%d")
  
  # Close progress bar
  close(progress.bar)
  
  output$quarter <- NULL
  output$filing.year <- NULL
  names(output)[names(output) == 'status'] <- 'downld.status'
  
  cat("Business descriptions are stored in 'Business descriptions text' directory.")
  
  return(output)
}
