#' Download metadata of the current GWAS catalog information
#'
#' @description
#' Downloads cleaned metadata information from GWAS catalog website of all the studies including summary statistics
#'
#' @return Dataframe with current GWAS-Catalog summary statistics information (from the web). Takes around 2-3 min
#'
#' @examples
#'
#'@import dplyr
#'@import vroom
#'@import rvest
#'@import stringr
#'@export
get_catalog <- function(){
  # Download all the GWAS Catalog data information (3-5 min)
  options(timeout = 1000)
  gwcat = vroom::vroom("https://www.ebi.ac.uk/gwas/api/search/downloads/alternative", show_col_types = F)

  # Keep only unique data accessions
  gwcat = gwcat %>% dplyr::filter(!duplicated(`STUDY ACCESSION`))

  ## Get all links with Summary Statistics available
  links_all <- XML::getHTMLLinks("http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/", relative = T) %>%
    grep('.*/GCST.*', .,value=T)

  # Keep only GCST-accessions with available summary statistics
  urls.SS = sapply(links_all, \(link){XML::getHTMLLinks(link, relative = T)}) %>%
    unlist(use.names=F) %>%
    grep('/GCST[0-9]+/$', ., value=T)

  # Only keep those studies with available summary statistics to donwload
  # And only keep relevant information
  gwcat = filter(gwcat, `STUDY ACCESSION` %in% gsub('.*/([^/]+)/$','\\1', urls.SS))

  # Keep the URL of the summary statistics folders
  gwcat$url.ss = urls.SS[gsub('.*/([^/]+)/$','\\1', urls.SS) %in% gwcat$`STUDY ACCESSION`]


  ## Create ancestry annotation and sample size annotation. Then filter

  # Define the columns of ancestry and sample size
  gwcat$ancestry = sapply(stringr::str_extract_all(gwcat$`INITIAL SAMPLE SIZE`,
                                                   '[A-Z][a-z]+'),
                          \(x) paste0(sort(unique(x)),collapse=',')
  )

  gwcat$sample_size = gsub('[a-zA-Z,.]','',gwcat$`INITIAL SAMPLE SIZE`) %>%
    stringr::str_extract('([0-9]+)') %>% as.numeric()

  # Standarize column names for R and select only relevant ones
  colnames(gwcat) = gsub(' ','_',tolower(colnames(gwcat)))
  gwcat = dplyr::select(gwcat, `disease/trait`, date_added_to_catalog, pubmedid,
                        first_author, date, journal, link, study, ancestry,
                        sample_size, genotyping_technology, url.ss)

  return(gwcat)
}
