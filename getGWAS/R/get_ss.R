#' Download metadata of the current GWAS catalog information
#'
#' @description
#' Downloads cleaned metadata information from GWAS catalog of all the studies only including summary statistics
#'
#' @param url.ss The link of the summary statistics file from GWAS Catalog that you wish to download
#' @param out.dir Output directory where the file should be stored
#' @param out.filename Name to give for the downloaded file
#' @return Download the file in the specified folder (out.dir) named as specified in out.filename
#'
#' @examples
#' get_ss('http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90004001-GCST90005000/GCST90004618','output_dir','GCST90004618')
#'@import dplyr
#'@import vroom
#'@import rvest
#'@import stringr
#'@export
get_ss <- function(url.ss, out.dir, out.filename=NULL){
# Get a list of all the folder and subfolder files
  url_files = find.URL.files(url)
  for (dir in grep('.*/$', url_files, value=T)){
    url_files = c(url_files[!url_files==dir], find.URL.files(dir))
  }

  # Keep only txt/tsv/gz files. If there are not, skip the dataset
  url_files = grep('.*\\.[gztsvtxtzip]*$', url_files, value=T)
  if (length(url_files)==0){next}

  # Keep the Summary Statistics file (the file with the biggest size)
  url_files_sizes = sapply(url_files, \(x) as.numeric(httr::HEAD(x)$headers$`content-length`)) %>% unlist()

  # Try to download the data and make sure the format seems to be the expected
  for (file_url in names(sort(url_files_sizes,T))){
    if (endsWith(file_url,'zip')){
      download.file(file_url, basename(file_url))
      ss_file_url = basename(file_url)
      test_df = try(vroom(basename(file_url), n_max=50, show_col_types = F))
    }
    else{test_df = try(vroom(file_url, n_max = 50))
    }
    if (any(class(test_df) %in% c("spec_tbl_df","tbl_df","tbl","data.frame"))){
      ss_file_url = file_url
      break}
  }

  # Partially load the file, decide the column names
  col_names = suppressWarnings(identify_SS_cols(test_df))
  if (any(is.na(col_names[c('SNP','EA','P','BETA','SE')]))){
    test_df = vroom(ss_file_url, show_col_types = F)
    col_names = identify_SS_cols(test_df, as.index = F)

    if (any(is.na(col_names[c('SNP','EA','P','BETA','SE')]))){
      next # If it lacks essential data just forget about it
    }
  }

  # Load only neccessary columns
  df = vroom(ss_file_url, col_select = unname(col_names[c('SNP', 'EA', 'P', 'BETA', 'SE', 'CHR', 'BP')]),
             col_type=c('c','c','n','n','n', 'n', 'n', 'n')) %>%
    `colnames<-`(c('SNP', 'EA', 'P', 'BETA', 'SE', 'CHR', 'BP')) %>%
    mutate(EA = toupper(EA),    # In case the effect allele is not in upper letter
           SNP = gsub('(rs[0-9]*).*$','\\1',SNP))  # Elude potential rsid additions


  # Save the data in the output folder with the name of the access number and the phenotype
  if (is.null(out.filename)){
    output_file_name = paste0(out.dir, '/',
                              filter(gwcat, `STUDY ACCESSION`==GCST) %>%
                                pull(`DISEASE/TRAIT`),'_', GCST, '.txt.gz')
  }
  else{output_file_name = paste0(out.dir, '/', out.filename)}

  fwrite(df, output_file_name, quote=F, row.names=F)
}
