
# ---
# title: "Retrieve data from Epicollect"
# author: "Brendan Casey"
# created: "2024-05-24"
# description: "# This function retrieves data from Epicollect by 
# using a projects API credentials"
# ---

# Arguments:
# cID: The client ID for the Epicollect API.
# secret: The secret key for the Epicollect API.
# proj.slug: The project slug, a unique identifier for the project.
# form.ref: The form reference, a unique identifier for the form.
# branch.ref.1: The first branch reference, a unique identifier for 
# the first branch.
# branch.ref.2: (Optional) The second branch reference, a unique 
# identifier for the second branch.
# branch.ref.3: (Optional) The third branch reference, a unique 
# identifier for the third branch.


get_epi_data <- function(cID, secret, proj.slug, form.ref, 
                                branch.ref.1, branch.ref.2 = NULL, 
                                branch.ref.3 = NULL) {
  # Install and load the necessary packages
  if (!require(httr)) install.packages("httr")
  library(httr)
  if (!require(jsonlite)) install.packages("jsonlite")
  library(jsonlite)
  
  # Get data from epicollect
  res <- POST("https://five.epicollect.net/api/oauth/token",
              body = list(grant_type = "client_credentials",
                          client_id = cID, client_secret = secret))
  http_status(res)
  token <- content(res)$access_token
  
  url.form <- paste0('https://five.epicollect.net/api/export/entries/',
                     proj.slug, '?map_index=&form_ref=', form.ref,
                     '&format=json&per_page=500')
  
  res1 <- GET(url.form, 
              add_headers(Authorization = paste("Bearer", token)))
  http_status(res1)
  ct1 <- fromJSON(rawToChar(content(res1)))
  
  url.branch.1 <- paste0('https://five.epicollect.net/api/export/entries/',
                         proj.slug, '?map_index=0&branch_ref=',
                         branch.ref.1, "&format=json")
  
  res2 <- GET(url.branch.1, 
              add_headers(Authorization = paste("Bearer", token)))
  http_status(res2)
  ct2 <- fromJSON(rawToChar(content(res2)))
  
  ct3 <- NULL
  if (!is.null(branch.ref.2)) {
    url.branch.2 <- paste0('https://five.epicollect.net/api/export/entries/',
                           proj.slug, '?map_index=0&branch_ref=',
                           branch.ref.2, "&format=json")
    
    res3 <- GET(url.branch.2, 
                add_headers(Authorization = paste("Bearer", token)))
    http_status(res3)
    ct3 <- fromJSON(rawToChar(content(res3)))
  }
  
  ct4 <- NULL
  if (!is.null(branch.ref.3)) {
    url.branch.3 <- paste0('https://five.epicollect.net/api/export/entries/',
                           proj.slug, '?map_index=0&branch_ref=',
                           branch.ref.3, "&format=json")
    
    res4 <- GET(url.branch.3, 
                add_headers(Authorization = paste("Bearer", token)))
    http_status(res4)
    ct4 <- fromJSON(rawToChar(content(res4)))
  }
  
  data_list <- list(ct1 = ct1, ct2 = ct2)
  if (!is.null(ct3)) data_list$ct3 <- ct3
  if (!is.null(ct4)) data_list$ct4 <- ct4
  
  data_list
}

# Example usage:
# Enter epicollect credentials
# cID <- "your_client_id"  # client ID
# secret <- "your_client_secret"  # client secret
# 
# # The following arguments can be found via the API tab in the 
# # eipcollect form dashboard. 

# proj.slug <- "your_project_slug"  # project slug
# form.ref <- "your_form_reference"  # form reference
# branch.ref.1 <- "your_branch_1_reference"  # branch reference
# branch.ref.2 <- "your_branch_2_reference"  # branch reference (optional)
# branch.ref.4 <- "your_branch_3_reference"  # branch reference (optional)

# data <- get_epi_data("your_client_id", "your_secret_key", 
#                             "your_project_slug", "your_form_ref", 
#                             "your_branch_ref_1", 
#                             "your_branch_ref_2 (optional)", 
#                             "your_branch_ref_3 (optional)")
