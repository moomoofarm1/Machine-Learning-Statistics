# Load Required Libraries
library(httr)
library(jsonlite)
library(base64enc)

# -------------------------------
# 1. Configuration Parameters
# -------------------------------

# GitHub Credentials
Sys.setenv(GITHUB_TOKEN = "")  # Token classic, including "repo" scope via https://github.com/settings/tokens
github_token <- Sys.getenv("GITHUB_TOKEN")

# Repository Details
repo_owner <- "moomoofarm1"  # Replace with your GitHub username or organization name
repo_name <- "topics"        # Replace with your repository name

# Target Directory in Repository
repo_target_dir <- '.../test'  # Replace with your desired repository directory

# Local Directory Containing Files to Upload
files_dir <- "/.../test/"  # Replace with your local directory path

# Branch to Upload Files To
target_branch <- "main"  # Replace with your target branch if different

# -------------------------------
# 2. Helper Functions
# -------------------------------

# Function to Ensure the Target Directory Exists
ensure_directory_exists <- function(base_api_url, repo_target_dir, headers, target_branch) {
  # Normalize path (remove double slashes)
  repo_target_dir <- gsub("//", "/", repo_target_dir)
  
  # URL encode the path properly
  encoded_dir_path <- URLencode(repo_target_dir, reserved = TRUE)
  dir_api_url <- paste0(base_api_url, encoded_dir_path)
  
  # Make a GET request to check if the directory exists
  dir_response <- GET(url = dir_api_url, headers)
  
  if (status_code(dir_response) == 200) {
    message(sprintf("📁 Directory '%s' already exists.", repo_target_dir))
  } else if (status_code(dir_response) == 404) {
    message(sprintf("📁 Directory '%s' does not exist. Creating it by uploading a .gitkeep file.", repo_target_dir))
    
    # Construct the path for .gitkeep file
    gitkeep_path <- file.path(repo_target_dir, ".gitkeep")
    gitkeep_path <- gsub("//", "/", gitkeep_path)  # Normalize path
    
    # Encode the content (empty file)
    gitkeep_content_base64 <- base64encode(charToRaw(""))
    
    # Prepare JSON payload
    payload <- list(
      message = "Create directory with .gitkeep",
      content = gitkeep_content_base64,
      branch = target_branch
    )
    
    # Convert payload to JSON
    json_payload <- toJSON(payload, auto_unbox = TRUE)
    
    # Encode the full file path properly
    gitkeep_api_url <- paste0(base_api_url, URLencode(gitkeep_path, reserved = TRUE))
    
    # Upload .gitkeep file
    gitkeep_response <- PUT(url = gitkeep_api_url, headers, body = json_payload, encode = "json")
    
    if (status_code(gitkeep_response) == 201) {
      message(sprintf("✅ Successfully created directory '%s' with .gitkeep.", repo_target_dir))
    } else {
      error_details <- content(gitkeep_response, "parsed", simplifyVector = TRUE)
      error_message <- ifelse(!is.null(error_details$message), error_details$message, "Unknown error")
      message(sprintf("❌ Failed to create directory '%s': %s (Status Code: %s)", 
                      repo_target_dir, error_message, status_code(gitkeep_response)))
    }
  } else {
    error_details <- content(dir_response, "parsed", simplifyVector = TRUE)
    error_message <- ifelse(!is.null(error_details$message), error_details$message, "Unknown error")
    message(sprintf("❌ Failed to check existence of directory '%s': %s (Status Code: %s)", 
                    repo_target_dir, error_message, status_code(dir_response)))
  }
}


# Function to Encode File Content to Base64
encode_file_to_base64 <- function(file_path) {
  file_content <- readBin(file_path, "raw", file.info(file_path)$size)
  base64_content <- base64encode(file_content)
  return(base64_content)
}

# Function to Upload a Single File to GitHub
upload_file <- function(file_path, base_api_url, repo_target_dir, target_branch, headers) {
  file_name <- basename(file_path)
  repo_file_path <- file.path(repo_target_dir, file_name)
  encoded_repo_file_path <- URLencode(repo_file_path, reserved = TRUE)
  file_api_url <- paste0(base_api_url, encoded_repo_file_path)
  file_content_base64 <- encode_file_to_base64(file_path)
  
  payload <- list(
    message = paste("Add", file_name),
    content = file_content_base64,
    branch = target_branch,
    committer = list(
      name = "moomoofarm1",
      email = "46774289+moomoofarm1@users.noreply.github.com"
    )
  )
  
  get_response <- GET(url = file_api_url, headers)
  
  if (status_code(get_response) == 200) {
    file_info <- content(get_response, "parsed", simplifyVector = TRUE)
    current_sha <- file_info$sha
    payload$message <- paste("Update", file_name)
    payload$sha <- current_sha
  } else if (status_code(get_response) != 404) {
    error_details <- content(get_response, "parsed", simplifyVector = TRUE)
    error_message <- ifelse(!is.null(error_details$message), error_details$message, "Unknown error")
    message(sprintf("❌ Failed to check existence of '%s': %s (Status Code: %s)", 
                    file_name, error_message, status_code(get_response)))
    return(NULL)
  }
  
  json_payload <- toJSON(payload, auto_unbox = TRUE)
  
  response <- PUT(url = file_api_url, headers, body = json_payload, encode = "json")
  
  if (status_code(response) == 201) {
    message(sprintf("✅ Successfully uploaded '%s'.", file_name))
  } else if (status_code(response) == 200) {
    message(sprintf("🔄 Successfully updated '%s'.", file_name))
  } else {
    error_details <- content(response, "parsed", simplifyVector = TRUE)
    error_message <- ifelse(!is.null(error_details$message), error_details$message, "Unknown error")
    message(sprintf("❌ Failed to upload '%s': %s (Status Code: %s)", 
                    file_name, error_message, status_code(response)))
  }
}

# -------------------------------
# 3. Main Execution Flow
# -------------------------------

# Verify that the GitHub token is available
if (github_token == "") {
  stop("❗ GitHub token not found. Please set the 'GITHUB_TOKEN' environment variable.")
}

# Construct the base GitHub API URL for repository contents
base_api_url <- sprintf("https://api.github.com/repos/%s/%s/contents/", 
                        repo_owner, 
                        repo_name)

# Set up the HTTP headers with authorization
headers <- add_headers(
  Authorization = paste("token", github_token),
  Accept = "application/vnd.github.v3+json"
)

# Ensure that the target directory exists before uploading files
ensure_directory_exists(base_api_url, repo_target_dir, headers, target_branch)

# Recursively list all files
files_to_upload <- list.files(path = files_dir, full.names = TRUE, recursive = TRUE)

# Iterate through all files and folders
for (file_path in files_to_upload) {
  if (!file.info(file_path)$isdir) {
    # Extract relative path (to keep structure)
    relative_path <- sub(paste0("^", files_dir), "", file_path)
    
    # Normalize slashes
    relative_path <- gsub("\\\\", "/", relative_path)
    repo_file_path <- file.path(repo_target_dir, relative_path)
    repo_file_path <- gsub("//", "/", repo_file_path)
    
    # Extract and normalize folder path
    repo_folder_path <- dirname(repo_file_path)
    repo_folder_path <- gsub("//", "/", repo_folder_path)
    
    # Ensure folder exists
    ensure_directory_exists(base_api_url, repo_folder_path, headers, target_branch)
    
    # Upload file
    upload_file(file_path, base_api_url, repo_folder_path, target_branch, headers)
  }
}
