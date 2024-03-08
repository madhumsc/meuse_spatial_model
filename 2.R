# Function to get the current script path
scriptPath <- function() {
  # Obtain the path of the current script
  script_file <- commandArgs(trailingOnly = TRUE)[1]
  
  # Return the directory name of the script file
  dirname(script_file)
}

# Get the current script path
current_path <- scriptPath()

print(current_path)