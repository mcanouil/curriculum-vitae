#' Read CV data from remote YAML files
#' @description
#' This function reads CV data from separate YAML files hosted on GitHub.
#' @param github_repo The GitHub repository in format "username/repo-name"
#' @param branch The branch to read from (default: "main")
#' @param section The section to read (e.g., "contact", "experience", "publications")
#' @return The parsed YAML data for the specified section
read_cv_data_remote <- function(github_repo, section, branch = "main") {
  # Construct the raw GitHub URL
  base_url <- paste0(
    "https://raw.githubusercontent.com/",
    github_repo,
    "/",
    branch,
    "/"
  )
  file_url <- paste0(base_url, section, ".yaml")

  tryCatch(
    {
      # Try to read from GitHub
      yaml::read_yaml(file_url)
    },
    error = function(e) {
      # Fallback to local file if GitHub fails
      local_file <- paste0("data/sections/", section, ".yaml")
      if (file.exists(local_file)) {
        warning("Could not fetch from GitHub, using local file: ", local_file)
        yaml::read_yaml(local_file)
      } else {
        stop(
          "Could not read from GitHub and no local fallback found for: ",
          section
        )
      }
    }
  )
}

#' Read all CV data from remote YAML files
#' @description
#' This function reads all CV sections from separate YAML files hosted on GitHub.
#' @param github_repo The GitHub repository in format "username/repo-name"
#' @param branch The branch to read from (default: "main")
#' @return A list containing all CV data sections
read_all_cv_data_remote <- function(github_repo, branch = "main") {
  sections <- c(
    "contact",
    "profile",
    "skills",
    "languages",
    "education",
    "experience",
    "workshops",
    "awards",
    "publications",
    "oral_presentations",
    "posters",
    "packages"
  )

  cv_data <- list()

  for (section in sections) {
    tryCatch(
      {
        cv_data[[section]] <- read_cv_data_remote(github_repo, section, branch)
      },
      error = function(e) {
        warning("Could not read section: ", section, ". Error: ", e$message)
        cv_data[[section]] <- NULL
      }
    )
  }

  return(cv_data)
}
