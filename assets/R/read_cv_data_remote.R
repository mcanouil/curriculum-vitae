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
      message("📡 Loading '", section, "' from GitHub...")
      data <- yaml::read_yaml(file_url)
      message("✅ Success: '", section, "' loaded from remote")
      data
    },
    error = function(e) {
      # Fallback to local file if GitHub fails
      local_file <- paste0("data/sections/", section, ".yaml")
      if (file.exists(local_file)) {
        message("⚠️  GitHub failed for '", section, "' - using local fallback")
        message("   📁 Local file: ", local_file)
        yaml::read_yaml(local_file)
      } else {
        stop(
          "❌ CRITICAL: Could not read '",
          section,
          "' from GitHub and no local fallback found.\n",
          "   🌐 GitHub URL: ",
          file_url,
          "\n",
          "   📁 Local file checked: ",
          local_file,
          "\n",
          "   💥 Error: ",
          e$message
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
  message(
    "🚀 Starting CV data loading from GitHub repository: ",
    github_repo,
    " (branch: ",
    branch,
    ")"
  )

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
    "presentations",
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
        message("❌ Failed to read section: ", section, ". Error: ", e$message)
        cv_data[[section]] <- NULL
      }
    )
  }

  message("🎉 CV data loading complete!")
  return(cv_data)
}
