format_package_url <- function(repo_user, repo_name, where, colour) {
  if (where == "github") {
    url <- paste0("https://github.com/", repo_user, "/", repo_name)
    return(paste0(add_item_logo(url, "github", colour), " [GitHub](", url, ")"))
  } else if (where == "gitlab") {
    url <- paste0("https://gitlab.com/", repo_user, "/", repo_name)
    return(paste0(add_item_logo(url, "gitlab", colour), " [GitLab](", url, ")"))
  } else if (where == "bitbucket") {
    url <- paste0("https://bitbucket.org/", repo_user, "/", repo_name)
    return(paste0(
      add_item_logo(url, "bitbucket", colour),
      " [Bitbucket](",
      url,
      ")"
    ))
  } else {
    # Default to github
    url <- paste0("https://github.com/", repo_user, "/", repo_name)
    return(paste0(add_item_logo(url, "github", colour), " [GitHub](", url, ")"))
  }
}
