# Install packages without sources; prevents memory issues in renv projects
options("install.opts" = "--without-keep.source")

pkg_install_binary <- function(pkg, ...) {
  repo = Sys.getenv("R_BINARY_REPO")
  if (is.null(repo) || trimws(repo) == "") {
    stop("R_BINARY_REPO is not set! Put something like 'export R_BINARY_REPO=\"https://packagemanager.posit.co/cran/__linux__/jammy/latest\"' in your .bashrc")
  } else {
    message(sprintf("Using Package Manager binary repository: %s", toString(repo)))
  }
  old = options(repos = c(CRAN = toString(repo)))
  on.exit(options(old), add = TRUE)
  pak::pkg_install(pkg, ...)
}

if (interactive()) {
  options(
    menu.graphics=FALSE, #no popups, use text prompts
    useFancyQuotes = FALSE, # Uses straight ASCII quotes (" and ') instead of typographic “curly” quotes in
    digits = 4, # don't print a million digits
    scipen = 2, # use scientific notation for values >10^7
    help_type = "html", #html help is nicer than the terminal one
    deparse.max.lines = 3L, # reduce output of traceback
    # Disable colored output for this project, because otherwise
    # in error outputs paths with line-nrs are colored and we cannot ctrl-click on them in vscode
    crayon.enabled = FALSE
  )

  # enable persistent history
  histfile = file.path(getwd(), ".Rhistory") # project-local history
  message(sprintf("Loading history for project at %s", histfile))
  Sys.setenv(R_HISTFILE = histfile, R_HISTSIZE = "100000")
  try(utils::loadhistory(histfile), silent = TRUE)
  # Save after every top-level command to avoid loss on abrupt termination
  if (!exists(".savehist_cb", envir = .GlobalEnv)) {
    .savehist_cb = function(expr, value, ok, visible) {
      try(utils::savehistory(histfile), silent = TRUE)
      TRUE
    }
    base::addTaskCallback(.savehist_cb, name = "savehistory")
  }

  # autoload devel packages
  devel_packages = c("devtools", "testthat", "roxygen2", "pak")
  message(sprintf(
    "Loading devel packages: %s",
    paste(devel_packages, collapse = ", ")
  ))
  lapply(devel_packages, library, character.only = TRUE)
  invisible(TRUE)

  # Define some global settings
  options(width = 150)
}

# Hotfix languageserver: ignore virtual URIs for diagnostics
ns = asNamespace("languageserver")
orig = get("diagnose_file", envir = ns)
my_diagnose_file = function(uri, content, is_rmarkdown = FALSE, globals = NULL, cache = FALSE) {
  if (grepl("^(git:|vscode-|gitlens:|scm:)", uri)) {
    return(list())
  }
  # Ensure `.lintr` is respected for unsaved buffers / inline linting.
  #
  # languageserver lints editor buffers via `lintr::lint(path, text = content)`.
  # lintr treats this as "inline data" and (by default) skips parsing settings,
  # which leads to default linters being used and false positives (e.g. `=`).
  if (length(content) == 0) {
    return(list())
  }
  if (is_rmarkdown) {
    if (!any(stringi::stri_detect_regex(content, "```\\{r[ ,\\}]"))) {
      return(list())
    }
  }
  path = languageserver:::path_from_uri(uri)
  if (length(content) == 1) {
    content = c(content, "")
  }
  if (length(globals)) {
    env_name = "languageserver:globals"
    do.call("attach", list(globals, name = env_name, warn.conflicts = FALSE))
    on.exit(do.call("detach", list(env_name, character.only = TRUE)))
  }
  lints = lintr::lint(path, cache = cache, text = content, parse_settings = TRUE)
  diagnostics = lapply(lints, languageserver:::diagnostic_from_lint, content = content)
  names(diagnostics) = NULL
  diagnostics
}
unlockBinding("diagnose_file", ns)
assign("diagnose_file", my_diagnose_file, envir = ns)
lockBinding("diagnose_file", ns)

