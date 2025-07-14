# Package initialization
.onLoad <- function(libname, pkgname) {
  # Load the C++ dynamic library
  tryCatch({
    dyn.load(system.file("libs", "gadget.so", package = "gadget"))
  }, error = function(e) {
    # Only show warning if the package is properly installed
    if (file.exists(system.file("libs", "gadget.so", package = "gadget"))) {
      warning("Failed to load C++ dynamic library: ", e$message)
    }
  })
}

.onUnload <- function(libpath) {
  # Unload the C++ dynamic library when package is unloaded
  tryCatch({
    dyn.unload(system.file("libs", "gadget.so", package = "gadget"))
  }, error = function(e) {
    # Ignore errors during unloading
  })
} 