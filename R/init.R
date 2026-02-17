# Package initialization
# The C++ library is loaded automatically by R via useDynLib(gadget) in NAMESPACE,
# which uses the correct extension (.so on Unix, .dll on Windows). Do not manually
# dyn.load("gadget.so") here, as that fails on Windows where the file is gadget.dll.
.onLoad = function(libname, pkgname) {
  # Optional: any other package-level setup can go here
}

.onUnload = function(libpath) {
  # R will unload the dynamic library loaded via useDynLib when the namespace is unloaded
}
