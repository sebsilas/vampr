

set_vamp_variable <- function(os) {

  if(os == "linux") {
    set_linux()
  } else if(os == "osx") {
    set_osx()
  } else if(os == "windows") {
    set_windows()
  } else {
    warning("Only Linux or Windows 64 bit is currently supported")
  }

}



set_windows <- function() {

  # package library path
  pkg_path <- system.file('bin/windows64', package = 'vampr')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- homePath <- paste0(fs::path_home(), 'C:\\Program Files\\Vamp Plugins')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ";")

  Sys.setenv(VAMP_PATH = dirs)
  Sys.getenv("VAMP_PATH")
}

set_osx <- function() {

  # package library path
  pkg_path <- system.file('bin/osx', package = 'vampr')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- paste0(fs::path_home(), '/Library/Audio/Plug-Ins/Vamp')

  # potential library path 2
  vamp_path2 <- '/Library/Audio/Plug-Ins/Vamp'

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, vamp_path2, sep = ":")

  Sys.setenv(VAMP_PATH = dirs)
}


set_linux <- function() {

  # package library path
  pkg_path <- system.file('bin/linux64', package = 'vampr')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path 1
  vamp_path1 <- paste0('/usr/local/lib/vamp')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ":")

  Sys.setenv(VAMP_PATH = dirs)
}

sonic_annotator_construct_args <- function(transform_file, vamp_cmd, file_name, normalise) {
  if(is.null(transform_file)) {
    args <- c("-d",
              vamp_cmd,
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }
  args
}



get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

sonic_annotator_construct_command <- function(args, hidePrint = TRUE, os) {

  if(os == "osx") {
    cmd <- system.file('bin/osx/sonic-annotator', package = 'vampr')
  } else if(os == "windows") {
    cmd <- system.file('bin/windows64/sonic-annotator64.exe', package = 'vampr')
  } else if(os == "linux") {
    cmd <- system.file('bin/linux64/sonic-annotator', package = 'vampr')
  } else {
    warning('OS not supported.')
  }

  if(hidePrint) {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE)
  }
}


list_vamp_plugins <- function() {

  op_sys <- get_os()

  if(op_sys %in% c("osx", "linux", "windows")) {
    set_vamp_variable(op_sys)
  }

  sonic_annotator_construct_command(args = "-l", os = op_sys)
}
