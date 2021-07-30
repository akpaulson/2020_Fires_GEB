### Convenience functions to share across scripts in this repo

# prepend a given path with the path to the data folder
datadir = function(dir) {
  return (paste0(data_dir,dir))
}

