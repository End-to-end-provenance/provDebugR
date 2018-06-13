library(RDataTracker)
library(provParseR)
library(provGraphR)

debug.init <- function(input.data) {
  file.parts <- strsplit(input.data, "\\.")
  file.ext <- file.parts[[1]][[length(file.parts[[1]])]]

  if (file.ext == "R" || file.ext == "Rmd") {
    # OPENISSUE currently not supported due to RDataTracker JSON limitations
    #ddg.run(input.data)
    print("passing of scripts is currently not supported due to RDataTracker JSON limitations")
    #prov.parse(ddg.json(), isFile = F)
  } else if (file.ext == "json") {
    prov.parse(input.data)
  } else {
    warning("Please enter a valid script or prov.json name")
  }

}
