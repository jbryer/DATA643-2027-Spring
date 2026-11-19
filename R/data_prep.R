


library(EdSurvey)
naep <- readNAEP(path = system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
naep |> showCodebook() |> View()
searchSDF(string = 'private', data = naep)
EdSurvey::downloadTIMSS(root = "data/", years = c(2019), cache = TRUE, verbose = TRUE)
timss <- EdSurvey::readTIMSS(path = 'data/', countries = 'USA', gradeLvl = '8')
