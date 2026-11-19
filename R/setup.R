# List of packages to install. This can be CRAN or Github packages
pkgs <- c(
	'causaldata',
	'devtools', 
	'ForCausality',
	'ggdag',
	'haven',
	'intsvy,',
	'lavaan',
	'Matching', 
	'MatchIt',
	'rbounds', 
	'tidyverse',
	'tipr', 
	'jbryer/psa',
	'jbryer/VisualStats',
	'jbryer/multilevelPSA',
	'jbryer/PSAboot',
	'jbryer/TriMatch',
	'jbryer/VisualStats',
	'jbryer/medley',
	'DoubleML/doubleml-for-r',
	'NickCH-K/causaldata/R/'
)

install.packages('pak')
pak::pkg_install(pgks)

