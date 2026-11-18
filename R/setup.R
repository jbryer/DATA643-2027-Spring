# List of packages to install. This can be CRAN or Github packages
pkgs <- c(
	'devtools', 
	'tidyverse',
	'causaldata',
	'lavaan',
	'ggdag',
	'rbounds', 
	'tipr', 
	'Matching', 
	'MatchIt',
	'jbryer/psa',
	'jbryer/VisualStats',
	'NickCH-K/causaldata/R/',
	'DoubleML/doubleml-for-r',
	'jbryer/multilevelPSA',
	'jbryer/PSAboot',
	'jbryer/TriMatch',
	'jbryer/VisualStats',
	'jbryer/medley'
)

install.packages('pak')
pak::pkg_install(pgks)

