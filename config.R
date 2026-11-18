##### Course Parameters ############################################################################
year <- 2027
semester <- 'Spring'
course <- 'DATA643'
class_day <- 'TBD'
class_time <- ''
instructor <- 'Jason Bryer, Ph.D.'
instructor_email <- 'jason.bryer@cuny.edu'
description <- paste0(course, ' Causality ', semester, ' ', year)
github_user <- 'jbryer'
github_repo <- "DATA643-2027-Spring"
one_minute_paper <- ''
one_minute_paper_results <- ''
formative_assessment <- ''
formative_assessmnet_results <- ''
slack_invite_link <- ''
slack_link <- 'https://cuny-msds.slack.com'
# course_link <- paste0('https://', github_user, '.github.io/', github_repo)
course_link <- paste0('https://', tolower(semester), year, '.data643.net')
meetup_link <- 'https://us02web.zoom.us/j/86571117058?pwd=poBqROAFdBBUPWij6AqAtPf955C0pk.1'
project_signup <- 'https://docs.google.com/spreadsheets/d/1BowBqdwQPw7vEH7e1wpuIa9pz2KUZ6wPpOVXz43BvcU/edit?usp=sharing'

##### Color palettes
palette_qual <- c("#7fc97f", "#386cb0", "#beaed4", "#fdc086", "#ffff99")
cuny_colors <- c('#0033A1', '#FFB71B', '#9A3CB0', '#A3C9FF', '#EA0045', '#45C2B1')

##### Load Common Packages #########################################################################
library("cowplot")
library("dplyr")
library("ggplot2")
library("icons") # remotes::install_github("mitchelloharawild/icons")
library("kableExtra")
library("knitr")
library("latex2exp")
library("psych")
library("qrcode")
library("RColorBrewer")
library("reshape2")
library("rmarkdown")
library("tidyverse")
library("tm")
library("VisualStats") # remotes::install_github('jbryer/VisualStats')
library("wordcloud")

##### Global Configurations ########################################################################

options(
	knitr.kable.NA = ""
)

opts_chunk$set(
	digits = 3, 
	width = 120,
	# The following is to fix a DT::datatable issue with Xaringan
	# https://github.com/yihui/xaringan/issues/293
	htmltools.dir.version = FALSE, 
	htmltools.preserve.raw = FALSE
)

##### Set default chuck options
knitr::opts_chunk$set(
	warning = FALSE,
	message = FALSE,
	error = FALSE,
	fig.width = 12,
	fig.height = 6,
	out.width = '90%',
	fig.align = 'center',
	digits = 3,
	width = 120,
	dev.args = list(bg = 'transparent')
)

##### Configure the default ggplot2 theme.
# This also ensures the background is transparent so it matches the slides
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))
ggplot2::update_theme(
	panel.background = element_rect(fill = "transparent", colour = NA),
	plot.background = element_rect(fill = "transparent", colour = NA),
	legend.background = element_rect(fill = "transparent"),
	legend.box.background = element_rect(fill = "transparent")
)

##### Utility Functions ############################################################################

# This style was adapted from Max Kuhn: https://github.com/rstudio-conf-2020/applied-ml
# And Rstudio::conf 2020: https://github.com/rstudio-conf-2020/slide-templates/tree/master/xaringan
# This slide deck shows a lot of the features of Xaringan: https://www.kirenz.com/slides/xaringan-demo-slides.html
# To use, add this to the slide title:   `r I(hexes(c("DATA606")))`
# It will use images in the images/hex_stickers directory (i.e. the filename is the parameter)
hexes <- function(x) {
	x <- rev(sort(x))
	markup <- function(pkg) glue::glue('<img src="images/hex/{pkg}.png" class="title-hex">')
	res <- purrr::map_chr(x, markup)
	paste0(res, collapse = "")
}

printLaTeXFormula <- function(fit, digits=2) {
	vars <- all.vars(fit$terms)
	result <- paste0('\\hat{', vars[1], '} = ', prettyNum(fit$coefficients[[1]], digits=2))
	for(i in 2:length(vars)) {
		val <- fit$coefficients[[i]]
		result <- paste0(result, ifelse(val < 0, ' - ', ' + '),
						 prettyNum(abs(val), digits=digits),
						 ' ', names(fit$coefficients)[i])
	}
	return(result)
}

ompWordCloud <- function(text) {
	if(length(text) < 1) {
		warning('No text to create a word cloud.')
		return()
	}
	docs <- Corpus(VectorSource(text))
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))
	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	# Remove english common stopwords
	docs <- tm_map(docs, removeWords, stopwords("english"))
	# Remove your own stop word
	# specify your stopwords as a character vector
	docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)
	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)
	dtm <- TermDocumentMatrix(docs)
	m <- as.matrix(dtm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	head(d, 10)
	wordcloud(words = d$word, freq = d$freq, min.freq = 2,
			  max.words=200, random.order=FALSE, rot.per=0.35,
			  colors=brewer.pal(8, "Dark2"))
}

#' Generate Meetup banner
meetup_image <- function(title,
						 date = format(Sys.Date(), '%B %d, %Y'),
						 url = course_link,
						 out_file) {
	# print(here::here())
	cuny_logo <- png::readPNG(paste0(here::here(), '/images/CUNY_SPS_Logo_Wide.png')) |>
		grid::rasterGrob(interpolate = TRUE)
	course_logo <- png::readPNG(paste0(here::here(), '/course_logo.png')) |>
		grid::rasterGrob(interpolate = TRUE)

	ggplot2::ggplot() +
		ggplot2::annotate(geom = 'text', label = likert:::label_wrap_mod(title, width = 20),
						  x = 0, y = 7.5, color = '#0033A1',
				 hjust = 0, size = 14, size.unit = 'pt', fontface = 'bold') +
		ggplot2::annotate(geom = 'text', label = date, x = 0, y = 5.5, color = '#0033A1',
				 hjust = 0, size = 10, size.unit = 'pt') +
		ggplot2::annotate(geom = 'text', label = url, x = 10, y = 0, color = '#FF9822',
				 hjust = 1, size = 6, size.unit = 'pt') +
		ggplot2::annotation_custom(cuny_logo, xmin = 0, xmax = 4, ymin = 0, ymax = 2) +
		ggplot2::annotation_custom(course_logo, xmin = 6, xmax = 10, ymin = 0, ymax = 10) +
		ggplot2::xlim(0, 10) + ggplot2::ylim(0, 10) +
		ggplot2::geom_point() +
		ggplot2::theme_void() +
		ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'white'))
	ggplot2::ggsave(filename = out_file,
		   width = 1280, height = 720, units = 'px', device = 'png')
}
