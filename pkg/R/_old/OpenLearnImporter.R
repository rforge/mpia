
library(XML)

#setwd("~/Documents/werkstatt/knowledge-cartography")

fetchOpenlearnUnit <- function ( url="", unit="" ) {

	if (url=="") {
		stop("No url specified!")
	}
	
	if (unit=="") {
		unit = gsub("^.*/([A-Za-z0-9\\_]*)\\_rss\\.xml","\\1",url)
	}
	
	rss = xmlTreeParse(url)
	
	textnodes = getNodeSet(rss$doc$children$rss, "//item/title | //item/description")
	
	texts = NULL
	for (i in 1:length(textnodes)) {
		tag = xmlName(textnodes[i][[1]])
		if (tag == "title") {
			texts = c(texts, xmlValue(textnodes[i][[1]]))
		} else {
			texts[length(texts)] = paste(texts[length(texts)], xmlValue(textnodes[i][[1]]))
		} # if
	} # for
	
	# remove html tags
	texts = gsub("<(.|\n)*?>"," ",texts)
	
	# html2text
	#texts = htmlParse(texts, asText=TRUE )	
	
	# clean up leftovers
	texts = gsub("&copy;"," ",texts)
	texts = gsub("&#x[A-Za-z0-9]*;"," ",texts)
	texts = gsub("\n"," ",texts)
	
	# copyright statement removal
	
	texts = gsub("(Original Copyright   2004 The Open University\\. Now made available within the Creative Commons framework under the CC Attribution   Non\\-commercial licence \\(see http://creativecommons.org/licenses/by-nc-sa/2.0/uk/\\))."," ",texts)
	texts = gsub("(Except for third party materials and otherwise stated \\(see  terms and conditions \\), this content is made available under a  Creative Commons Attribution\\-NonCommercial\\-ShareAlike 2\\.0 Licence   )"," ", texts)
	texts = gsub("(Original Copyright   [0-9]* The Open University\\. Now made available within the Creative Commons framework under the CC Attribution   Non\\-commercial licence \\(see  )", " ", texts)
	texts = gsub("(Copyright   [0-9]* The Open University)"," ", texts)
	# link removal
	texts = gsub("((http|ftp|https)://)([^:/^[:blank:]]+)(:([0-9]+))?(/.*)", " ", texts)
	
	# quotes removel
	texts = gsub("\\\""," ", texts)
	
	# create units of text
	
	texts
	
}

# fetch a couple of courses

tmp = tempfile()
dir.create(tmp)

curDir = getwd()
setwd(tmp)

units = c(
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/1486/B615_1_rss.xml", 
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4528/Y159_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/1615/B120_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3909/GSG_5_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2503/LIB_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4397/BD131_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3986/Y165_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4443/B203_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4528/Y159_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3749/B201_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4426/B204_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/1486/B615_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4427/B629_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4460/B203_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2898/B202_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3562/BU130_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3382/B625_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3747/B625_3_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2719/B855_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3324/B713_5_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2710/B824_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2710/B824_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4415/B716_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2589/B822_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2842/B854_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/3360/B713_4_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2803/B853_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2785/B853_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4520/B835_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2599/B821_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/2953/B823_2_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/1588/B830_1_rss.xml",
"http://openlearn.open.ac.uk/rss/file.php/stdfeed/4417/B716_2_rss.xml"
)

for (i in 1:length(units)) {

	url = units[i]
	unit = gsub("^.*/([A-Za-z0-9\\_]*)\\_rss\\.xml","\\1",url)
	texts = fetchOpenlearnUnit(url)
	
	#if (! file.exists(unit) ) dir.create(unit) else unlink(dir(unit, full.names=TRUE))
	for (t in 1:length(texts)) {
		write(texts[t], file=paste(unit,"(", t,")", sep=""))
	}

} # for a bunch of courses

setwd(curDir)
rm(curDir)
