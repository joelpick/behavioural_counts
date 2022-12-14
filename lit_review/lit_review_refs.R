rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")


####
#--- Functions
####

pasteV <- function(x,sep=" ") do.call(paste,c(as.list(x),sep=sep))

simpleCap <- function(x) {
	x <- tolower(x)
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

firstup <- function(x) {
	x <- tolower(x)
	substr(x, 1, 1) <- toupper(substr(x, 1, 1))
x
}

remove_trailing_s <- function(x) ifelse(substring(x,nchar(x)) == "s", substr(x,1,nchar(x)-1), x)


####
#--- Import Data
####

dat <- subset(read.csv(file="lit_review/lit_review_all_dl_papers.csv",stringsAsFactors=FALSE), Included==1)

names(dat)
nrow(dat)


dat$Authors2 <- sapply(strsplit(dat$Authors,";"), function(x){
	z <- sapply(strsplit(x,", "), function(y){
		y[2] <- paste0(ifelse(nchar(y[2])>1, pasteV(strsplit(y[2],"")[[1]],sep="."), y[2]),".")
		pasteV(y,", ")	
	})
	ifelse(length(z)>1, paste(pasteV(z[1:(length(z)-1)], sep=","),z[length(z)],sep=" \\& "), z)
})

dat$Pages <- with(dat, ifelse(is.na(Page_Start), Article_Number, paste0(Page_Start,"-",Page_End)))


load("lit_review/extracted_lit_review.Rdata")
genus_names <- unique(sapply(na.omit(c(dat2$scientific_name1,dat2$scientific_name1)), function(x) strsplit(x,"\ ")[[1]][1]))



dat$Title2 <- sapply(dat$Title, function(y){
	genus_name <- genus_names[sapply(genus_names, function(x) grepl(tolower(x), firstup(y)))]
	if(length(genus_name)==1) gsub(tolower(genus_name), genus_name, firstup(y))
	else firstup(y)
})

species <- unique(c(dat2$scientific_name1,dat2$scientific_name1))
dat$Title3 <- sapply(dat$Title2, function(y){
	species <- species[sapply(species, function(x) grepl(x, y))]
	if(length(species)>=1) for(i in species) y <- gsub(i, paste0("\\\\textit{",i,"}"),y)
	y
})

refs <- with(dat, paste0(Search_Ref, ". ", Authors2, " (", Year, ") ", Title3, ifelse(substring(dat$Title,nchar(dat$Title))=="?","","."), " \\textit{", sapply(dat$Journal, simpleCap), "},  \\textbf{", Volume, "}, ", Pages, ".\n"))
write(refs, file="lit_review/lit_review_refs.tex")












dat2022 <- subset(read.csv(file="lit_review/lit_review_2022_all_papers.csv",stringsAsFactors=FALSE, na.strings = c("NA","")), Included==1)

names(dat2022)
nrow(dat2022)

dat2022$Year<-2022
dat2022$Authors2 <- sapply(strsplit(dat2022$Authors,";"), function(x){
	z <- sapply(strsplit(x,", "), function(y){
		y[2] <- paste0(ifelse(nchar(y[2])>1, pasteV(strsplit(y[2],"")[[1]],sep="."), y[2]),".")
		pasteV(y,", ")	
	})
	ifelse(length(z)>1, paste(pasteV(z[1:(length(z)-1)], sep=","),z[length(z)],sep=" \\& "), z)
})

dat2022$Pages <- with(dat2022, 
	ifelse(!is.na(Page_Start), paste0(", ",Page_Start,"-",Page_End),
	ifelse(!is.na(Article_Number),paste0(", ",Article_Number),""))
	)
dat2022$Volume <- ifelse(!is.na(dat2022$Volume),paste0(",  \\textbf{", dat2022$Volume, "}"),"")
dat2022$DOI_Link <- ifelse(!is.na(dat2022$DOI_Link),dat2022$DOI_Link,"")

refs2022 <- with(dat2022, paste0(Search_Ref, ". ", Authors2, " (", Year, ") ", Title, ifelse(substring(dat2022$Title,nchar(dat2022$Title))=="?","","."), " \\textit{", sapply(dat2022$Journal, simpleCap), "}", Volume, Pages, ". ",DOI_Link,"\n"))
refs2022
write(refs2022, file="lit_review/lit_review_refs2022.tex")





