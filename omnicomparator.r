#Omnicomparator v1.7
#License: CC-BY
#Author: Zdzisław Koczarski MA
# ---
#This application is meant to compare n-grams (sequences of n-word) from the any number of texts with the source text.
#Present version of the app allows only to compare and indicate LITERAL n-grams common for both source and compared text and to save them in form of .csv file.
#Application is based on comparison of words, not lemmas.
# ---
#Application uses packages "stylo" and "magrittr".
# ---

#0. Install R-console (if you haven't any) from: https://cran.r-project.org/
#	Create folders "Source" and "Corpus" in: C:/Users/user/Documents/Omnicomparator/

#1. Install packages and activate the libraries:
	
	requiredPackages = c('stylo','magrittr')
		for(p in requiredPackages){
			if(!require(p,character.only = TRUE)) install.packages(p)
			library(p,character.only = TRUE)
		}
	
#2. Set path for your application folder:

	setwd("C:/Users/user/Documents/Omnicomparator/")
	BaseFolder<-setwd(getwd())
	
#3. Create folders for results:

	if(file.exists("N-gram2")) {
	setwd(BaseFolder)
	} 	else 	{
		for (i in 2:10) {
        NFol <- paste0("N-gram",i)
		dir.create(NFol)
        setwd(paste0("N-gram", i))
        assign(paste0("N-gram", i), setwd(getwd()))
        setwd(BaseFolder)
	}	}

#4. Prepare n-grams from source text:

	setwd("Source")
	source.text<-tolower(unlist(strsplit(readLines(list.files()), "[^A-Za-z]")))
	source.ready<-source.text[nchar(source.text)>0]
	source.ready %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
	setwd("..")
	
	if(exists("source.ready10")) {
		setwd(BaseFolder)
	} 	else 	{
		for(i in 2:10) {
		assign(paste0("Source_Text", i), make.ngrams(source.ready, i))    
	}	}
	
	
#5. Set path to your corpus:

setwd(BaseFolder)
setwd("Corpus")
Corpus<-setwd(getwd())


#6. Start comparison. Application will conduct comparison of n-grams and save the results in .csv files in proper folders:

	for(deka_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(deka_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Deka<-make.ngrams(prepared.text, 10)
		DekaList<-intersect(Source_Text10, Deka)
		DekaPerc<-sum(Deka %in% Source_Text10)/sum(Deka %in% Deka)*100
		setwd(BaseFolder)
		
		if(length(DekaList)>0) {
			setwd("N-gram10")
			write.csv(DekaList, paste0(deka_grams, ".csv"))
			setwd(Corpus)
			message(c(deka_grams, "\t", DekaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
	
	for(ennea_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(ennea_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Ennea<-make.ngrams(prepared.text, 9)
		EnneaList<-intersect(Source_Text9, Ennea)
		EnneaPerc<-sum(Ennea %in% Source_Text9)/sum(Ennea %in% Ennea)*100
		setwd(BaseFolder)
		
		if(length(EnneaList)>0) {
			setwd("N-gram9")
			write.csv(EnneaList, paste0(ennea_grams, ".csv"))
			setwd(Corpus)
			message(c(ennea_grams, "\t", EnneaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}

	for(okto_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(okto_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Okto<-make.ngrams(prepared.text, 8)
		OktoList<-intersect(Source_Text8, Okto)
		OktoPerc<-sum(Okto %in% Source_Text8)/sum(Okto %in% Okto)*100
		setwd(BaseFolder)
		
		if(length(OktoList)>0) {
			setwd("N-gram8")
			write.csv(OktoList, paste0(okto_grams, ".csv"))
			setwd(Corpus)
			message(c(okto_grams, "\t", OktoPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(hepta_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(hepta_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Hepta<-make.ngrams(prepared.text, 7)
		HeptaList<-intersect(Source_Text7, Hepta)
		HeptaPerc<-sum(Hepta %in% Source_Text7)/sum(Hepta %in% Hepta)*100
		setwd(BaseFolder)
		
		if(length(HeptaList)>0) {
			setwd("N-gram7")
			write.csv(HeptaList, paste0(hepta_grams, ".csv"))
			setwd(Corpus)
			message(c(hepta_grams, "\t", HeptaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(hexa_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(hexa_grams), "[^A-Za-z]")))
		B<-raw.text[nchar(raw.text)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Hexa<-make.ngrams(B, 6)
		HexaList<-intersect(Source_Text6, Hexa)
		HexaPerc<-sum(Hexa %in% Source_Text6)/sum(Hexa %in% Hexa)*100
		setwd(BaseFolder)
		
	if(length(HexaList)>0) {
			setwd("N-gram6")
			write.csv(HexaList, paste0(hexa_grams, ".csv"))
			setwd(Corpus)
			message(c(hexa_grams, "\t", HexaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(penta_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(penta_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Penta<-make.ngrams(prepared.text, 5)
		PentaList<-intersect(Source_Text5, Penta)
		PentaPerc<-sum(Penta %in% Source_Text5)/sum(Penta %in% Penta)*100
		setwd(BaseFolder)
		
		if(length(PentaList)>0) {
			setwd("N-gram5")
			write.csv(PentaList, paste0(penta_grams, ".csv"))
			setwd(Corpus)
			message(c(penta_grams, "\t", PentaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}	
			
	for(tetra_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(tetra_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Tetra<-make.ngrams(prepared.text, 4)
		TetraList<-intersect(Source_Text4, Tetra)
		TetraPerc<-sum(Tetra %in% Source_Text4)/sum(Tetra %in% Tetra)*100
		setwd(BaseFolder)
		
		if(length(TetraList)>0) {
			setwd("N-gram4")
			write.csv(TetraList, paste0(tetra_grams, ".csv"))
			setwd(Corpus)
			message(c(tetra_grams, "\t", TetraPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(tri_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(tri_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Tri<-make.ngrams(prepared.text, 3)
		TriList<-intersect(Source_Text3,Tri)
		TriPerc<-sum(Tri %in% Source_Text3)/sum(Tri %in% Tri)*100
		setwd(BaseFolder)
		
		if(length(TriList)>0) {
			setwd("N-gram3")
			write.csv(TriList, paste0(tri_grams, ".csv"))
			setwd(Corpus)
			message(c(tri_grams, "\t", TriPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}

	for(bi_grams in list.files()) {
		raw.text<-tolower(unlist(strsplit(readLines(bi_grams), "[^A-Za-z]")))
		prepared.text<-raw.text[nchar(raw.text)>0]
		prepared.text %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("j", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("ichi", "ihi", .) %>%
			gsub("nq", "mq", .)
		Bi<-make.ngrams(prepared.text, 2)
		BiList<-intersect(Source_Text2, Bi)
		BiPerc<-sum(Bi %in% Source_Text2)/sum(Bi %in% Bi)*100
		setwd(BaseFolder)
		
		if(length(BiList)>0) {
			setwd("N-gram2")
			write.csv(BiList, paste0(bi_grams, ".csv"))
			setwd(Corpus)
			message(c(bi_grams, "\t", BiPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
#7. After finished comparison you have to check manually single files for results. The longer common n-gram the more possible is the quotation.
# Application will be improved in future :)
### END OF CODE ###
	