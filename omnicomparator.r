#Omnicomparator v1.5
#Program na licencji CC-BY
#Autor: Zdzisław Koczarski
# ---
#Niniejszy program służy do porównywania n-gramów dowlnego tekstu z wybranym korpusem źródeł.
#Obecna wersja programu służy jedynie do wykazania, że dla porównywanych tekstów istnieją wspólne n-gramy, zaprezentowania tej informacji w konsoli oraz zapisania wyników w postaci pliku .csv.
#Założenie przyświecające temu przedsięwzięciu jest proste - jeżeli autor korzysta ze źródeł bezpośrednio (tj. cytuje je w niezmienionej formie), porównanie n-gramów, zwłaszcza tetragramów i wyżej powinno wykazać przejątki.
#Analiza bi- i trigramów przyniesie raczej mniejsze efekty (ze względu na wysoką częstotliwość występowania np. wyrażeń przyimkowych), ale może dać nam ogólne pojęcie o podobieństwie porównywanych tekstów.
#Analiza monogramów ma przedstawić, jak wiele wspólnych słów mają ze sobą porównywane teksty.
#Program bazuje TYLKO na wyrazach, nie lemmatach.
# ---
#Program korzysta z pakietów "stylo" oraz "magrittr".
# ---


#1. Zainstaluj pakiet i uruchom biblioteki.
	
	install.packages("stylo")
	install.packages("magrittr")
	
	library(stylo)
	library(magrittr)
	
#2. Podaj ścieżkę folderu, w którym rozpakowałeś program. W naszym wypadku jest to:

	setwd("C:/Users/user/Documents/Omnicomparator/")
	BaseFol<-setwd(getwd())
	
#3. Stwórz foldery, w których zostaną zapisane wyniki.

	if(file.exists("N-gram1")) {
	setwd(BaseFol)
	} 	else 	{
		for (i in 2:10) {
        NFol <- paste0("N-gram",i)
		dir.create(NFol)
        setwd(paste0("N-gram", i))
        assign(paste0("N-gram", i), setwd(getwd()))
        setwd(BaseFol)
	}	}

#4. Przygotuj n-gramy z tekstu źródłowego

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
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
	setwd("..")
	
	if(exists("source.ready10")) {
		setwd(BaseFol)
	} 	else 	{
		for(i in 2:10) {
		assign(paste0("Source_Text", i), make.ngrams(source.ready, i))    
	}	}
	
	
#5. Podaj ścieżkę folderu, w którym znajdują się teksty do porównywania.

setwd(BaseFol)
setwd("Corpus")
Corpus<-setwd(getwd())


#6. Zaczynamy porównywanie. Możemy wybrać interesujący nas zestaw n-gramów (monogramy, bigramy itp.) lub uruchomić wszystkie po kolei.
#Program będzie przeprowadzał operacje i zapisywał poszczególne pliki .csv we wcześniej przygotowanych folderach.
#Żeby uniknąć przeklikiwania wszystkich plików możemy sprawdzać w konsoli, jaki jest procentowy udział wspólnych n-gramów w porównywanych tekstach.
#(UWAGA: poniższy kod można prawdopodobnie zamknąć w jakiejś pętli 'for', jeszcze nie wymyśliłem, jak to skutecznie zrobić.)

	for(bi_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(bi_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Bi<-make.ngrams(B, 2)
		BiList<-intersect(Source_Text2, Bi)
		BiPerc<-sum(Bi %in% Source_Text2)/sum(Bi %in% Bi)*100
		setwd(BaseFol)
		
		if(length(BiList)>0) {
			setwd("N-gram2")
			write.csv(BiList, paste0(bi_grams, ".csv"))
			setwd(Corpus)
			message(c(bi_grams, "\t", BiPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(tri_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(tri_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Tri<-make.ngrams(B, 3)
		TriList<-intersect(Source_Text3,Tri)
		TriPerc<-sum(Tri %in% Source_Text3)/sum(Tri %in% Tri)*100
		setwd(BaseFol)
		
		if(length(TriList)>0) {
			setwd("N-gram3")
			write.csv(TriList, paste0(tri_grams, ".csv"))
			setwd(Corpus)
			message(c(tri_grams, "\t", TriPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}

	for(tetra_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(tetra_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Tetra<-make.ngrams(B, 4)
		TetraList<-intersect(Source_Text4, Tetra)
		TetraPerc<-sum(Tetra %in% Source_Text4)/sum(Tetra %in% Tetra)*100
		setwd(BaseFol)
		
		if(length(TetraList)>0) {
			setwd("N-gram4")
			write.csv(TetraList, paste0(tetra_grams, ".csv"))
			setwd(Corpus)
			message(c(tetra_grams, "\t", TetraPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(penta_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(penta_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Penta<-make.ngrams(B, 5)
		PentaList<-intersect(Source_Text5, Penta)
		PentaPerc<-sum(Penta %in% Source_Text5)/sum(Penta %in% Penta)*100
		setwd(BaseFol)
		
		if(length(PentaList)>0) {
			setwd("N-gram5")
			write.csv(PentaList, paste0(penta_grams, ".csv"))
			setwd(Corpus)
			message(c(penta_grams, "\t", PentaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}	
		
	for(hexa_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(hexa_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Hexa<-make.ngrams(B, 6)
		HexaList<-intersect(Source_Text6, Hexa)
		HexaPerc<-sum(Hexa %in% Source_Text6)/sum(Hexa %in% Hexa)*100
		setwd(BaseFol)
		
	if(length(HexaList)>0) {
			setwd("N-gram6")
			write.csv(HexaList, paste0(hexa_grams, ".csv"))
			setwd(Corpus)
			message(c(hexa_grams, "\t", HexaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(hepta_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(hepta_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Hepta<-make.ngrams(B, 7)
		HeptaList<-intersect(Source_Text7, Hepta)
		HeptaPerc<-sum(Hepta %in% Source_Text7)/sum(Hepta %in% Hepta)*100
		setwd(BaseFol)
		
		if(length(HeptaList)>0) {
			setwd("N-gram7")
			write.csv(HeptaList, paste0(hepta_grams, ".csv"))
			setwd(Corpus)
			message(c(hepta_grams, "\t", HeptaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(okto_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(okto_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Okto<-make.ngrams(B, 8)
		OktoList<-intersect(Source_Text8, Okto)
		OktoPerc<-sum(Okto %in% Source_Text8)/sum(Okto %in% Okto)*100
		setwd(BaseFol)
		
		if(length(OktoList)>0) {
			setwd("N-gram8")
			write.csv(OktoList, paste0(okto_grams, ".csv"))
			setwd(Corpus)
			message(c(okto_grams, "\t", OktoPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(ennea_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(ennea_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Ennea<-make.ngrams(B, 9)
		EnneaList<-intersect(Source_Text9, Ennea)
		EnneaPerc<-sum(Ennea %in% Source_Text9)/sum(Ennea %in% Ennea)*100
		setwd(BaseFol)
		
		if(length(EnneaList)>0) {
			setwd("N-gram9")
			write.csv(EnneaList, paste0(ennea_grams, ".csv"))
			setwd(Corpus)
			message(c(ennea_grams, "\t", EnneaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
		
	for(deka_grams in list.files()) {
		A<-tolower(unlist(strsplit(readLines(deka_grams), "[^A-Za-z]")))
		B<-A[nchar(A)>0]
		B %<>%
			gsub("tio", "cio", .) %>%
			gsub("tiu", "ciu", .) %>%
			gsub("tie", "cie", .) %>%
			gsub("tia", "cia", .) %>%
			gsub("tii", "cii", .) %>%
			gsub("ae", "e", .) %>%
			gsub("oe", "e", .) %>%
			gsub("y", "i", .) %>%
			gsub("k", "c", .) %>%
			gsub("v", "u", .) %>%
			gsub("j", "i", .)
		Deka<-make.ngrams(B, 10)
		DekaList<-intersect(Source_Text10, Deka)
		DekaPerc<-sum(Deka %in% Source_Text10)/sum(Deka %in% Deka)*100
		setwd(BaseFol)
		
		if(length(DekaList)>0) {
			setwd("N-gram10")
			write.csv(DekaList, paste0(deka_grams, ".csv"))
			setwd(Corpus)
			message(c(deka_grams, "\t", DekaPerc, "%"))
		} else {
			setwd(Corpus)
		} 	}
	
#7. Po skończeniu kompilacji możemy manualnie przeglądać wyniki w celu odnalezienia wspólnych dla porównywanych tekstów n-gramów. Na tej podstawie możemy wnioskować o ewentualnym cytowaniu źródeł przez autora.
#Program jest niedoskonały z kilku względów. Po pierwsze, w tej postaci potrafi jedynie zaprezentować wyniki w celu dalszej, manualnej ich analizy. Po drugie, wbudowany korpus jest stosunkowo niewielki.
#Jest jednak dobrą podstawą do prostych badań porównawczych. 
### END OF CODE ###
	