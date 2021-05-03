# lista danych wejsciowych

# przykladowe wejscie
inputDataList <- list(
  
                        nbItems = as.integer(4),
                        weights = as.integer(c(1, 2, 3, 4)),
                        values = as.integer(c(5, 6, 7, 8)),
                        knapsackBound = as.integer(10)
                        # moj zamysl jest taki: nazwy parametrow beda brane z nazw rekordow listy; wartoscia danego rekordu jest
                        # wartosc parametru badz wektor/array wartosci (w zaleznosci od dlugosci bedziemy )
                      )


# przykladowe uzycie dla powyzszej listy - funkcja i plik_LSP zdefiniowane ponizej
generateInput(inputDataList, plik_LSP)


# NIE ZDAZYLAM OBCLUZYC ARRAYOW, TO NIE DO KONCA DZIALA!
#       # drugie przykladowe - dla arrayow; to jeszcze wymaga dopracowania!
#       inputDataList <- list(
#         
#         exampleArray = array(as.integer(1:10), c(2,5)) #sprawdzamy dzialanie dla tablicy
#       )


nazwaPlikuLSP <- "zadanie.lsp"

setwd("C:\\Users\\user\\Dysk Google\\Nowy pakiet do optymalizacji")
plik_LSP <- file.path("C:\\Users\\user\\Dysk Google\\Nowy pakiet do optymalizacji", nazwaPlikuLSP) 
#pewnie potem sie zmieni na jakas sensowniejsza sciezke



# INPUT - to dziala np dla pierwszej listy (same stałe i wektory)

generateInput <- function(inputDataList, plikLSP){ # argumenty funkcji: lista parametrów + ścieżka do pliku lsp

  plikPomocniczy <- file.path(getwd(), "dane.txt") # dac mozliwosc zmiany nazwy pliku/sciezki?
  
  plikLSP <- file(description=plik_LSP, encoding="utf-8", open="w")
  plikZDanymiWejsciowymi <- file(description=plikPomocniczy, encoding="utf-8", open="w")
  
  
  cat("", file = plikLSP)
  cat("", file = plikZDanymiWejsciowymi)
  cat("function input() {\n", file = plikLSP, append=TRUE)
  cat("\tif (input == nil) error(usage);", file = plikLSP, append=TRUE)
  cat("\n\ninFile = openRead(input);", file = plikLSP, append=TRUE)
  
  
  # wypisuje dane z listy danych wejsciowych do pliku pomocniczego + wypisuje input
  for (nazwaParametru in names(inputDataList)){
    
    obiekt <- inputDataList[[nazwaParametru]]
    #rozpoznanie typu parametru
    wymiary <- dim(obiekt)
    if(is.null(wymiary)){ #mamy do czynienia z wektorem 1-wymiarowym/liczba
      
      if(class(obiekt) == "integer"){
        polecenieZczytania = "readInt"
      }else{
        polecenieZczytania = "readDouble"        
      }
      
      dlugoscWektora <- length(obiekt)
      
      if(dlugoscWektora == 1){
        
        cat(sprintf("%d\n", obiekt), file = plikZDanymiWejsciowymi, append = TRUE)
        napis <- sprintf("\n\t%s = %s(inFile);", nazwaParametru, polecenieZczytania)
        
      }else{ #wektor 1-wymiarowy dlugosci >=2
        
        cat(paste(paste(obiekt, collapse = " "), "\n", sep = ""), file = plikZDanymiWejsciowymi, append = TRUE)
        ostatniaWspolrzedna <- dlugoscWektora - 1
        napis = sprintf("\n\t%s[i in 0..%d] =  %s(inFile);", nazwaParametru, ostatniaWspolrzedna, polecenieZczytania)
      }
      
    }else{ #przekazany zostal array o >=2 wymiarach
      
      liczbaWymiarow <- length(wymiary)
      napis <- nazwaParametru
      
      # nalezy sprawdzic typ elementow w arrayu - obecnie robie to brzydko ale nie mam pomyslu jak zrobic lepiej
      plikPomocniczy <- "pom.R"
      
      WspolrzednaPierwszegoElementu <- paste(rep("1,", liczbaWymiarow - 1), collapse = " ")
      WspolrzednaPierwszegoElementu <- paste(WspolrzednaPierwszegoElementu, "1")
      cat(sprintf("KlasaObiektu <- class(%s[%s])", nazwaParametru, WspolrzednaPierwszegoElementu), file=plikPomocniczy)
      source(plikPomocniczy)
      
      # tu nalezy usunac plikPomocniczy!!!
      
      if(KlasaObiektu == "integer"){
        polecenieZczytania = "readInt"
      }else{
        polecenieZczytania = "readDouble"        
      }
      
      # tu zostal jeszcze zapis pliku z danymi wejsciowymi!!!
      
      # chyba to ponizej jednak nie jest dobrze! - sprawdzic, ale chyba potrzeba tu wypisac kilka forow.
      
      #wypisanie danych do pliku txt
      #plikPomocniczy <- "pom.R" - juz jest definiowany
      
      pom <- character()
      #wypisanie polecenia zczytania danych do pliku lsp
      for(i in 1:liczbaWymiarow - 1){

        liczbaWspolrzednychWUstalonymWymiarze = dim(obiekt)[i]
        cat(paste("\n", paste(rep("\t", i), collapse="")), file = plikLSP, append = TRUE)
        cat(sprintf("for[i%d in 1..%d]", i, dim(obiekt)[i]), file = plikLSP, append = TRUE)
        
      }
      
      
      
      for(i in 1:liczbaWymiarow){

        maxWspolrzednaNaWymiarze <- dim(obiekt)[i] - 1
        napis <- sprintf("%s[0..%d]", napis, maxWspolrzednaNaWymiarze)
        pom[i] <- sprintf("[i%d]", i)
      }
#      komendaWPetli <- sprintf("%s%s = %(inFile)", nazwaParametru, paste(pom))
      
      napis <- sprintf("%s%s = %s(inFile);", napis, polecenieZczytania, paste(pom, collapse = ""))
      
    }
    
    #pewnie nalezaloby dodac wyjatki na wypadek blednych danych wejsciowych
    
    cat(napis, file = plikLSP, append=TRUE)
    
  }
  
  cat("\n}", file = plikLSP, append=TRUE)
  
  close(plikLSP)
  close(plikZDanymiWejsciowymi)
  
}



# MODEL

# to co następnie powinno się pojawić w pliku lsp:
# funtcion model(){
# sformułowanie modelu w języku lsp
#}


# ponizej: zaczelam prace nad generowaniem funkcji z parametrami solvera oraz komendy ktora bedzie wywolywac localsolvera,
# ale to wymaga jeszcze uporzadkowania i dokladniejszego zapoznania sie z parametrami, tym, ktore sa konieczne itd.

# PARAMETRY ZAPISU

# function param(){ ... } # <- lsp

# 
# function param() {
#   if (lsTimeLimit == nil) lsTimeLimit = 60;
#   lsTimeBetweenDisplays = 3;
#   lsNbThreads = 4;
# }


generateParam <- function(sciezkaDoPlikuLSP, timeLimit = NULL, timeBetweenDisplays = NULL, nbThreads = NULL){

#  if(!is.null(timeLimit) || !is.null(timeLimit) || !is.null(timeLimit)){
    
    plik_LSP <- file.path(getwd(), sciezkaDoPlikuLSP)
    cat("\n\nfunction param() {\n", file = plik_LSP, append = TRUE)
    
    # czy czasem nie potrzebujemy jakichs domyslnych wartosci tych parametrow zeby to w ogole zadzialalo???
    
    #chyba time limit jest potrzebny bo inaczej solver sie zapetli - sprawdzic!
    if(in.null(timeLimit)){
      timeLimit = 60;
    }

    cat(sprintf("\n\tif (lsTimeLimit == nil) lsTimeLimit = %d;", timeLimit))      
    
    if(!in.null(timeBetweenDisplays)){
      
      cat(sprintf("\n\tlsTimeBetweenDisplays = %d;", timeBetweenDisplays))      
    }
    
    if(!in.null(timeBetweenDisplays)){
      
      cat(sprintf("\n\tlsNbThreads = %d;", nbThreads))      
    }
    
    cat("\n}\n", file = plik_LSP, append = TRUE)    
#  }
  
}

  
  
# komenda dla solvera, ktora bedzie wywolywana przez system
LimitCzasu = character()
if(length(DaneWejsciowe$limitCzasu) == 0){
  LimitCzasu = ""
}else{
  LimitCzasu = paste("lsTimeLimit=", )
}
#sciezkaDoPlikuZDanymi # - to jest generowane

# chyba jednak nie ma sensu 
komenda <- paste("localsolver ", plikLSP, " inFileName = ", sciezkaDoPlikuZDanymi,
sep=" ")



solverParameterList = list(
  
  sciezkaDoPlikuZModelem = character(), #czy chce sprawdzac juz na tym etapie czy to istnieje? :P
  sciezkaDoPlikuWyjsciowego = character(),
  limitCzasu = numeric(), # + ew. pozostale parametry solvera
)

#solverParameterList - argument dla funkcji generujacej output
