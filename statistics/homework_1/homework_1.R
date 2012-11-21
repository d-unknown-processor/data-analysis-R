# zapisujemy dane z pliku csv do data.frame
dane <- read.csv("homework_1_data.csv", sep = ',')


# wydobywamy "wartość spółek" z data.frame
dane_wartosc <- dane[,2]

# wydobywamy "cena do zysk" z data.frame
dane_cena_do_zysk <- dane[,3]

sigma <- function(x){
   n <- length(x)
   return(sqrt((n-1)/n)*sd(x));

}

# liczymy kwantyle, szóstego typu
kwantyle <- quantile(dane_wartosc, type = 6)

bardzo_male <- subset(dane, Wartość <= as.numeric(kwantyle[2]))

male <- subset(dane, Wartość > as.numeric(kwantyle[2]) & Wartość <= as.numeric(kwantyle[3]))

srednia <- subset(dane, Wartość > as.numeric(kwantyle[3]) & Wartość <= as.numeric(kwantyle[4]))

duze <- subset(dane, Wartość > as.numeric(kwantyle[4]))

# tworzymy macierz 3x4 i wypełniamy ją zerami
# następnie wpiszemy do niej wyniki
# dalszych obliczeń
wynik <- matrix(0, nrow = 3, ncol = 4)

# następujące obliczenia są oczywiste
wynik[1,1] <- round(mean(bardzo_male[,3]), digit = 2)
wynik[2,1] <- round(median(bardzo_male[,3]), digit = 2)
wynik[3,1] <- round(sigma(bardzo_male[,3]), digit = 2)


wynik[1,2] <- round(mean(male[,3]), digit = 2)
wynik[2,2] <- round(median(male[,3]), digit = 2)
wynik[3,2] <- round(sigma(male[,3]), digit = 2)


wynik[1,3] <- round(mean(srednia[,3]), digit = 2)
wynik[2,3] <- round(median(srednia[,3]), digit = 2)
wynik[3,3] <- round(sigma(srednia[,3]), digit = 2)


wynik[1,4] <- round(mean(duze[,3]), digit = 2)
wynik[2,4] <- round(median(duze[,3]), digit = 2)
wynik[3,4] <- round(sigma(duze[,3]), digit = 2)

# i podsumowanie jako date.frame
podsumowanie <- as.data.frame(wynik)
colnames(podsumowanie) <- c("bardzo małe", "małe", "średnie", "duże")
row.names(podsumowanie) <- c("mean", "median", "sigma")
