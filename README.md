# Sudoku

Sudoku konsolowa aplikacja napisana w Haskellu. Jej główne funkcje to:

- umożliwienie użytkownikowi rozwiązania planszy Sudoku
- automatyczne rozwiązywanie planszy zadanej przez użytkownika przez algorytm
- podpowiadanie użytkownikowi ruchu

## Kompilacja

Najprostszym sposobem na skompilowanie projektu jest użycie systemu Cabal. Szczegółowe instrukcje dotyczące instalacji Cabala znaduję się [pod tym linkiem](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package).

Aby zbudować Sudoku należy przejść do folderu głównego repozytorium wykonać następujące instrukcje:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install
```

**Uwaga:** w czasie redagowania tego pliku, Cabal kończył pracę z błędem podczas wykonywania instrukcji `cabal install --only-dependencies`, było to związane z problemami zależności dla biblioteki `hscurses`. Wersja bilbioteki pobrana wprost z repozytorium właściciela jest pozbawiona tych błędów, zatem w razie konieczności zaleca się własnoręczne pobranie `hscurses` i manualną intalację w środowisku sandboxowym, a następnie ponowienie instrukcji powyżej.


Aby manualnie zainstalować `hscurses` należy wykonać następujące instrukcje:
```
$ cd sciezka_do_folderu_z_pobranym_sudoku
$ git clone https://github.com/skogsbaer/hscurses.git
$ cabal install hscurses/hscurses.cabal
```

## Uruchomienie

Aby uruchomić program zainstalowany za pomocą Cabala, nalaży użyć komendy (przykładowe plansze znajdują się w folderze `samples`:

```
.cabal-sandbox/bin/sudoku sciezka_do_planszy
```

## Sterowanie

- strzałki - poruszanie się po planszy
- klawisze 1-9 - wpisanie wartości w kratkę
- spacja - usunięcie wartości z kratki
- h - uzupełnienie pojedynczej losowej kratki przez program
- s - rozwiązanie całej planszy
- c - sprawdzenie, czy plansza została rozwiązana poprawnie
- q - wyjście z programu

## Użyty algorytm

Głównym algorytmem służącym do rozwiązywania plansz oraz podpowiadania ruchów dla użytkownika jest algorytm oparty na metodzie backtrackingu:

```
1. Dla każdego niewypełnionego pola znajdź wszystkie liczby, które można wstawić w dane pole
2. Wybierz pole o najmniejszej ilości liczb możliwych do wstawienia i wstaw pierwszą z nich
3. Uruchom algorytm rozwiązujący na planszy początkowej ze wstawioną liczbą z punktu 2.
  - jeśli rozwiązanie planszy się powiedzie, zwróć rozwiązanie
  - w przeciwnym przypadku weź inną liczbę z możliwych do wstawienia w to pole i ponownie uruchom algorytm rozwiązywania planszy
  - jeśli lista możliwych do wstawienie jest pusta, to plansz nie ma rozwiązania
```