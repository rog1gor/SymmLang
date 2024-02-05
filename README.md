# SymmLang
Interpreter for my own language, implemented in Haskell

Dostarczone funkcjonalności języka :
        + 1. (trzy typy -> Int, Bool, String)
        + 2. (literały, arytmetyka, porównania)
        + 3. (zmienne, przypisanie)
        + 4. (print)
        + 5. (while, if)
        + 6. (funkcje lub procedury, rekurencja)
        + 7. (zmienne read-only i pętla for)
        + 8. (statyczne wiązanie)
                - 09. (przesłanianie)
        + 9. (obsługa błędów wykonania)
        + 10. (funkcje zwracające wartość)
        + 11. (2) (funkcje zagnieżdżone ze statycznym wiązaniem)

W folderze good/ znajdują się przykłady dobrego użycia, a w folderze bad/ złego.

Uruchomienie interpretera:
    1. make
    2. ./interpreter "nazwa-programu"
Czyszczenie plików:
    1. make clean
Uwaga: Pliki w folderze Grammar są CZĘŚCIOWO wygenerowane przy pomocy programu bnfc na gramatyce zapisanej w grammar.cf. Oznacza to, że nie należy go usuwać i ponownie generować, ponieważ zmiany naniesione ręcznie nie zostaną zaaplikowane, co spowoduje problemy przy kompilacji interpretera

Opis języka:
:> Należy pamiętać o ";" na końcach operacji (włącznie z operacjami blokowymi typu while () {};)
:> W języku Symm instrukcje są czytane od góry do dołu i po kolei wykonywane.
:> Zmienne są wiązane statycznie i nie wychodzą poza "scope", w którym zostały stworzone.
:> Nie można jednak przesłaniać nazwy nowymi wartościami. W przypadku próby zdefiniowania zmiennej lub funkcji, która już istnieje, wyrzucony zostanie odpowiedni błąd na standardowe wyjście błędów.
:> Poza trzema podstawowymi typami istnieje typ przechowujący funkcje - FunT. Można przekazać takiemu typowi wcześniej zdefiniowaną funkcję, a następnie odwoływać się do niej jak do zwykłej funkcji.
:> Wspierane jest dodawanie Stringów do Stringów, Stringów do Intów oraz Stringów do Booli.
:> Wspierane jest porównywanie przy pomocy "==" oraz "!=" Stringów i Booli.

Mniej typowa składnia:
:> W przypadku zadeklarowania zmiennej bez zainicjowania jej, przypisana zostanie jej domyślna wartość (Int: 0, Bool: False, String: ""). Typ przechowujący funkcje nie posiada jednak domyślnej wartośc, dlatego w przypadku próby utworzenia typu FunT bez zainicjowania go, zostanie rzucony błąd o braku domyślnej wartości dla typu przetrzymującego funkcje
:> Język udostępnia zmienne typu read-only dla typów Int, Bool oraz String. Deklaracja zmiennej read-only działa jak zwykła deklaracja z inicjalizacją, tylko zamiast "Let" należy użyć "Read". Podobnie jak typ FunT, nie wspierają one wartości domyślnych, zatem składnia np: "Read Int x;", czyli tworzenie zmiennej typu read-only bez inicjalizacji nie jest dozwolone i się nie sparsuje
:> Nie istnieje typ Void
:> Nie istnieje bezparametrowy "return;" ()
:> Rozróżnienie pomiędzy if () {}, a eif () {} else {};
:> Pętla for. Przykładowo następująca pętla: "for (i in 1...10) {};" wykona się od "i" równego 1 do "i" mniejszego od 10. Zmienna "i" jest typu read-only oraz kolejno przyjmie wartości 1,2,...,9 w kolejnych obrotach pętli
:> Podczas definicji funkcji, jako argumenty należy podać jedynie nazwy argumentów (bez typu). Funkcja później może przyjąć dowolne argumenty. Sprawdzanie poprawności typów odbywa się dynamicznie - czyli tak długo jak podana jako argument wartość nie jest sprzeczna z wykonywanymi w funkcji operacjami, to żaden błąd nie zostanie podniesiony, a operacje poprawnie się wykonają
:> Wbudowana funkcja "Print" działa analogicznie do Pythonowego printa, wypisuje kolejne wartości z przekazanej listy argumentów po spacji. Wypisywanie automatycznie kończy się znakiem końca linii.
:> Funkcja Print wspiera typy Int, Bool oraz String zarówno zwykłe jak i read-only
