# Manuál na program

## Akce

### Plánování servisů

Na první stránce se zobrazují firmy standartně seřazené podle toho, kde se má nejdříve jet. Počítají se podle tohoto [algoritmu](Manuál.md). 

#### Plánování servisu
Pokud se rozmyslím, že někam chci naplánovat servis, jdu na první stránku, tam vyberu firmu, u které chci naplánovat servis, kliknu na ní. Na další obrazovce kliknu na *Naplánovat servis* - klikem na položky pod nadpisem *Stroj* vyberu, které stroje budou v servisu zahrnuty. Vyberu **Datum** - kdy se servis odehraje, určím kdo tam pojede za servismany - naklikám to tlačítkem *Další servisman*. Do políček můžu přidat nějaké poznámky k servisu - poznámky společné všem strojům napíšu dolů do políčka **Popis servisu**. Položky ke konrétnímu kompresoru dám přímo na řádek ke kompresoru pod nadpis **Plánované úkony**.

Pak kliknu na *Naplánovat*. Hodí mě to na obazovku, kde je seznam naplánovaných servisů. Musí se to tam zobrazit - v jedné z obrazovek - buďto - **Servisujeme my** nebo **Servisují jiné firmy**. Když půjdu na hlavní stránku, tato firma, u které jsem naplánoval servis se zobrazí bez data.

#### Plánování kontaktu
Když zavolám do nějaké firmy a oni mi řeknou že se mám ozvat např. za tři měsíce s tímto servisem, že teď ještě nevědí, tak to udělám stejně jako při **Plánování servisu**, ale s tím rozdílem, že tam zaškrtnu políčko **Jenom nastavit datum**. Pak mi bude tato firma vybíhat na první stránce s tímto datem, takže jí nezapomenu zavolat

#### Přeplánování servisu
Jdu na obrazovku *Naplánované servisy* - na každém řádku je skoro na konci tlačítko se čtverečkem s tužkou - na to kliknu a můžu změnit údaje k servisu, např. **datum**, nebo **servismeny**, kteří tam pojedou.

#### Uzavření servisu
Když se servisáci vrátí ze servisu a já to chci zaznamenat, tak nejprve přidám fotku archivního listu - popsané dole **Přidání servisního listu**. Pak jdu na obrazovku *Naplánované servisy* - na každém řádku je na konci tlačítko se čtverečkem a fajfkou - na to kliknu. Principialně tam můžu vyplnit všechno, co jsem vyplnil při plánování, je tam navíc políčko **Závěry po servisu**, **Doporučení** - kam můžu přidat nějaké zjištění po servisu. Pak kliknu na *Uzavřít* a potom by se u firmy přepočítá, kdy se do ní musí znovu jet. Pak se zobrazí v tom místě na první stránce.

Důležitý je typ servisu, který se zaznamenává, jsou tam tyto možnosti:
* **(I)nstalace**: úvodní namontování kompresorů, od tohoto servisu se odpočítá další servis. Má smysl samozřejmě mít insalaci pro jeden kompresor jednou.
* **(O)prava**: oprava kompresoru znamená, že se neměnily spotřební díly, pracovalo se na poruchách sání, řemenů, motoru, atd. Program bude opravu ignorovat a odpočítá další servis z pravidelného servisu nebo instalace
* **(S)ervis** - pravidelný: znamená, že byly vyměněny spotřební díly - separátor, olejový filtr, vzduchový filtr. Od pravidelného servisu se odpočítá další servis. Pokud bylo v rámci pravidelného servisu uděláno i něco jiného, nevadí, je třeba použít tento, aby se dobře odpočítal další servis.
* **(K)ontrola**: na kompresoru se nedělal nějaký zásah, jenom se zjistilo, jak běží. Zjistily se údaje o motohodinách atd. Toto také, jako oprava neovlivní výpočet dalšího servisu.

#### Smazání servisu
Jdu na první stránku, tam vyberu firmu u které chci něco smazat, u ní kliknu na **Historie servisů**. Tam kliknu na políčko **Povolit smazávání**. Tím se aktivuje smazávání servisů - to je pro jistotu, aby jsem se nepřeklikl když nechci smazat servis. Kouknu se na datum nahoře v tabulce a kliknu na *Smazat* - to smaže servis.

Když chci smazat jenom obrázek, tak postupuju stejně jako v předchozí situaci, akorát místo toho, abych klikal na smazat, kliknu na ikonku obrázku u konkrétního datumu. Tam se mi zobrazí servisní listy a já můžu smazat nějaký servisní list, který tam nepatří. Vždycky se maže ten obrázek pod tlačítkem - je na něm šipka dolů.

### Přidání položek

**Firma**: Jdu na položku *Seznam firem*, kliknu na tlačítko *Přidat firmu*, vyplním formulář, dám *Uložit*

**Servisman**: Jdu na položku *Servismani*, kliknu na tlačítko *Přidat servismana*, vyplním formulář, dám *Přidat servismana*

**Kompresor**, **Sušičku**, nebo jiné zařízení: Jdu na hlavní stranu, tam kliknu na firmu, u které chci zařízení přidat. Mezi kartami dole kliknu u poslední na *Přidat zařízení*. Dostanu se na formulář, kde vyberu typ zařízení. U šroubových kompresorů nastavím, intervaly servisů, například u remezy dám 2 servisní řady - jednu po 500mth, druhou po 4000mth. U řady s prvním servisem zaškrtnu políčko *První servis*. Tyto servisní řady jsou důležité, protože se pak podle nich bude odvíjet, kdy se kompresor bude servisovat - kdy bude vybíhat na první straně. Na další straně vyplní další údaje, zejména tyto:
- **Kontaktní osoba**, buďto vyberu už stávající přidanou, nebo vyplním tři políčka. Pak bude jasné, komu volat pro servis tohoto kompresoru. 
- **Výrobní číslo**, unikátní kód, kterým se dá kompresor/zařízení identifikovat, je na štítku kompresoru.
- **Datum uvedení do provozu** - důležité u nových kompresorů, protože se od toho odvíjí, kdy se bude dělat první servis
- **Provoz** - podle toho, jaký je provoz kompresorů u firmy se bude určovat, jak často k nim budeme jezdit
- **Označení** - označení kompresoru - zejména u provozů, kde je více kompresorů je třeba se v nich nějak vyznat a dát jim onačení *K1*, *K2* atd.
- **Archivován** - zaškrtnu, když už nechci, aby se mi kompresor zobrazoval na první straně, protože se o kompresor nestaráme - buď byl vyřazen, prodán, nebo ho servisuje konkurence atd.
- **Kdo servisuje** - toto ovlivňuje, v které ze sekcí v *Naplánovaných servisech* se bude zařízení objevovat.
- **Další specifikace** - označení kompresoru - na kolik je nastavený barů, jakou má nádobu atd.

**Speciální pole**: Jdu na položku *Speciální pole* - tam vyberu u kterého druhu zařízení chci přidat políčko - např. u *Adsobční sušičky* - chci aby bylo políčko *Filtr* - které určí, jaký filtr má být před a za *sušičkou*. Potom můžu u každé adsopční sušičky zaznamenávat tento údaj.

**Přidání servisního listu (iPhone)**: Přes telefon (prohlížeč chrome) jdu na položku *Fotky servisů* - tam vyberu, ke kterému servisu chci přidávat fotku. Kliknu na *Přidat fotky* a na další obrazovce kliknu na *Vyber obrázek*. Pak v iPhonu kliknu na *Pořídit snímek/video*, vyfotím arch a dám *Nahrát fotku* - modré tlačítko.
**Přidání servisních listů (sken)**: Naskenuju servisní listy na skenu, takže mi vznikne *.pdf* soubor s mnoha servisními listy. Jdu na položku *Fotky servisů*, dostanu se na daný servis a tam kliknu na *Vyber obrázek* - vyberu vytvořený *.pdf* soubor a kliknu na *Nahrát fotku*. Tím pádem budou u servisu nahrané servisní listy.

### Úprava položek

**Firmu** můžu editovat snadno: na první stránce vyberu firmu, kliknu na ní, na další obrazovce kliknu na editovat a tam se mi stránka promění na formulář, kde vyplním údaje a dám *Uložit*

**Servismena** můžu editovat podobně, jdu na *Servismeni*, vyberu konkrétního a upravím, co chci upravit, dám potom **Ulož**. 

#### Úprava stroje

Jdu na první stranu, potom vyberu firmu, tam se zobrazí seznam zařízení - max. 3 na řádek v kartách. Tam kliknu na název zařízení, které chci editovat. Zobrazí se mi seznam údajů k zařízení, jako **Druh zařízení** atd. Kliknu na *Jdi do editačního módu* a rázem se objeví formulář. Tam můžu editovat údaje, které chci editovat. 

Když chci editovat **Typ zařízení**, musím vybrat nějaký typ, který už existuje. Pokud chci změnit zařízení na typ, který neexistuje, nejdříve ho vytvořím pro firmu **test** na první stránce, která se k ničemu nepoužívá, pak tento typ můžu použít při editaci.

#### Úprava typu stroje

Jdu na stránku *Editace typů zařízení*, tam vyberu typ zařízení, který chci editovat. Kliknu na něj a změním údaje. To se promítne u všech zařízení! Např. pokud je **REMEZA BK-20** u 12-ti firem, tak se změna projeví u všech těchto firem.
