# 2e crm

Program, co počítá datum příštího servisu pro kompresor. To je jádro programu. Dále obsahuje formuláře pro zadávání dat.

## Výpočet dalšího servisu

Na první stránce se zobrazí seznam všech firem a datum dalšího servisu. 

Firma se zobrazí v jednom z těchto stavů u datumu dalšího servisu:

### Nedá se určit
Pokud není u kompresoru vyplněn ani datum uvedení do provozu ani první servis, pak se zobrazí úplně nahoře s *Nedá se určit* - u datumu.

### S datem uprostřed
Se zobrazí buďto
* když není u firmy naplánovaný žádný výjezd - tím pádem se vypočítá - vezme se nejbližší datum ze všech zařízení, které jsou u firmy
* nebo pokud je naplánovaný servis u kterého bylo zaškrtnuto políčko *Jenom nastavit datum*.

### Úplně dole bez data
To se zobrazí buďto
* když je servis u firmy naplánovaný
* pokud je kompresor vyřazený - *archivován*

1. Další servis se vypočíta pro všechny kompresory ve firmě a vezme se ten nejdřívější termín - další termín servisu pro kompresor se počítá podle vzorce: vezme se datum minulého servisu, nebo datum uvedení do provozu, pokud nebyl minulý servis a přičte se k tomu nějaká doba podle vytížení kompresoru a podle toho, jak se má servisovat dle tabulek od výrobce.
