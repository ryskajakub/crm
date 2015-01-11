# 2e crm #

Program, co počítá datum příštího servisu pro kompresor. To je jádro programu. Dále obsahuje formuláře pro zadávání dat.

## Výpočet dalšího servisu ##

Na první stránce se zobrazí seznam všech firem a datum dalšího servisu. 

Výpočet termínu dalšího servisu se provádí podle následujících pravidel:

1. Pokud je ve firmě nějaký naplánovaný servis, pak se jak termín dalšího servisu zvolí datum, které bylo vybráno při plánování. Pozn. je tady riziko, že pokud má firma 2 kompresory a jeden se naplánuje, tak na ten druhý se může zapomenout, vadí to?
1. Jinak se vypočítá další servis pro všechny kompresory ve firmě a vezme se ten nejdřívější termín
1. Další termín servisu pro kompresor se počítá podle vzorce: vezme se datum minulého servisu, nebo datum uvedení do provozu, pokud nebyl minulý servis a přičte se k tomu nějaká doba podle vytížení kompresoru a podle toho, jak se má servisovat dle tabulek od výrobce.

## Zadávání dat ##

Program obsahuje formuláře, kde se zadávají data, tak aby program mohl počítat další servis. Jsou to formuláře pro tyto typy dat:

1. **Firma** - obsahuje základní informace o firmě, adresu atd. Pod firmou jsou vedeny kompresory.
1. **Kompresor** - obsahuje údaje specifické pro ten konrétní stroj - to je o uvedení do provozu, výrobní číslo, atd.
1. **Typ kompresoru** - obsahuje údaje společné pro jeden typ kompresoru, nejdůležitější informace z toho je jak často se kompresor servisuje.
1. **Plán údržby** - obsahuje informace o plánu výjezdu na údržbu - co se bude servisovat, kdo tam pojede, kdy, do jaké firmy
1. **Uzavření údržby** - po uzavření údržby je třeba zaznamenat informace, o tom, jak servis proběhl, kolik motohodin bylo naměřeno na kompresorech
