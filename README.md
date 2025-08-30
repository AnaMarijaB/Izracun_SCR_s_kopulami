# Izračun kapitalskih zahtev po Solventnosti II za tveganje premije in rezervacije premoženjskega zavarovanja z uporabo kopul

Ta repozitorij vsbuje empirični del magistrske naloge, dostopne na povezavi --. Naloga obravnava izračun zahtevanega solventnostnega kapitala (SCR - angl. *Solvency Capital Requirement*) za tveganje premije in rezervacije z različnimi metodami, predstavljenimi v teoretičnem delu naloge. 

## Podatki
- **Vir podatkov**: Obrazec S.05.01 (Premije, škode in odhodki po vrstah poslovanja) iz letnih poročil o solventnosti in finančnem položaju slovenskih zavarovalnic in pozavarovalnic (Sava, Triglav, Grawe, Generali, Sava Re, Triglav Re). 
- V mapi `Podatki` so Excel dokumenti po analiziranih letih, ki imajo za vsako podajtje svoj zavihek. Vsa vsakem zavihku je dodana povezava do originalnega dokmenta in staran, kjer se nahaja obrazec S.05.01.02.
- Iz podatkov izračunani kombinirani količniki (CR – angl. *combined ratio*) so shranjeni v datoteki `podatki.csv` in služijo kot vhod za R kodo, kjer z njimi modeliramo tveganje. Kombinirani količnik je definiran kot:

$$
CR_k = \frac{NCE_k + OE_k}{NP_k},
$$

kjer so $NCE_k$ neto odhodki za škode, $OE_k$ stroški poslovanja in $NP_k$ neto zaslužene premije.

- Analiza je omejena na pet segmentov, kjer podatki obstajajo pri vseh zavarovalnicah.

## Koda
- `01_FUNKCIJE_SIMULACIJA.R` – funkcije za simulacijo večrazsežnih porazdelitev z določenimi odvisnostnimi strukturami (kopule).
- `02_ANALIZA.R` – glavni del kode za analizo modelov.
- `03_GRAFICNE_PREDSTAVITVE.R` – poizvedbe za izris grafov in tabel, uporabljenih v nalogi.

## Rezultati
Rezultati so predstavljeni v poglavju **6.4 Rezultati** magistrske naloge.
