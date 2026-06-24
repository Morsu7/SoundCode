# SoundCode

**Deadline:** 22/08/2026

## Componenti del Gruppo
* Giacomo Biagioni (giacomo.biagioni@studio.unibo.it)
* Federico Morsucci (federico.morsucci@studio.unibo.it)
* Cristian Morbidelli (cristian.morbidelli@studio.unibo.it)
* Tommaso Remedi (tommaso.remedi@studio.unibo.it)

---

## Obiettivo
Implementazione in Scala di un ambiente di live coding musicale ispirato a Strudel, finalizzato alla definizione, interpretazione ed esecuzione di pattern musicali tramite una mini-DSL testuale.

## Descrizione Generale del Progetto
Il progetto prevede la realizzazione di un’applicazione che permetta all’utente di scrivere semplici espressioni musicali testuali, ispirate al paradigma di Strudel/TidalCycles, e di trasformarle in sequenze temporali di eventi sonori. L’applicazione sarà pensata come un ambiente minimale per la composizione algoritmica: l’utente potrà definire pattern ritmici e melodici, modificarli durante l’esecuzione e osservare/ascoltare il risultato prodotto.

Il sistema non intende replicare integralmente Strudel, ma proporne una versione semplificata e realizzabile nel contesto del progetto PPS, concentrandosi soprattutto sulla modellazione funzionale dei pattern, sull’interpretazione della DSL e sulla gestione del flusso temporale degli eventi musicali.

---

## Requisiti di Massima Obbligatori
* **Mini-DSL:** Implementazione di una mini-DSL testuale per la definizione di pattern musicali.
* **Parsing:** Parsing e interpretazione delle espressioni inserite dall’utente.
* **Trasformazioni:** Supporto a semplici trasformazioni sui pattern, ad esempio ripetizione, concatenazione, sovrapposizione o variazione della velocità.
* **Scheduling:** Scheduling di eventi musicali a partire dai pattern definiti.
* **Interfaccia Utente:** Implementazione di una GUI per semplificare l'utilizzo da parte degli utenti.
* **Audio:** Utilizzo di un sintetizzatore e sampler per la riproduzione dei suoni.

## Requisiti di Massima Opzionali
* **I/O File:** Salvataggio e caricamento di pattern musicali.
* **Esportazione:** Esportazione dei pattern generati in un formato semplice, ad esempio file testuale, JSON o MIDI.
* **Impostazioni Musicali:** Configurazione di parametri musicali come BPM, scala musicale o strumento.
* **Multitraccia:** Supporto a più tracce musicali contemporanee.
* **Trasformazioni Avanzate:** Introduzione di trasformazioni più complesse sui pattern.
* **Visualizzazione:** Visualizzazione attraverso la GUI dei pattern.