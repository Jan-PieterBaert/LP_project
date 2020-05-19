# Project Con-tac-tix

## Inleiding:

## Interne bord voorstelling
We houden volgende data bij voor een bord:
- de grootte in beide richtingen
- wie aan de beurt is
- wat de oriëntatie is van de 2 spelers
- de huidige status van het bord (dit is een getal, waar -100 en 100 een winst aanduiden en de andere waarden ertussen voor de heuristiek zijn)

## Algoritme
We maken gebruik van min-max bomen die tot een diepte van 4 zoeken naar een winnend spel voor de huidige speler.

## Conclusie
- Een AI maken met bomen is niet altijd even makkelijk, spelbomen kunnen heel snel heel groot worden.

## Memoization
Om het effect van memoization te zien voeren we `tests/run_tests.sh` uit en bekijken we hoe lang dit duurt met of zonder memoization per propositie

| Propositie            | Tijd zonder | Tijd met | Percentage met/zonder |
| --------------------- | ----------- | -------- | --------------------- |
| `floodfill/2`         | 228s        | 67s      | 29.4%                 |

We zien dat floodfill goed gebruik kan maken van de memoizatie, dit is allicht omdat floodfill zichzelf vele malen gebruikt.
