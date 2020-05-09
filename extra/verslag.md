# Project Con-tac-tix

## Inleiding:

## Interne bord voorstelling
We houden volgende data bij voor een bord:
- de grootte in beide richtingen
- wie aan de beurt is
- wat de oriëntatie is van de 2 borden
- de huidige status van het bord (dit is een getal, waar -100 en 100 een winst aanduiden en de andere waarden ertussen voor de heuristiek zijn)

## Algoritme
We maken gebruik van min-max bomen die tot een diepte van 4 zoeken naar een winnend spel voor de huidige speler, anders gebruiken we ... metrieken om scores van de borden te bepalen.

## Conclusie
- Een AI maken met bomen is niet altijd even makkelijk

## Memoization
Om het effect van memoization te zien voeren we `tests/run_tests.sh` uit en bekijken we hoe lang dit duurt met of zonder memoization per propositie

| Propositie                  | Tijd zonder | Tijd met | Percentage verschil |
| --------------------------- | ----------- | -------- | ------------------- |
| `floodfill/2`               | 660s        | 285s     |                     |
| `get_states/2`              | 350s        | 285s     |                     |
| `get_all_coords_in_bound/2` | 320s        | 285s     |                     |
| `get_extra_border_coords/3` | 350s        | 285s     |                     |
