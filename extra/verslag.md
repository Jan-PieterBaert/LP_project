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
