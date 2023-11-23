import random

print(
    """Welche Rechnung möchten Sie üben?
[1] Addition/Subtraktion
[2] Multiplikation/Divison ohne Rest
[3] Beides"""
)

rechnungstyp = int(input())
if rechnungstyp > 3 or rechnungstyp < 1:
    print("Falsche Eingabe, Rechnungstyp 3 wird verwendet")
    rechnungstyp = 3
print("Wie viele Fragen wollen sie beantworten?")
anzahl_fragen = int(input())
print("Wie groß sollen die Zahlen maximal sein?")
größte_zahl = int(input())

anzahl_richtige_antworten = 0

for i in range(anzahl_fragen):
    print(f"Frage Nr. {i}")
    num1 = random.randrange(1, größte_zahl, 1)
    num2 = random.randrange(1, größte_zahl, 1)
    if rechnungstyp == 1:
        if random.random() < 0.5:
            operation_zeichen = "+"
            ergebnis = num1 + num2
        else:
            operation_zeichen = "-"
            ergebnis = num1 - num2
    elif rechnungstyp == 2:
        if random.random() < 0.5:
            operation_zeichen = "*"
            ergebnis = num1 * num2
        else:
            operation_zeichen = "/"
            ergebnis = num1 // num2
    else:
        r = random.random() < 0.5
        if r < 0.25:
            operation_zeichen = "+"
            ergebnis = num1 + num2
        elif r < 0.5:
            operation_zeichen = "-"
            ergebnis = num1 - num2
        elif r < 0.75:
            operation_zeichen = "*"
            ergebnis = num1 * num2
        else:
            operation_zeichen = "/"
            ergebnis = num1 // num2
    print(f"{num1} {operation_zeichen} {num2}")
    antwort = int(input())
    if antwort == ergebnis:
        print("Richtig")
        anzahl_richtige_antworten += 1
    else:
        print("Falsch")

print(f"Fragen beendet. Sie haben {anzahl_richtige_antworten} von {anzahl_fragen} richtig beantwortet.")