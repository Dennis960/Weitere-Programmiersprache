import random


def random_numbers(
    anzahl_zufallszahlen, min: int = 0, max: int = 1000, isTuple: bool = True
):
    zahlen = [random.randrange(min, max + 1, 1) for i in range(anzahl_zufallszahlen)]
    if isTuple:
        return tuple(zahlen)
    return zahlen


def analyze(*values):
    m = min(values)
    l = max(values)
    avg = sum(values) / float(len(values))
    return m, l, avg


def frequencies(list):
    dict = {}
    for obj in list:
        if dict.get(obj) == None:
            dict.update({obj: 1})
        else:
            dict[obj] += 1
    return dict

def print_frequencies(list):
    frequencyDict = frequencies(list)
    for key in frequencyDict.keys():
        print(f"[ {key} ] :: " + ("|" * frequencyDict[key]))