#import pandas
import json
#taxo = open("/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/ITIS_plantaeTaxonomy.txt")
taxo = open("Q:/My Drive/Research/APHIS_Pathways/analysis/Plantae_Genus_Synonyms.txt")

familyDict = {}
taxo.readline()
taxo.readline()
taxo.readline()
taxo.readline()

family = ""
for line in taxo:
    line = line.strip()
    line = line.replace("[", ",")
    line = line.replace("]", "")
    line = line.replace(":", ",")
    line = line.split(",")
    if line[0] == "Family":
        family = line[1].strip()
        familyDict[family] = []
    elif line[0] == "Genus":
        n = 1
        for i in line[1:]:
            genus = line[n].strip()
            if genus != "":
                familyDict[family].append(genus)
            n = n+1

jsonFamily = json.dumps(familyDict)
f = open("familyDict.json","w")
f.write(jsonFamily)
f.close()

taxo.close()

taxo = open("Q:/My Drive/Research/APHIS_Pathways/analysis/Plantae_Genus_Synonyms.txt")

orderDict = {}
taxo.readline()
taxo.readline()
taxo.readline()
taxo.readline()

order = ""
for line in taxo:
    line = line.strip()
    line = line.replace("[", ",")
    line = line.replace("]", "")
    line = line.replace(":", ",")
    line = line.split(",")
    if line[0] == "Order":
        order = line[1].strip()
        orderDict[order] = []
    elif line[0] == "Family":
        n = 1
        for i in line[1:]:
            family = line[n].strip()
            if family != "":
                orderDict[order].append(family)
            n = n+1

jsonOrder = json.dumps(orderDict)
f = open("orderDict.json","w")
f.write(jsonOrder)
f.close()

taxo.close()

taxo = open("Q:/My Drive/Research/APHIS_Pathways/analysis/Plantae_Genus_Synonyms.txt")

classDict = {}
taxo.readline()
taxo.readline()
taxo.readline()
taxo.readline()

classTaxo = ""
for line in taxo:
    line = line.strip()
    line = line.replace("[", ",")
    line = line.replace("]", "")
    line = line.replace(":", ",")
    line = line.split(",")
    if line[0] == "Class":
        classTaxo = line[1].strip()
        classDict[classTaxo] = []
    elif line[0] == "Order":
        n = 1
        for i in line[1:]:
            order = line[n].strip()
            if order != "":
                classDict[classTaxo].append(order)
            n = n+1

jsonClass = json.dumps(classDict)
f = open("classDict.json","w")
f.write(jsonClass)
f.close()

