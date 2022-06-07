from LexerParser import runLexerParser
from VirtualMachine import runVM
from Managers.objManager import readOBJ


generaOBJ = True
memoria = []


f = open("sort.txt", "r")

fullText =  ""
for x in f:
    fullText = fullText + x

if generaOBJ:
    fileOBJ = runLexerParser(fullText, generaOBJ, memoria)
    obj = readOBJ(fileOBJ)
    print(obj)
    runVM(obj)
else:
    obj = runLexerParser(fullText, generaOBJ, memoria)
    runVM(obj)
