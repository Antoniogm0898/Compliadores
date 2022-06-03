datos = open("newfile.obj").read()

cuadFlag = False
dirFlag = False
constFalg = False
enter = True


lineas = datos.splitlines()

# Vamos a crear una clase para los cuadruplos
cuadArr = []
dirFunc = {"global" : {}}
consTable = {}
currentFunc = ""

class cuadruplos : 
    def __init__(self,op, rightop, leftop,top):
        self.op = op
        self.rightop = rightop
        self.leftop = leftop
        self.top = top


for linea in lineas:

    enter = True
    if linea == "CODIGO INTERMEDIO":
        cuadFlag = True
        enter = False

    if linea == "DIRECTORIO DE FUNCIONES":
        cuadFlag = False
        dirFlag = True
        enter = False

    if linea == "TABLA DE CONSTANTES":
        dirFlag = False
        constFalg = True
        enter = False

    if cuadFlag and enter:
        splitLine = linea.split(",")
        cuadArr.append(cuadruplos(splitLine[0], splitLine[1], splitLine[2], splitLine[3]))

    if dirFlag and enter:
        splitLine = linea.split(" ")
        if currentFunc == "global":
            dirFunc["global"][splitLine[0]] = splitLine[0]
        elif (splitLine[0] != "funcion") and (len(splitLine) > 1):
            print(splitLine)
            dirFunc[currentFunc][splitLine[0]] = splitLine[1]

        if splitLine[0] == "global":
            currentFunc = "global"

        if splitLine[0] == "funcion":
            currentFunc = splitLine[1]
            dirFunc[currentFunc] = {}


        print(splitLine)

    if constFalg and enter:
        splitLine = linea.split(",")
        consTable[splitLine[0]] = splitLine[1]

print(dirFunc)