from tkinter import E
from h11 import SWITCHED_PROTOCOL
from Semantica import cuadruplos
from objManager import readOBJ

cuadruplos, dirFunc, consTable = readOBJ("newfile.obj")
count = 0
memoriaGlobal = dirFunc["global"]

def getTypeVal(pos, val):
    pos = round((int(pos) / 1000)) % 4
    if val is None:
        return val
    if pos == 0:
        return (int(val))
    elif pos == 1:
        return (float(val))
    elif pos == 2:
        return (val)
    else:
        if val == "True":
            return (bool(True))
        else:
            return (bool(False))

def generateLeftRight(rightM, rightV, leftM, leftVal):
    rightO = getTypeVal(rightM ,rightV)
    leftO = getTypeVal(leftM , leftVal)

    return rightO, leftO

memoriaGlobal.update(consTable)

operadores = ['&&', '||', '<', '>', '==', '>=', '<=', '+', '-', '*', '/', '%']
while cuadruplos[count].op != "END":
    if cuadruplos[count].op == "GOTOMAIN":
        count = int(cuadruplos[count].top)

    elif cuadruplos[count].op in operadores:
        if cuadruplos[count].op == '+':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])

            memoriaGlobal[cuadruplos[count].top] = rightO + leftO
            print("+")
        elif cuadruplos[count].op == '-':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            print(memoriaGlobal[cuadruplos[count].rightop], memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO - leftO
            print("-")

        elif cuadruplos[count].op == '*':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO * leftO
            print("*")

        elif cuadruplos[count].op == '/':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO / leftO
            print("/")

        elif cuadruplos[count].op == '<':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            print("Eror aqui")
            memoriaGlobal[cuadruplos[count].top] = rightO < leftO
            print("<")

        elif cuadruplos[count].op == '>':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO > leftO
            print(">")
            
        elif cuadruplos[count].op == '<=':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO <= leftO
            print("<=")

        elif cuadruplos[count].op == '>=':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO >= leftO
            print(">=")
            
        elif cuadruplos[count].op == '||':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            if rightO or leftO:
                memoriaGlobal[cuadruplos[count].top] = True
            else:
                memoriaGlobal[cuadruplos[count].top] = False  
            print("||")

        elif cuadruplos[count].op == '&&':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            if rightO and leftO:
                memoriaGlobal[cuadruplos[count].top] = True
            else:
                memoriaGlobal[cuadruplos[count].top] = False  
            print("&&")

        elif cuadruplos[count].op == '==':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])
            memoriaGlobal[cuadruplos[count].top] = rightO == leftO
            print("==")
            
        elif cuadruplos[count].op == '%':
            rightO, leftO = generateLeftRight(cuadruplos[count].rightop ,memoriaGlobal[cuadruplos[count].rightop], 
                                              cuadruplos[count].leftop ,memoriaGlobal[cuadruplos[count].leftop])

            print(rightO, cuadruplos[count].rightop, cuadruplos[count].leftop, leftO)

            memoriaGlobal[cuadruplos[count].top] = rightO % leftO
            print("%")
            
        count += 1
    elif cuadruplos[count].op == "=":
        memoriaGlobal[cuadruplos[count].top] = memoriaGlobal[cuadruplos[count].rightop]  #a= 1  1000 = 2000
        print("=")
        count += 1

    elif cuadruplos[count].op == "print":
        print(memoriaGlobal[cuadruplos[count].top])
        
        count += 1
        
    elif cuadruplos[count].op == "return":
        print("return")
        count += 1

    elif cuadruplos[count].op == "lee":
        print("lee")
        count += 1

    elif cuadruplos[count].op == "Goto":
        count = int(cuadruplos[count].top)

    elif cuadruplos[count].op == "GotoF":
        if(cuadruplos[count].rightop == False):
            count = int(cuadruplos[count].top)
        else:
            count += 1    
    elif cuadruplos[count].op == "GotoV":
        if(cuadruplos[count].rightop == True):
            count = int(cuadruplos[count].top)
        else:
            count += 1
    elif cuadruplos[count].op == "PARAMETER":
        print("PARAMETER")
        count += 1
    elif cuadruplos[count].op == "GOSUB":
        print("GOSUB")
        count += 1
    else:
        print("Haz d")