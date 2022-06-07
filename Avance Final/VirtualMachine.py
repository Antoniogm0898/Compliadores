
from Managers.semManager import cuadruplos

def runVM(obj):
    global memoriaGlobal
    cuadruplos = obj[0]
    dirFunc = obj[1]
    consTable = obj[2]
    count = 0
    memoriaGlobal = dirFunc["global"]
    pilaReturn = []
    pilaReturn2 = []
    paramcount = 0
    memoriaLocal = [4000,5000,6000,7000]

    
    memoriaGlobal.update(consTable)

    #Checamos el tipo del id, evaluamos los valores que tiene la memoria correspondiente con su operador y se la agregamos a un temporal
    operadores = ['&&', '||', '<', '>', '==', '>=', '<=', '+', '-', '*', '/', '%']
    while cuadruplos[count].op != "END":
        print(count + 2)
        if cuadruplos[count].op == "GOTOMAIN":
            count = int(cuadruplos[count].top)

        elif cuadruplos[count].op in operadores:
            if cuadruplos[count].op == '+':

                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                #ver si se suman las memorias en un arreglo o es una suma normal
            
                if memoriaGlobal[pl] is None:
                    memoriaGlobal[cuadruplos[count].top] = int(memoriaGlobal[cuadruplos[count].rightop]) + int(cuadruplos[count].leftop)
                
                else:
                
                
                    rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                    print(rightO,"+",leftO)
                    memoriaGlobal[cuadruplos[count].top] = rightO + leftO
            
            elif cuadruplos[count].op == '-':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
            
                memoriaGlobal[cuadruplos[count].top] = rightO - leftO
                

            elif cuadruplos[count].op == '*':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO * leftO
                

            elif cuadruplos[count].op == '/':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO / leftO
                

            elif cuadruplos[count].op == '<':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
            
                memoriaGlobal[cuadruplos[count].top] = rightO < leftO
            

            elif cuadruplos[count].op == '>':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO > leftO
                
                
            elif cuadruplos[count].op == '<=':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO <= leftO
            

            elif cuadruplos[count].op == '>=':

                
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO >= leftO
                
                
            elif cuadruplos[count].op == '||':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                if rightO or leftO:
                    memoriaGlobal[cuadruplos[count].top] = True
                else:
                    memoriaGlobal[cuadruplos[count].top] = False  
            

            elif cuadruplos[count].op == '&&':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                if rightO and leftO:
                    memoriaGlobal[cuadruplos[count].top] = True
                else:
                    memoriaGlobal[cuadruplos[count].top] = False  
            

            elif cuadruplos[count].op == '==':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")

                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])
                memoriaGlobal[cuadruplos[count].top] = rightO == leftO
            
                
            elif cuadruplos[count].op == '%':
                pr,pl,pt = generatepointerMemory(cuadruplos[count].rightop,cuadruplos[count].leftop,"0")
            
                rightO, leftO = generateLeftRight(pr ,memoriaGlobal[pr], 
                                                    pl ,memoriaGlobal[pl])

                

                memoriaGlobal[cuadruplos[count].top] = rightO % leftO
                
                
            count += 1
        elif cuadruplos[count].op == "=":
            pr,pl,pt =  generatepointerMemory(cuadruplos[count].rightop,"0",cuadruplos[count].top)
        
            
            memoriaGlobal[pt] = memoriaGlobal[pr]  #a= 1  1000 = 2000
        
            count += 1

        elif cuadruplos[count].op == "print":
            print(memoriaGlobal)

            pr,pl,pt =  generatepointerMemory("0","0",cuadruplos[count].top)

            print("print",memoriaGlobal[pt])
            
            count += 1
            
        elif cuadruplos[count].op == "return":
            #asignar el return a la memoria de funcion correspondiente e igualar el count para que siga en el cuadruplo siguiente
            memreturn = pilaReturn2.pop()
            memoriaGlobal[memreturn] = memoriaGlobal[cuadruplos[count].top]
            count = pilaReturn.pop()

        elif cuadruplos[count].op == "lee":
            count += 1
        # en los gotos asignamos al contador con el salto correspondiente
        elif cuadruplos[count].op == "Goto":
            count = int(cuadruplos[count].top)

        elif cuadruplos[count].op == "GotoF":
            
            if(memoriaGlobal[cuadruplos[count].rightop] == False):
                count = int(cuadruplos[count].top)
            else:
                count += 1    
        elif cuadruplos[count].op == "GotoV":
            #si es falso actualizar el contador a el salto correspondiente
            if(memoriaGlobal[cuadruplos[count].rightop] == True):
                count = int(cuadruplos[count].top)
            else:
                count += 1
        # A cada parametro se lo igualamos a una nueva memorialocal
        elif cuadruplos[count].op == "PARAMETER":
            memoriaGlobal[getTypeValParam(cuadruplos[count].rightop)] = memoriaGlobal[cuadruplos[count].rightop]
            paramcount +=1
            count += 1
        elif cuadruplos[count].op == "GOSUB":
            #agegar a la pila de return donde se tiene que regresar despues de el return y ageregar a la pila la dirrecion de memoria de la funcion
            memoriaLocal = [4000,5000,6000,7000]
            pilaReturn.append(count + 1)
            pilaReturn2.append(cuadruplos[count].rightop)
            count = int(cuadruplos[count].top) + 1
        
        elif cuadruplos[count].op == "VERIFY":
            count += 1
        elif cuadruplos[count].op == "ERA":
            count += 1
        elif cuadruplos[count].op == "ENDFUNCTION":
            count += 1
        else:
            print("Haz d")




# Estas 2 funciones checan el tipo del id
def getTypeValParam(pos):

    global memoriaLocal
    pos = round((int(pos) / 1000)) % 4

    if pos == 0:
        aux = memoriaLocal[0]
        memoriaLocal[0] += 1
        return (str(aux))
    elif pos == 1:
        aux = memoriaLocal[1]
        memoriaLocal[1] += 1
        return (str(aux))
    elif pos == 2:
        aux = memoriaLocal[2]
        memoriaLocal[2] += 1
        return (str(aux))
    else:
        aux = memoriaLocal[3]
        memoriaLocal[3] += 1
        return (str(aux))


def getTypeVal(pos, val):
    pos = round((int(pos) / 1000)) % 4
   
    if val is None:
        return val
    if pos == 0:
        
        return (int(float(val)))
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

#agarrar la memoria del pointer
def getPointer(memaux):
        global memoriaGlobal
        memaux = memaux.replace("(","")
        memaux = memaux.replace(")","")
        mempos = memoriaGlobal[memaux]

        return mempos
# si es un pointer asignarle la memoria 
def generatepointerMemory(right,left,top1):
    if "(" in right:
            pointerr = getPointer(right)
    else:
            pointerr = right
        
    if "(" in top1:
            pointert = getPointer(top1)
    else:
            pointert = top1

    if "(" in left:
            pointerl = getPointer(left)
    else:
            pointerl = left

    return pointerr,pointerl,pointert