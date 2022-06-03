import sys

def catalogoErrores(error):
    print("ERROR NUM: ", error[0])
    if error[0] == 0:
        sys.exit("Error en duplicacion de variables: " + str(error[0]))
    elif error[0] == 1:
        sys.exit("Error en duplicacion de funciones: ", error[0])
    elif error[0] == 2:
        sys.exit("Error: Type mismatch: " + str(error[1]) + "-" + str(error[2]))
    elif error[0] == 3:
        sys.exit("Errors: Fake floor inexistente")
    elif error[0] == 4:
        sys.exit("Error: Type mistmatch en parametros", error[1], error[2])
    elif error[0] == 5:
        sys.exit("Error: Numero de parametros no valido")
    elif error[0] == 6:
        sys.exit("Error: Return en funcion VOID. " + error[1])
    elif error[0] == 7:
        sys.exit("Error: Variable no declarada " + error[1])
    elif error[0] == 8:
        sys.exit("Error tipo invalido en for loop")

