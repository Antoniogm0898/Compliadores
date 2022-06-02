import sys

def catalogoErrores(error):
    if error[0] == 0:
        sys.exit("Error en duplicacion de variables: ", error[0])
    elif error[0] == 1:
        sys.exit("Error en duplicacion de funciones: ", error[0])
    elif error[0] == 2:
        sys.exit("Error: Type mismatch", error[1], error[2])
    elif error[0] == 3:
        sys.exit("Errors: Fake floor inexistente")
