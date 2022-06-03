
# Generar los valores para la memoria
def generaMemoria(inputs):
    if inputs == []:
        # Memoria estandar
        return [0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000]
    else:
        memory = [0]
        for memorySize in inputs:
            memory.append(memorySize + memory[-1])
        return memory

# Obtener el indice de memoria
def returnMemoryType(function, type):

    if function == "global":
        return(type + "Global")

    elif function == "operando":
        return(type + "Temp")

    else:
        return(type + "Local")
    