from xml.etree.ElementTree import tostring
import ply.lex as lex
import ply.yacc as yacc
from Semantica import newTV, newVar, apply_cubo_semantico, cuadruplos
from CatalogoErrores import catalogoErrores
from objManager import writeOBJ
from Memoria import generaMemoria, returnMemoryType

# Ejemplo
fileTxt = r"""
    programa proyecto;

    atributos
        int : atributoInt, atributoInt2, a;
        float : atributoFloat, atributoFloat2;
        char : atributoChar, atributoChar2;
        bool : atributoBool, atributoBool2 ;
        int  : atributoArr[20];
        float : atributoMat[20][10] ;

    metodos
        int funcion ejemploCondicional(float atributoInt, bool nans, char ab2, int prueba){
            
            int : atributoInt2,atributoInt3,atributoInt4,atributoInt5,c;

            if ((atributoInt2 >= prueba) && (atributoInt3 <= prueba)){
                escribir(nans)
                atributoInt5 = 2 + 1;
                atributoInt5 = 3 % 4;
                ab2 = 'hola';
            } elif ((atributoInt4 < prueba) || (atributoInt2 == prueba)){
                escribir(atributoInt, ab2)
            } else {
                escribir(atributoInt)
            }

            return (atributoInt)
        }

        void funcion ejemploWhile(bool flag,int g){

            bool : b;
            float : a;
            char : c;
            int : d; 

            g = ejemploCondicional(a,b,c,d);

            b = ejemploCondicional(a,b,c,d);



            while (flag == True) do {
                lee(1.2,1,2)
                a = 1.2 + 2.3;
            }

            do {
                
                a = 1 + 2;
            } while (flag == False)

         
        }

    principal(
        atributoInt = 1;
        escribir(atributoInt)
        atributoInt = 1 + (2 + 10);
        atributoFloat = 3 * ((28 / 4) % 2)*1.2;
        atributoChar = 'Palabra';
        atributoBool = True;

        escribir(atributoInt)
        escribir(atributoFloat)
        escribir(atributoChar)     
        escribir(atributoBool)    
    )
    """

#####################################################################
###################             LEXER             ###################
#####################################################################

# Vamos a crear los tokens para nuestro compilador, para esto nos vamos a apoyar con el lexer de Ply

# Tokens palabras reservadas (El diccionario las convierte de minuscula a mayuscula)
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'elif' : 'ELSEIF',
    'for' : 'FOR',
    'while' : 'WHILE',
    'do' : 'DO',
    'to' : 'TO',
    'programa' : 'PROGRAMA',
    'principal' : 'PRINCIPAL',
    'return' : 'RETURN',
    'lee' : 'LEE',
    'escribir' : 'ESCRIBIR',
    'int'  : 'INT',
    'float' : 'FLOAT',
    'char' : 'CHAR',
    'funcion' : 'FUNCION',
    'void'  : 'VOID',
    'bool' : 'BOOL',
    'atributos': 'ATRIBUTOS',
    'metodos' :'METODOS',  
    'True' : 'TRUE',
    'False' : 'FALSE'
 }

# Agregamos los tokens que se van a definir con regex
tokens = ['ID', 'PLUS' , 'MINUS' ,'MULT','MOD','DIV','EQUALS','COLON','SEMI','COMMA','LPAREN',
         'RPAREN','LBRACKET','RBRACKET','LBRACES','RBRACES','LT', 'LE', 'GT', 'GE', 'EQ',
         'NE','LOR','LAND','CTE_I','CTE_F','CTE_S'] + list(reserved.values())
#, 'RANGE'

# Regex para los tokens
t_ignore = " \t\n"  
t_PLUS = r'\+'
t_MINUS   = r'-'
t_MULT   = r'\*'
t_MOD = r'%'
t_DIV  = r'/'
t_EQUALS = r'='
t_SEMI = r';'
t_COLON = r':'
t_COMMA = r'\,'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACES = r'\{'
t_RBRACES = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NE = r'!='
#t_TP = r'..'
t_LOR = r'\|\|'
t_LAND = r'&&'
t_CTE_I = r'\d+'
t_CTE_F = r'((\d*\.\d+)(E[\+-]?\d+)?|([1-9]\d*E[\+-]?\d+))'
t_CTE_S = r'\'.*?\''


# Vamos a identificar si es un id o una palabra reservada
def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')
     return t

def t_error(t):
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

lex.lex()

#####################################################################
################             ESTRUCTURAS            #################
#####################################################################

# Estructuras para el programa
current_name = None # ID actual
current_type = None # Tipo actual
current_func = [] # Una pila para mostrar la funcion actual
pOper = [""] # Pila de Operandos
pilaO = [] # Pila de Operadores
cuad = [] # Cuadruplos
ptype = [] # Pila de tipos
pjumps = [] # Pila de saltos
pelse = [] # Una pila para el if elif else
vControl = 0 ################################################################################# HAY V CONTROLS QUE NO SE USAN
cont = 0 # Contador de posicion actual
countCuad = 0 # Contador para los saltos en ciclos
parameterCounter = 0 # Contador de cantidad de parametros
tempcount = 0 # Contador de cantida de temporales
pParam = [] # Pila con los tipos de los parametros
paramTable = [] # Tabla de parametros 
pCurrentCall = [] # Pila para manejar la llamada actual
constTable = {} # Tabla de constantes
obj = [] # Arreglo para elementos obj (Cuadruplos, dirFun, consTable)

# Vamos a definir las direcciones iniciales para la memoria virtual
# Para cada modulo de memoria se definen los tipos
particionesDeMemoria = ["intConst", "floatConst", "charConst", "boolConst",
                        "intLocal", "floatLocal", "charLocal", "boolLocal",
                        "intGlobal", "floatGlobal", "charGlobal", "boolGlobal",
                        "intTemp", "floatTemp", "charTemp", "boolTemp"]

# Hacemos la llamada para generar memoria
memoria = generaMemoria([])

# Guardamos las dirreciones dentro de un diccionario
memoriaVir = dict(zip(particionesDeMemoria, memoria))

# Guardamos los valores de los temporales para reiniciar la memoria cuando se deba
memoriaTemp = dict(zip(particionesDeMemoria[4:8], memoria[4:8]))

#####################################################################
###################             PARSER            ###################
#####################################################################

# Programa
def p_programa(p):
    '''
    programa : gotomain PROGRAMA addDir ID SEMI programa2
    '''
    # Una vez que termina de correr el programa creamos el OBJ y borramos el directorio de funciones
    global directorio_fun
    cuad.append(cuadruplos("END","","",""))
    obj.append(cuad)
    obj.append(directorio_fun)
    obj.append(constTable)
    del(directorio_fun)
def p_programa2(p):
    '''
    programa2 : atributos programa2
              | programa3
    '''
def p_programa3(p):
    '''
    programa3 : metodos programa3
              | programa4 
    '''
def p_programa4(p):
    '''
    programa4 : setmainloc principal
    '''

# Atributos
def p_atributos(p):
    '''
    atributos : ATRIBUTOS addDir var
    '''

# Metodos
def p_metodos(p):
    '''
    metodos : METODOS funciones
    '''

# Principal
def p_principal(p):
    '''
    principal : PRINCIPAL LPAREN estatutos principal2
    '''
def p_principal2(p):
    '''
    principal2 : estatutos principal2
               | RPAREN
    '''

# Funciones
def p_funciones(p):
    '''
    funciones : tipo funciones2
              | VOID funciones2
    '''  
def p_funciones2(p):

    '''
    funciones2 : FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF atributos funciones3
               | FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF funciones3
               | FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES var addContF funciones3
    '''
def p_funciones3(p):
    '''
    funciones3 : estatutos RBRACES addDir funciones
               | estatutos RBRACES addDir
               | estatutos funciones3
    '''

# Parametros
def p_parametros(p):
    '''
    parametros : tipo ID setCurrentID addValueVarT newParamType COMMA parametros
               | tipo ID setCurrentID addValueVarT newParamType
    '''
    
# Return
def p_retornar(p):
    '''
    retornar : RETURN returnFlag LPAREN retornar2
    '''
def p_retornar2(p):
    '''
    retornar2 : exp cuadRetornar COMMA retornar2
              | exp cuadRetornar RPAREN
    '''

# Variables
def p_var(p):
    '''
    var : tipo COLON var2
    '''
def p_var2(p):
    ''' 
    var2 : ID setCurrentID addValueVarT var3
         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET var3
         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET LBRACKET CTE_I RBRACKET var3

    '''
def p_var3(p):
    '''
    var3 : COMMA var2
         | SEMI var
         | SEMI    
    '''

# Tipo
def p_tipo(p):
    '''
    tipo : INT
         | FLOAT
         | CHAR
         | BOOL
    '''
    # Symbol Table 4. Se agrega el programa al directorio
    global current_type
    current_type = p[1]

# Exp
def p_exp(p):
    '''
    exp : texp checarexp
        | exp addOL exp 
    '''
def p_texp(p):
    '''
    texp : gexp checarexp
         | texp addOL texp
    '''
def p_gexp(p):
    '''
    gexp : mexp checarexp
         | gexp gexp2 gexp
    '''
def p_gexp2(p):
    '''
    gexp2 : LT addO
          | LE addO
          | GT addO
          | GE addO
          | EQ addO
          | NE addO
    '''
def p_mexp(p):
    '''
    mexp : t checarexp
         | mexp mexp2 
    '''
def p_mexp2(p):
    '''
    mexp2 : PLUS addO mexp 
          | MINUS addO mexp 
    ''' 
def p_t(p):
    '''
    t : f checarexp
      | t t2 
    '''
def p_t2(p):
    '''
    t2 : MULT addO t 
       | DIV addO t 
       | MOD addO t 
    '''
def p_f(p):

    '''
    f : LPAREN addFakeBottom exp RPAREN popFakeBottom
      | CTE_I addIntType
      | CTE_F addFloatType
      | CTE_S addCharType
      | TRUE addBoolType
      | FALSE addBoolType
      | llamada
      | ID 
    '''
    global current_type,current_name, directorio_fun,memoriaVir
    # En caso que se este tratando de hacer una llamada o obtener una variable 
    if len(p) == 2 and p[1] != None:
        # Verificamos que la variable exista en la tabla
        if p[1] in directorio_fun[current_func[-1]]["Vars"]:          
            current_name = p[1] 
            # Agregamos la direccion de memoria a la pila
            pilaO.append(directorio_fun[current_func[-1]]["Vars"][p[1]]["Memoria"])
            if p[1] != None:
                # Si el tipo de llamada no es void se agrega el tipo a la pila
                ptype.append(directorio_fun[current_func[-1]]["Vars"][current_name]["Type"])
        else:
            catalogoErrores([7, p[1]])

# Estatutos
def p_estatutos(p):
    '''
    estatutos : estatutos2 estatutos
              | estatutos2 
    '''
def p_estatutos2(p):
    '''
    estatutos2 : asigna
               | llamada
               | lee
               | escribe
               | condicion
               | ciclow
               | ciclof
               | funciones
               | retornar
    '''

# Asigna
def p_asigna(p):
    '''
    asigna : ID EQUALS asignarid exp SEMI
    '''
    # Vamos a asignar un "valor" a una direccion virtual
    global countCuad, pilaO, directorio_fun
    # Obtenemos que se asigna y donde. Al igual sacamos el = de la pila
    leftO = pilaO.pop()
    newid = pilaO.pop()
    oper = pOper.pop()
    
    # Revisamos que exista la dirreccion en el directorio de variables, si no levanta error
    if p[1] in directorio_fun[current_func[-1]]["Vars"]:
        resultado = directorio_fun[current_func[-1]]["Vars"][newid]["Memoria"]
    else:
        catalogoErrores([7, p[1]])  
    #print(oper,leftO,"",resultado)     
    aux = cuadruplos(oper,leftO,"",resultado)
    cuad.append(aux)
    countCuad += 1

# Llamada
def p_llamada(p):
    '''
    llamada : ID callFunc LPAREN exp genParameter llamada2
    '''
def p_llamada2(p):
    '''
    llamada2 : COMMA paramCounter exp genParameter llamada2
             | verLastParam RPAREN
    '''

# Lectura
def p_lee(p):
    '''
    lee : LEE LPAREN exp cuadlee lee2
    '''
def p_lee2(p):
    '''
    lee2 : COMMA exp cuadlee lee2
         | RPAREN
    '''

# Escribir
def p_escribe(p):
    '''
    escribe : ESCRIBIR LPAREN exp cuadprint escribe2
    '''
def p_escribe2(p):
    '''
    escribe2 : COMMA exp cuadprint escribe2
             | RPAREN
    '''

# Condicion if else
def p_condicion(p):
    '''
    condicion : IF condicion2
    '''
def p_condicion2(p):
    '''
    condicion2 : LPAREN exp RPAREN genGTF condicion3 ELSEIF genElse condicion2
               | LPAREN exp RPAREN genGTF condicion3 ELSE genElse condicion3 
               | LPAREN exp RPAREN genGTF condicion3 
    '''
def p_condicion3(p):
    '''
    condicion3 : LBRACES estatutos fillGoto RBRACES 
    '''

# Ciclo While
def p_ciclow(p):
    '''
    ciclow : WHILE storeWhile LPAREN exp RPAREN genGTF DO LBRACES estatutos RBRACES fillWhile
           | DO storeWhile LBRACES estatutos RBRACES WHILE LPAREN exp RPAREN genDoWhile
    '''

# Ciclo for
def p_ciclof(p):
    '''
    ciclof : FOR ciclof2
    '''
def p_ciclof2(p):
    '''
    ciclof2 : ciclof3 ciclof2
            | ciclof3
    '''
def p_ciclof3(p):
    '''
    ciclof3 : ID addForId EQUALS exp genFor TO exp compFor DO LBRACES estatutos RBRACES actFor
    '''

def p_error(p):
    if p == None:
        print("EOF")
    else:
        catalogoErrores([100, p.value])
        print("Parser: Error en '%s' favor de revisar el codigo" % p.value)
        global flag 
        flag = True
        exit()

#####################################################################
##############           PUNTOS NEURALGICOS         #################
#####################################################################

def p_addDir(p):
    '''
    addDir :
    '''
    global directorio_fun, current_func, varTable, current_type, countCuad, memoriaVir, memoriaTemp, tempcount

    if (p[-1] == "atributos") or (p[-1] == "("):
        # Symbol Table 3 | 10. Crear tabla de variables si no existe
        if directorio_fun[current_func[-1]]["Vars"] == None:
            directorio_fun[current_func[-1]]["Vars"] = newTV()

    elif p[-1] == "programa":
        # PN Symbol Table 1 y 2. Crear directorio de funciones y agregar el programa al directorio
        directorio_fun = {
            "global" : {
                "Type" : "program",
                "Vars" : None
            }
        }

        # Agregamos el valor a current function
        current_func.append("global")

    elif p[-1] == "}" :
        # Symbol Table 12. Borramos la tabla de variables, ya no la ocupamos
        #directorio_fun[current_func[-1]]["Vars"] = None
        # Generar el ENDFUNC
        cuad.append(cuadruplos('ENDFUNC',"","",""))
        countCuad = countCuad + 1 

        # Liberamos la tabla
        directorio_fun[current_func[-1]]["Vars"] = None

        #reiniciamos el contador de temporales
        directorio_fun[current_func[-1]]["# VT"] = tempcount
        tempcount = 0

        # Una vez que salgamos de la funcion le damos reset al temporal
        for item in memoriaTemp:
            memoriaVir[item] = memoriaTemp[item]

        current_func.pop()
    elif p[-2] == "funcion": 
        # Symbol Table 7. Prepara la tabla para agregar funcion nueva
        current_func.append(p[-1])
        if p[-3] == "void":
            current_type = p[-3]

        # Symbol table 9. Buscamos por el id en la tabla de valores, si ya existe se arroja error
        try: 
            directorio_fun[current_func[-1]]
            catalogoErrores([1, current_func[-1]])
        except:
            directorio_fun[current_func[-1]] = {
                "Type" : current_type,
                "Vars" : None
            }

def p_setCurrentID(p):
    '''
    setCurrentID :
    '''
    # Symbol table 4. Marcar cual es el ID actual
    global current_name
    current_name = p[-1]

# Symbol table 5 / 11. Buscamos por el id en la tabla de valores, si ya existe se arroja error
def p_addValueVarT(p):
    '''
    addValueVarT :
    '''

    global directorio_fun, current_func, current_name, current_type,memoriaVir

    memoryIndex = returnMemoryType(current_func[-1], current_type)
    result = memoriaVir[memoryIndex]
    memoriaVir[memoryIndex] += 1
    directorio_fun[current_func[-1]]["Vars"][current_name] = newVar(directorio_fun[current_func[-1]]["Vars"], current_name, current_type,result)



# Punto neuralgico con funcion de agregar fake bottom (Apoya cambiando el orden de operaciones, flujo, etc)
def p_addFakeBottom(p):
    '''
    addFakeBottom : 
    '''
    pOper.append('[')

# Punto neuralgico con funcion de quitar el fake bottom
def p_popFakeBottom(p):
    '''
    popFakeBottom : 
    '''
    top_val = pOper.pop()
    if top_val != '[':
        catalogoErrores([3])


#############################################################
##############           EXPRESIONES        #################
#############################################################


# Agregar operador
def p_addO(p):
    '''
    addO : 
    '''
    pOper.append(p[-1])

# Agregamos el operador OR y AND
def p_addOL(p):
    '''
    addOL : LOR
          | LAND
    '''
    pOper.append(p[1])

# Crear cuadruplo
def p_checarexp(p):
    '''
    checarexp :
    '''
    # Generamos los cuadruplos para las operaciones
    global cont, memoriaVir, current_func,countCuad,tempcount

   
    operadores = ['&&', '||', '<', '>', '==', '>=', '<=', '+', '-', '*', '/', '%']
    if  pOper[-1] in operadores:
            
            # Sacamos de las pilas los operandos derechos, izquierdos y sus tipos
            leftO = pilaO.pop()
            rightO = pilaO.pop()
            leftT = ptype.pop()
            rightT = ptype.pop()

            # Sacamos el operador
            oper = pOper.pop()
            
            # Utlizamos el cubo semantico para validar la operacion entre tipos
            resultType = apply_cubo_semantico(leftT,rightT,oper)
            # Si hay un error se levanta el error
            if resultType == 'err': 
                catalogoErrores([2, str(leftO) + " : " + leftT, str(rightO) + " : " + rightT])

            # En caso que no haya errores crea el cuadruplo del resultado
                

            indexVal = returnMemoryType("operando", resultType)
            result = memoriaVir[indexVal]
            memoriaVir[indexVal] += 1
            tempcount += 1
            cuad.append(cuadruplos(oper,rightO,leftO,result))
            #print(oper,rightO,leftO,result)
            countCuad += 1
            

            # Se agrega a la pila de operadores y tipos
            pilaO.append(result)
            ptype.append(resultType)
            cont = cont + 1


def p_addIntType(p):
    '''
    addIntType : 
    '''
    global memoriaVir, constTable, current_type

    indexVal = "intConst"
    result = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
    ptype.append("int")
    current_type = "int"
    pilaO.append(result)
    constTable[result] = {"Type" : "bool", result : p[-1]}
  
      

def p_addFloatType(p):
    '''
    addFloatType : 
    '''
    global memoriaVir, constTable, current_type
    
    indexVal = "floatConst"
    result = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
    ptype.append("float")
    current_type = "float"
    pilaO.append(result)
    constTable[result] = {"Type" : "bool", result : p[-1]}
 
     

def p_addCharType(p):
    '''
    addCharType : 
    '''
    global memoriaVir, constTable, current_type

    indexVal = "charConst"
    result = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
    ptype.append("char")
    pilaO.append(result)
    constTable[result] = {"Type" : "bool", result : p[-1]}
    current_type = "char"
    
   

def p_addBoolType(p):
    '''
    addBoolType : 
    '''
    global memoriaVir, constTable, current_type

    indexVal = "boolConst"
    result = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
    ptype.append("bool")
    pilaO.append(result)
    constTable[result] = {"Type" : "bool", result : p[-1]}
    current_type = 'bool'
    
    
def p_asignarid(p):
    '''
    asignarid : 
    '''   
    pilaO.append(p[-2])
    pOper.append(p[-1])
    
# funciones para el primer cuadruplo del main
def p_gotomain(p):
    '''
    gotomain : 
    '''
    cuad.append(cuadruplos("GOTOMAIN", "", "", 0))

def p_setmainloc(p):
    '''
    setmainloc : 
    '''
    cuad[0] = (cuadruplos("GOTOMAIN", "", "", len(cuad)))

############################################################
##############           ESTATUTOS        ##################
############################################################

# Escribe
def p_cuadprint(p):
    '''
    cuadprint : 
    '''
    global countCuad
    resultado = pilaO.pop()
    aux = cuadruplos("print","","",resultado)
    #print("print","","",resultado)
    countCuad +=1
    cuad.append(aux)

# Lee
def p_cuadlee(p):
    '''
    cuadlee : 
    '''
    global cont, countCuad, current_type, constTable,tempcount
    resultado = pilaO.pop()
    
    indexVal = returnMemoryType("operando", constTable[resultado]["Type"])
    result = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
    tempcount += 1
    aux = cuadruplos("lee",resultado,"",result)
    #print("lee",resultado,"",str(cont))
    countCuad +=1
    cuad.append(aux)
    cont = cont + 1

# Retorna
def p_returnFlag(p):
    '''
    returnFlag : 
    '''
    global current_func, directorio_fun, pilaO

    if directorio_fun[current_func[-1]]["Type"] == "void":
        catalogoErrores([6, current_func[-1]])

def p_cuadRetornar(p):
    '''
    cuadRetornar : 
    '''
    global countCuad
    resultado = pilaO.pop()
    aux = cuadruplos("return","","",resultado)
    #print("return","","",resultado)
    countCuad +=1
    cuad.append(aux)

#######################################################################
##############           CICLOS CONDICIONALES        ##################
#######################################################################

# If, elif, else
def p_genGTF(p):

    '''
    genGTF : 
    '''
    # Generamos el primer cuadruplo para goToF, guardamos su posicion para el jump a futuro
    global cont , countCuad
    resultado = pilaO.pop()
    cuad.append(cuadruplos("GotoF",resultado,"",0))
    #print("GotoF",resultado,"",0)
    countCuad +=1
    pjumps.append(len(cuad) -1)
    if not pelse:
        pelse.append("if")
    cont = cont + 1

def p_genElse(p):

    '''
    genElse : 
    '''
    # Generamos el cuadruplo para else, guardamos su posicion para el jump a futuro
    global countCuad
    if p[-1] == "else":
        cuad.append(cuadruplos("Goto","","",0))
        #print("Goto","","",0)
        countCuad +=1
        pjumps.append(len(cuad) - 1)
    pelse.append(p[-1])   

def p_fillGoto(p):
    '''
    fillGoto : 
    '''
    global countCuad
    if pjumps:
        # Sacamos los valores de las pilas
        end = pjumps.pop()
        resultado = cuad[end].rightop

        # Se les da el valor del jump apropiado, distingue si es goToF o goTo. 
        if cuad[end].op == "GotoF":
            if pelse[-1] == "elif":
                cuad[end] = cuadruplos("GotoF",resultado,"",len(cuad) + 1)
                
            else:
                cuad[end] = cuadruplos("GotoF",resultado,"",len(cuad))
                
        elif cuad[end].op == "Goto" :
            cuad[end] = cuadruplos("Goto",resultado,"",len(cuad))
            

        pelse.pop()

# While
def p_storeWhile(p):

    '''
    storeWhile : 
    '''
    # Punto neuralgico para agregar el valor a la pila de saltos
    pjumps.append(len(cuad) - 1)

def p_genDoWhile(p):

    '''
    genDoWhile : 
    '''
    global countCuad
    # Debemos crear un GoToV para el do while
    jump = pjumps.pop()
    resultado = pilaO.pop()
    # Creamos el cuadruplo apropiado
    cuad.append(cuadruplos("GotoV",resultado, "",jump + 1))
    #print("GotoV",resultado, "",jump + 1)
    countCuad +=1
    

def p_fillWhile(p):

    '''
    fillWhile : 
    '''
    global countCuad
    # Definimos con end cual es el cuadruplo a actualizar, con jump el valor que se le asignara
    end = pjumps.pop()
    jump = pjumps.pop()

    # Se crea el cuadruplo GoTo (Permanecer en el ciclo)
    cuad.append(cuadruplos("Goto","", "",jump + 1))
    #print("Goto","", "",jump + 1)
    countCuad +=1

    # Se crea el cuadruplo GoToF (Salir del ciclo)
    resultado = cuad[end].rightop
    cuad[end] = cuadruplos("GotoF",resultado,"",len(cuad))

# For
def p_addForId(p):
    '''
    addForId : 
    '''
    global current_name
    # Punto neuralcico para agregar el id definido en el for loop
    pilaO.append(p[-1])
    current_name = p[-1]
    


def p_genFor(p):
    '''
    genFor : 
    '''
    global countCuad
    
    
    if current_type == "int":
        #Punto neuralgico para agregar el cuadruplo para controlar el for
        exp = pilaO.pop()
        #revisar que id sea int
        vControl = pilaO[-1]
        pOper.append(p[-1])
        memoryIndex = returnMemoryType(current_func[-1], current_type)
        result = memoriaVir[memoryIndex]
        memoriaVir[memoryIndex] += 1
        directorio_fun[current_func[-1]]["Vars"]["vControl"] = newVar(directorio_fun[current_func[-1]]["Vars"], "vControl", current_type, result)
            #poner en tablas de variables y checar que sea int
            #crear el cuadruplo de control para el for
        cuad.append(cuadruplos("=",exp,"",result))
        #print("=",exp,"", result)
  
        countCuad +=1
    else:
        catalogoErrores([8])

    

def p_compFor(p):
    '''
    compFor : 
    '''

    #Punto neuralgico para comprarar la expresion y aggregar el goto en falso
    global cont ,countCuad , cuad

    #poner en tabla de valores el vFinal
   
    exp = pilaO.pop()

    if current_type == "int":
        memoryIndex = returnMemoryType(current_func[-1], current_type)
        result = memoriaVir[memoryIndex]
        memoriaVir[memoryIndex] += 1
        directorio_fun[current_func[-1]]["Vars"]["vFinal"] = newVar(directorio_fun[current_func[-1]]["Vars"], "vFinal", current_type, result)
        vControl = directorio_fun[current_func[-1]]["Vars"]["vControl"]["Memoria"]
        indexVal = "intTemp"
        resultado = memoriaVir[indexVal]
        memoriaVir[indexVal] += 1
         #asignar el resultado de la exprecion a vfinal
        cuad.append(cuadruplos('=',exp,"",result))
        #print('=',exp,"",result)
        countCuad +=1
        #comparar vFinal y vControl
        cuad.append(cuadruplos('<',vControl,result,resultado))
        #print('<',vControl,result,resultado)
        countCuad +=1
        pjumps.append(len(cuad) - 1)
        #generar un go to en falso para el final del for
        cuad.append(cuadruplos('GotoF',resultado,"",""))
        #print('GotoF',resultado,"","")
        countCuad +=1
        pjumps.append(len(cuad) - 1)
        cont += 1
    else:
        catalogoErrores([8])

    
   
    
def p_actFor(p):
    '''
    actFor : 
    '''
    global countCuad

    vControl = directorio_fun[current_func[-1]]["Vars"]["vControl"]["Memoria"]
    indexVal = "intTemp"
    resultado = memoriaVir[indexVal]
    memoriaVir[indexVal] += 1
     #Punto neuralgico para actualizar el control y actualizar el go en falso anterior
    #sumar 1 al contol
    cuad.append(cuadruplos('+',vControl,1,resultado))
    #print('+',vControl,1,resultado)
    countCuad +=1
    #asignar el resultado al control
    cuad.append(cuadruplos('=',resultado,"",vControl))
    #print('=',resultado,"","vControl")
    countCuad +=1
    #asignar resultado al id original
    cuad.append(cuadruplos('=',resultado,"",pilaO[-1]))
    #print('=',resultado,"",pilaO[-1])
    countCuad +=1
    fin = pjumps.pop()
    ret = pjumps.pop()
    #crear go to al inicio
    cuad.append(cuadruplos('Goto',ret,"",""))
    #print('Goto',ret,"","")
    countCuad +=1
    resultado = cuad[fin].rightop
    #actualizar el goto en falso a el cuadruplo donde termina el for
    cuad[fin] = cuadruplos("GotoF",resultado,"",len(cuad))
    pilaO.pop()



###########################################################
##############           FUNCIONES       ##################
###########################################################


#- Definicion de la funcion

def p_newParamType(p):
    '''
    newParamType : 
    '''
    global pParam, current_type
    # Agregamos el tipo de parametro a la tabla 
    pParam.append(current_type)

    # pParam

def p_addParamCount(p):

    '''
    addParamCount : 
    '''
    global directorio_fun, current_func,pParam, paramTable
    directorio_fun[current_func[-1]]["# Param"] = len(pParam)
    directorio_fun[current_func[-1]]["Param Table"]  = pParam
    #print(directorio_fun[current_func[-1]])

    pParam = []


def p_genParameter(p):
    '''
    genParameter : 
    '''
    # Obtener el tipo 
    argument = pilaO.pop()
    argumentType = ptype.pop()

    if argumentType == paramTable[parameterCounter] : 
        parc = "par" + str(parameterCounter) 
        cuad.append(cuadruplos("PARAMETER",argument,parc,""))
        #print("PARAMETER",argument,argumentType,"")
    else: 
        catalogoErrores([4, argumentType, paramTable[parameterCounter]])

def p_addContF(p):
    '''
    addContF : 
    '''
    directorio_fun[current_func[-1]]["# VL"] = len(directorio_fun[current_func[-1]]["Vars"])
    directorio_fun[current_func[-1]]["CONT"] = countCuad


#- Llamada de la funcion

def p_callFunc(p):
    '''
    callFunc : 
    '''
    
    global directorio_fun, paramTable, pCurrentCall
    # Verificar que exista la funcion en DirFunciones
    if p[-1] in directorio_fun:
        pCurrentCall.append(p[-1])
        paramTable = directorio_fun[p[-1]]["Param Table"]


def p_paramCounter(p):
    '''
    paramCounter : 
    ''' 
    global parameterCounter

    parameterCounter += 1         

def p_verLastParam(p):
    '''
    verLastParam : 
    '''
    global pCurrentCall, paramTable, cuad ,cont, pilaO, ptype,parameterCounter,tempcount

    if parameterCounter + 1 != len(paramTable):
        catalogoErrores[5]
    else:
        currentCall = pCurrentCall.pop()
        cuad.append(cuadruplos("GOSUB",currentCall,"",directorio_fun[currentCall]["CONT"]))
        #print("GOSUB",currentCall,"",directorio_fun[currentCall]["CONT"])
        
        if directorio_fun[currentCall]["Type"] != "void":
            indexVal = returnMemoryType("operando", directorio_fun[currentCall]["Type"])
            result = memoriaVir[indexVal]
            memoriaVir[indexVal] += 1
            tempcount += 1
            cuad.append(cuadruplos("=",currentCall,"",result))
            #print("=",currentCall,"",cont)
            pilaO.append(result)
            #ptype.append(directorio_fun[currentCall]["Type"])
            paramTable = []
            parameterCounter = 0
            cont += 1


yacc.yacc()
yacc.parse(fileTxt)
new_file=open("newfile.obj",mode="w",encoding="utf-8")
new_file.write("CODIGO INTERMEDIO" + "\n")

for item in obj[0]:
    cuadrStr = str(item.op) + "," + str(item.rightop) + "," + str(item.leftop) + "," + str(item.top) + "\n"
    new_file.write(cuadrStr)
    #print(item.op, item.rightop, item.leftop, item.top)


writeOBJ("newfile.obj", obj)

#print("Current func", current_func)
#print("Pila Oper", pOper)
#print("Pila O", pilaO)
#print("Pila type", ptype)
#print("Pila jumps", pjumps)
#print("pelse",pelse)
#print("param",pParam)
#print("ParamTable",paramTable)
#print("pCurrentCall",pCurrentCall)