import ply.lex as lex
import ply.yacc as yacc
from Semantica import newTV, newVar, apply_cubo_semantico
from CatalogoErrores import catalogoErrores

# Ejemplo
fileTxt = r"""
    programa proyecto;

    atributos
        int : atributoInt, atributoInt2;
        float : atributoFloat, atributoFloat2;
        char : atributoChar, atributoChar2;
        bool : atributoBool, atributoBool2 ;
        int  : atributoArr[20];
        float : atributoMat[20][10] ;

    metodos
        int funcion ejemploCondicional(int atributoInt){

            if ((atributoInt >= 1) && (atributoInt <= 100)){
                escribir(atributoInt)
                aZ = 2 + 1;
                b = 3 % 2;
            } elif ((atributoInt < 100) || (atributoInt == 209)){
                escribir(atributoInt, ab2)
            } else {
                escribir(atributoInt)
            }

            return (atributoInt)
        }

        void funcion ejemploWhile(bool flag){

            while (flag == True) do {
                lee(12)
                a = 1 + 2;
            }

            do {
                lee(a)
                a = 1 + 2;
            } while (flag == False)
        }

    principal(
        for a = 1 to 'dkd' do {
            a = 1 + 1;
            escribir(a)
        }
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
    'metodos' :'METODOS'  
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
###################             PARSER            ###################
#####################################################################

# Estructuras para el programa
global current_name, current_type, current_func
current_name = None
current_type = None
current_func = []

# Vamos a crear una clase para los cuadruplos
class cuadruplos : 
    def __init__(self,op, rightop, leftop,top):
        self.op = op
        self.rightop = rightop
        self.leftop = leftop
        self.top = top

# Vamos a definir las estructuras a ultizar para crear y m
variableName = []
variableType = []
pOper = [""]
pilaO = []
cuad = []
ptype = []
pjumps = []
pelse = []

vControl = 0
cont = 0

def p_programa(p):
    '''
    programa : PROGRAMA addDir ID SEMI programa2
    '''
    # Symbol Table 6. Una vez que acaba el programa se borra el Directorio de funciones
    global directorio_fun
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
    programa4 : principal
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
    funciones2 : FUNCION ID addDir LPAREN addDir parametros RPAREN LBRACES atributos funciones3
               | FUNCION ID addDir LPAREN addDir parametros RPAREN LBRACES funciones3
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
    parametros : tipo ID setCurrentID addValueVarT COMMA parametros
               | tipo ID setCurrentID addValueVarT
    '''
    

# Return
def p_retornar(p):
    '''
    retornar : RETURN LPAREN retornar2
    '''

def p_retornar2(p):
    '''
    retornar2 : exp COMMA retornar2
              | exp RPAREN
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
      | CTE_I 
      | CTE_F 
      | CTE_S 
      | llamada
      | ID 
    '''
    if len(p) == 2:
        pilaO.append(p[1])
        ptype.append(current_type)


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
    asigna : ID EQUALS exp SEMI
    '''
    pilaO.append(p[1])
    pOper.append(p[2])
    rightO = pilaO.pop()
    leftO = pilaO.pop()
    oper = pOper.pop()
    aux = cuadruplos(oper,rightO,"",leftO)
    cuad.append(aux)
    

# Llamada
def p_llamada(p):
    '''
    llamada : ID LPAREN exp llamada2
    '''
   

def p_llamada2(p):
    '''
    llamada2 : COMMA exp llamada2
             | RPAREN
    '''

# Lectura
def p_lee(p):
    '''
    lee : LEE LPAREN exp cuadlee RPAREN
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
        print(p)
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
    global directorio_fun, current_func, varTable

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
        current_func.pop()
    elif p[-2] == "funcion": 
        # Symbol Table 7. Prepara la tabla para agregar funcion nueva
        current_func.append(p[-1])

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
    global directorio_fun, current_func, current_name, current_type

    directorio_fun[current_func[-1]]["Vars"] = newVar(directorio_fun[current_func[-1]]["Vars"], current_name, current_type)
    (directorio_fun[current_func[-1]]["Vars"])

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
    global cont
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
            result = "t" + str(cont)
            cuad.append(cuadruplos(oper,rightO,leftO,result))

            # Se agrega a la pila de operadores y tipos
            pilaO.append(result)
            ptype.append(resultType)
            cont = cont + 1

############################################################
##############           ESTATUTOS        ##################
############################################################


def p_cuadprint(p):
    '''
    cuadprint : 
    '''
   
    resultado = pilaO.pop()
    aux = cuadruplos("print","","",resultado)
    cuad.append(aux)

def p_cuadlee(p):
    '''
    cuadlee : 
    '''
    global cont
    resultado = pilaO.pop()
    aux = cuadruplos("lee",resultado,"","t" + str(cont))
    cuad.append(aux)
    cont = cont + 1


#######################################################################
##############           CICLOS CONDICIONALES        ##################
#######################################################################

# If, elif, else
def p_genGTF(p):

    '''
    genGTF : 
    '''
    # Generamos el primer cuadruplo para goToF, guardamos su posicion para el jump a futuro
    global cont
    resultado = pilaO.pop()
    cuad.append(cuadruplos("GotoF",resultado,"",0))
    pjumps.append(len(cuad) -1)
    if not pelse:
        pelse.append("if")
    cont = cont + 1

def p_genElse(p):

    '''
    genElse : 
    '''
    # Generamos el cuadruplo para else, guardamos su posicion para el jump a futuro
    if p[-1] == "else":
        cuad.append(cuadruplos("Goto","","",0))
        pjumps.append(len(cuad) - 1)
    pelse.append(p[-1])   

def p_fillGoto(p):
    '''
    fillGoto : 
    '''
    
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
    # Debemos crear un GoToV para el do while
    jump = pjumps.pop()
    resultado = pilaO.pop()
    # Creamos el cuadruplo apropiado
    cuad.append(cuadruplos("GotoV",resultado, "",jump + 1))

def p_fillWhile(p):

    '''
    fillWhile : 
    '''
    # Definimos con end cual es el cuadruplo a actualizar, con jump el valor que se le asignara
    end = pjumps.pop()
    jump = pjumps.pop()

    # Se crea el cuadruplo GoTo (Permanecer en el ciclo)
    cuad.append(cuadruplos("Goto","", "",jump + 1))

    # Se crea el cuadruplo GoToF (Salir del ciclo)
    resultado = cuad[end].rightop
    cuad[end] = cuadruplos("GotoF",resultado,"",len(cuad))

# For
def p_addForId(p):
    '''
    addForId : 
    '''
    # Punto neuralcico para agregar el id definido en el for loop
    pilaO.append(p[-1])


def p_genFor(p):
    '''
    genFor : 
    '''
    exp = pilaO.pop()
    vControl = pilaO[-1]
    pOper.append(p[-1])
    cuad.append(cuadruplos("=",exp,"","vControl"))
    

def p_compFor(p):
    '''
    compFor : 
    '''
    global cont
    exp = pilaO.pop()
    cuad.append(cuadruplos('=',exp,"","vFinal"))
    cuad.append(cuadruplos('<',"vControl","vFinal",cont))
    pjumps.append(len(cuad) - 1)
    cuad.append(cuadruplos('GotoF',cont,"",""))
    pjumps.append(len(cuad) - 1)
    cont += 1
    
def p_actFor(p):
    '''
    actFor : 
    '''
    print(pilaO)
    cuad.append(cuadruplos('+',"vControl",1,cont))
    cuad.append(cuadruplos('=',cont,"","vControl"))
    cuad.append(cuadruplos('=',cont,"",pilaO[-1]))
    fin = pjumps.pop()
    ret = pjumps.pop()
    cuad.append(cuadruplos('Goto',ret,"",""))
    resultado = cuad[fin].rightop
    cuad[fin] = cuadruplos("GotoF",resultado,"",len(cuad))
    pilaO.pop()

yacc.yacc()
yacc.parse(fileTxt)

a = 0
for item in cuad:
    print(a, item.op, item.rightop, item.leftop, item.top)
    a = a + 1
