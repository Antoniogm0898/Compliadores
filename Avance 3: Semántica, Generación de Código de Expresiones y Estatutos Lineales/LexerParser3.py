from symtable import SymbolTable
import ply.lex as lex
import ply.yacc as yacc
from Semantica import newLineTV, newVar, apply_cubo_semantico
import pandas as pd

fileTxt = r"""
    programa proyecto;
     
    Clase Persona{ 
        atributos
            int : edad;
            float : altura;
            char : nombre;
            bool : alumno;

        metodos
            int funcion retornaEdad(int edad){ 
                atributos
                int : wow;
                int : non[20];
                float : nwn[20][20];

                return (edad + 2)
            }

            float funcion retornaAltura(float altura){ 
                atributos
                int : ni;
               

                return (ni)
            }

            void funcion escribirLeer(char nombre){
                a = 3 + 2;

            }
    };

    Clase Estudiante <hereda Persona> { 
        atributos
            int : grado;
            float : peso;

        metodos
            void funcion escribirLeer2(char nombre){
                lee(nombre)
            }
    };

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
            } elif ((atributoInt < 100) || (atributoInt == 209)){
                escribir(atributoInt, ab2)
            } else {
                lee(atributoInt)
            }

            return (atributoInt)
        }

        void funcion ejemploWhile(bool flag){

            while (flag == True) do {
                lee(a)
                a = 1 + 2;
            }

            do {
                lee(a)
                a = 1 + 2;
            } while (flag == False)
        }

    principal(
        for a to 10 do {
            a = 1 + 1;
            escribe(a)
            ejemploWhile(flag)
        }
    )


    """
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
    'Clase' : 'CLASE',
    'return' : 'RETURN',
    'lee' : 'LEE',
    'escribir' : 'ESCRIBIR',
    'int'  : 'INT',
    'float' : 'FLOAT',
    'char' : 'CHAR',
    'funcion' : 'FUNCION',
    'hereda': 'HEREDA',
    'void'  : 'VOID',
    'bool' : 'BOOL',
    'atributos': 'ATRIBUTOS',
    'metodos' :'METODOS'  
 }

# Agregamos los tokens que se van a definir con regex
tokens = ['ID', 'PLUS' , 'MINUS' ,'MULT','MOD','DIV','EQUALS','COLON','SEMI','COMMA','LPAREN',
         'RPAREN','LBRACKET','RBRACKET','LBRACES','RBRACES','LT', 'LE', 'GT', 'GE', 'EQ',
         'NE','LOR','LAND','CTE_I','CTE_F','CTE_S'] + list(reserved.values())


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
global current_func, current_class, current_name, current_type, varTable, funcFlag
current_func = []
current_class = []
current_name = None
current_type = None
current_state = "G"
varTable = []
class cuadruplos : 
    def __init__(self,op, rightop, leftop,top):
        self.op = op
        self.rightop = rightop
        self.leftop = leftop
        self.top = top

variableName = []
variableType = []
pOper = []
pilaO = []
cuad = []
cont = 0

def p_programa(p):
    '''
    programa : PROGRAMA ID addDir SEMI programa2
    '''
    # Symbol Table 6. Eliminar DirFunc y VarTable
    #del(directorio_fun, current_varT)

def p_programa2(p):
    '''
    programa2 : clases programa2
              | programa3
    '''

def p_programa3(p):
    '''
    programa3 : atributos programa3
              | programa4
    '''

def p_programa4(p):
    '''
    programa4 : metodos programa4
              | programa5
    '''

def p_programa5(p):
    '''
    programa5 : principal
    '''

# Clases
def p_clases(p):
    '''
    clases : CLASE ID addDir clases2
    '''   
    
def p_clases2(p):
    '''
    clases2 : LBRACES atributos metodos RBRACES addDir SEMI  
           | LT HEREDA ID GT  LBRACES atributos metodos RBRACES addDir SEMI 
    '''   

# Atributos
def p_atributos(p):
    '''
    atributos : ATRIBUTOS addDir var
    '''

def p_addDir(p):
    '''
    addDir :
    '''
    global directorio_fun, current_func, varTable, current_class
    if (p[-1] == "atributos") or (p[-1] == "("):
        # Symbol Table 3 | 10. Crear tabla de variables si no existe
        if directorio_fun.loc[current_class[-1], "Vars"] is None:
            varTable = []
            directorio_fun.loc[current_class[-1], "Vars"] = varTable 

        print("\t\t\t", "Comenzamos con los atributos")
    # Symbol Table 7. Prepara la tabla para agregar funcion nueva
    # Symbol table 9. Buscamos por el id en la tabla de valores, si ya existe se arroja error
    elif p[-2] == "funcion": 
        current_func.append(p[-1])
        print("\t\t", "Comenzamos con las funciones")
        print("\t\t", "FUNCION: ", current_func[-1], current_type)
        directorio_fun = newVar(directorio_fun, current_class[-1], current_func[-1], current_type)
        if len(varTable) > 0:
            directorio_fun.loc[current_class[-1], "Vars"] = directorio_fun.loc[current_class[-1], "Vars"]  + varTable
            varTable = []
    elif p[-2] == "Clase":
        current_class.append(p[-1])
        print("\t", "Comenzamos con las clases")
        print("\t", "CLASE: ", p[-2], current_class[-1])
        directorio_fun = newLineTV(current_class[-1], p[-2], directorio_fun)
    elif p[-2] == "programa":
        # Symbol Table 1. Crear directorio de funciones
        current_class.append("proyecto")

        # Symbol Table 2. Se agrega el programa al directorio'
        directorio_fun = newLineTV(p[-1], p[-2], pd.DataFrame(columns = ["ID", "Type", "Vars"]))
        directorio_fun.loc[current_class[-1], "Vars"] = []
        print("Comenzamos con el programa")
        print("PROGRAMA: ", p[-1], p[-2])
    elif p[-1] == "}" :
        if len(current_func) == 0:
            print("\t", "Cerrando ciclo: ", current_class[-1])
            directorio_fun.loc[current_class[-1], "Vars"] = directorio_fun.loc[current_class[-1], "Vars"] + varTable
            current_class.pop()
        else:
            print("\t\t", "Cerrando ciclo: ", current_func[-1])
            for var in directorio_fun.loc[current_class[-1], "Vars"]:
                if var["ID"] == current_func[-1]:
                   var["Vars"] = varTable
                   varTable = []
            current_func.pop()
            


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

# Symbol Table 11: Para finalizar vamos a borrar current_varT
# Funciones
def p_funciones(p):
    '''
    funciones : tipo funciones2
              | VOID getVoid funciones2
    '''


# Symbol Table 8. En caso de ser void actualizar su tipo
def p_getVoid(p):
    '''
    getVoid :
    '''
    global current_type
    if p[-1] == "void":
        current_type = p[-1]


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
    var2 : ID setCurrentID addValueVarT var2
         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET var2
         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET LBRACKET CTE_I RBRACKET var2
         | COMMA var2
         | SEMI var
         | SEMI
    '''

def p_setCurrentID(p):
    '''
    setCurrentID :
    '''
    global current_name
    current_name = p[-1]


# Symbol table 5 / 11. Buscamos por el id en la tabla de valores, si no existe se arroja error
def p_addValueVarT(p):
    '''
    addValueVarT :
    '''
    global varTable, directorio_fun

    for item in varTable:
        if item["ID"] == current_name:
            print("ERROR: Duplicaicon de variables")
    varTable = varTable + [{"ID" :current_name, "Type" : current_type, "Vars" : None}]
    print("\t\t\t", "VAR: ", current_type ,current_name)

# Tipo
# Symbol Table 4. Se agrega el programa al directorio
def p_tipo(p):
    '''
    tipo : INT
         | FLOAT
         | CHAR
         | BOOL
    '''
    global current_type
    current_type = p[1]

# Exp
def p_exp(p):
    '''
    exp : texp
        | texp LOR texp 
    '''

def p_texp(p):
    '''
    texp : gexp
         | texp LAND texp 
    '''

def p_gexp(p):
    '''
    gexp : mexp 
         | mexp gexp2 mexp
    '''

def p_gexp2(p):
    '''
    gexp2 : LT
          | LE
          | GT
          | GE
          | EQ
          | NE
    '''

def p_mexp(p):
    '''
    mexp : t 
         | t mexp2
    '''
    if len(p) == 3: 
       if pOper[len(pOper) -1] == '+' or pOper[len(pOper) -1] == '-':
         leftO = pilaO.pop()
         rightO = pilaO.pop()
         oper = pOper.pop()
         global cont
         aux = cuadruplos(oper,rightO,leftO,cont)
    
         cuad.append(aux)
         pilaO.append(cont)
         cont = cont + 1
         #print("Cuad: " ,aux.op,aux.rightOP,aux.leftop, "T",aux.top)


def p_mexp2(p):
    '''
    mexp2 : PLUS mexp
          | MINUS mexp
    '''
    pOper.append(p[1])

def p_t(p):
    '''
    t : f 
      | f t2
    '''
    if len(p) == 3: 
       if pOper[len(pOper) -1] == '*' or pOper[len(pOper) -1] == '/':
         leftO = pilaO.pop()
         rightO = pilaO.pop()
         oper = pOper.pop()
         global cont
         aux = cuadruplos(oper,rightO,leftO,cont)
         cuad.append(aux)
         pilaO.append(cont)
         cont = cont + 1
         #print("Cuad: " ,aux.op,aux.rightOP,aux.leftop,"T",aux.top)

def p_t2(p):
    '''
    t2 : MULT t
       | DIV t
       | MOD t
    '''
    pOper.append(p[1])

def p_f(p):
    '''
    f : LPAREN exp RPAREN
      | CTE_I
      | CTE_F
      | CTE_S
      | llamada
      | ID
    '''
    pilaO.append(p[1])

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
    lee : LEE LPAREN ID RPAREN
    '''
    resultado = pilaO.pop()
    aux = cuadruplos("print","","",resultado)
    cuad.append(aux)
    #print("Cuad: ",aux.op,aux.top)

# Escribir
def p_escribe(p):
    '''
    escribe : ESCRIBIR LPAREN ID escribe2
    '''

def p_escribe2(p):
    '''
    escribe2 : COMMA ID escribe2
             | RPAREN
    '''

# Condicion if else
def p_condicion(p):
    '''
    condicion : IF condicion2
    '''

def p_condicion2(p):
    '''
    condicion2 : LPAREN exp RPAREN condicion3 ELSEIF condicion2
               | LPAREN exp RPAREN condicion3 ELSE condicion3
               | LPAREN exp RPAREN condicion3 
    
    '''

def p_condicion3(p):
    '''
    condicion3 : LBRACES estatutos RBRACES
    '''

# Ciclo While
def p_ciclow(p):
    '''
    ciclow : WHILE LPAREN exp RPAREN DO LBRACES estatutos RBRACES
           | DO LBRACES estatutos RBRACES WHILE LPAREN exp RPAREN
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
    ciclof3 : ID TO exp DO LBRACES estatutos RBRACES
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



yacc.yacc()
file = "testA.txt"
yacc.parse(fileTxt)

for x in cuad:
    print(x.op, x.rightop, x.leftop, x.top)

for idV in directorio_fun["ID"]:
    print(idV, ": ")
    for value in directorio_fun[directorio_fun["ID"] == idV]["Vars"]:
        print(value)