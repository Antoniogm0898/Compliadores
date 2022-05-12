from lib2to3.pgen2.token import COLON
import ply.lex as lex
import ply.yacc as yacc

# Vamos a crear los tokens para nuestro compilador, para esto nos vamos a apoyar con el lexer de Ply

# Tokens palabras reservadas (El diccionario las convierte de minuscula a mayuscula)
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'elif' : 'ELSEIF',
    'for' : 'FOR',
    'while' : 'WHILE',
    'do' : 'DO',
    'programa' : 'PROGRAMA',
    'principal' : 'PRINCIPAL',
    'Clase' : 'CLASE',
    'return' : 'Return',
    'lee' : 'LEE',
    'escribir' : 'ESCRIBIR',
    'int'  : 'INT',
    'float' : 'FLOAT',
    'char' : 'CHAR',
    'hereda': 'HEREDA',
    'void'  : 'VOID',
    'bool' : 'BOOL',
    'atributos': 'ATRIBUTOS',
    'metodos' :'METODOS'  
 }

# Agregamos los tokens que se van a definir con regex
tokens = ['ID', 'PLUS' , 'MINUS' ,'MULT','MOD','DIV','EQUALS','COLON','SEMI','DOT','COMMA','LPAREN',
         'RPAREN','LBRACKET','RBRACKET','LBRACES','RBRACES','AND','OR','LT', 'LE', 'GT', 'GE', 'EQ',
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
t_DOT = r'\.'
t_COMMA = r'\,'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACES = r'\{'
t_RBRACES = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_OR = r'\|'
t_AND = r'&'
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

lex.lex()

lex.input(
    r"""
     programa proyecto;
     Clase Persona
     {atributos
        edad : int;
        nombre[30] : char;
      metodos
        int funcion uno (x : int)
        { regresa (edad - x)}
     }
     elif
     """

)
while True:
    tok = lex.token()
    print(tok)
    if not tok :
        break

#####################################################################
###################             PARSER            ###################
#####################################################################

# Vamos a declarar la gramatica
# Programa
def p_programa(p):
    '''
    programa : PROGRAMA ID SEMI programa2
    '''

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
    clases : CLASE clases2
    '''   

def p_clases2(p):
    '''
    clases2 : clases3 
           | clases4 clases3
    '''   

def p_clases3(p):
    '''
    clases3 : LBRACKET atributos metodos RBRACKET SEMI 
    '''   

def p_clases4(p):
    '''
    clases4 : LT HERDA ID GT 
    '''   

# Atributos
def p_atributos(p):
    '''
    atributos : ATRIBUTOS var
    '''

# Metodos
def p_metodos(p):
    '''
    metodos : METODOS funciones
    '''

# Principal
def p_principal(p):
    '''
    principal : PRINCIPAL LPARENT estatutos principal2
    '''

def p_principal2(p):
    '''
    principal2 : estatutos principal2
               | RPARENT
    '''

# Funciones
def p_funciones(p):
    '''
    funciones : tipo ID funciones2
              | VOID ID funciones2
    '''

def p_funciones2(p):
    '''
    funciones2 : LPARENT parametros RPARENT LBRACKET estatutos RBRACKET
    '''

# Parametros
def p_parametros(p):
    '''
    parametros : tipo ID COMMA parametros
               | tipo ID
    '''

# Variables
def p_var(p):
    '''
    var : ID var3 var2 
        | ID var2
    '''

def p_var2(p):
    '''
    var2 : COMMA var
         | COLON tipo SEMI
    '''

def p_var3(p):
    '''
    var3 : LBRACES exp RBRACES
         | LBRACES exp RBRACES LBRACES exp RBRACES
    '''

# Tipo
def p_tipo(p):
    '''
    tipo : INT
         | FLOAT
         | CHAR
         | BOOL
    '''

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
    gexp : mexp gexp2 mexp
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
    mexp : t mexp2
         | t
    '''

def p_mexp2(p):
    '''
    mexp2 : + mexp
          | - mexp
    '''

def p_t(p):
    '''
    t : f t2
      | f
    '''

def p_t2(p):
    '''
    t2 : * t
       | / t
    '''

def p_f(p):
    '''
    f : LPAREN exp RPAREN
      | CTE_I
      | CTE_F
      | CTE_S
      | var
      | llamada
    '''

# Estatutos
def p_estatutos(p):
    '''
    estatutos : asigna
              | llamada
              | lee
              | escribe
              | condicion
              | ciclow
              | ciclof
              | funciones
    '''

# Asigna
def p_asigna(p):
    '''
    asigna : var EQUALS exp
    '''

# Llamada
def p_llamada(p):
    '''
    llamada : ID LPAREN exp llamada2
    '''

def p_llamada2(p):
    '''
    llamada2 : COMMA exp llamada2
             | RPARENT
    '''

# Lectura
def p_lee(p):
    '''
    lee : LEE LPAREN var RPAREN
    '''

# Escribir
def p_escribe(p):
    '''
    escribe : ESCRIBIR LPAREN exp escribe2
    '''

def p_escribe2(p):
    '''
    escribe2 : COMMA exp escribe2
             | RPARENT
    '''

# Condicion if else
def p_condicion(p):
    '''
    condicion : IF LPAREN exp RPAREN condicion2
              | IF LPAREN exp RPAREN condicion3
    '''

def p_condicion2(p):
    '''
    condicion2 : ELIF LPAREN exp RPAREN condicion2
               | condicion3

    '''

def p_condicion(p):
    '''
    condicion3 : LBRACKET estatutos RBRACKET
               | ELSE LBRACKET estatutos RBRACKET
    '''

# Ciclo While
def p_ciclow(p):
    '''
    ciclow : WHILE ciclow2
           | DO ciclow3
    '''

def p_ciclow2(p):
    '''
    ciclow2 : LPAREN exp RPAREN LBRACKET estatutos RBRACKET ciclow2
            | LPAREN exp RPAREN LBRACKET estatutos RBRACKET 
    '''

def p_ciclow3(p):
    '''
    ciclow3 : LBRACKET estatutos RBRACKET WHILE LPAREN exp RPAREN ciclow3
            | LBRACKET estatutos RBRACKET 
    '''

# Ciclo for
def p_ciclof(p):
    '''
    ciclof : LPAREN FOR ciclof2
    '''

def p_ciclof2(p):
    '''
    ciclof2 : ciclof3 ciclof2
            | ciclof3
    '''

def p_ciclof3(p):
    '''
    ciclof3 : var EQUALS TO exp DO LBRACKET estatutos RBRACKET
    '''

#yacc.yacc()
#yacc.parse()