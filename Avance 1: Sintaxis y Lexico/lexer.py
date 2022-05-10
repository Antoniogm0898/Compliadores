from lib2to3.pgen2.token import COLON
import ply.lex as lex

# Vamos a crear los tokens para nuestro compilador, para esto nos vamos a apoyar con el lexer de Ply

# Tokens palabras reservadas (El diccionario las convierte de minuscula a mayuscula)
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'else if' : 'ELSEIF',
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
t_ignore = r' \t'
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
t_RBRACES = r'\{'
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

lex.input("metoos")
while True:
    tok = lex.token()
    print(tok)
    if not tok :
        break