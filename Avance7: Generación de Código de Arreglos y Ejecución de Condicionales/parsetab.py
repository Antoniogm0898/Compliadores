
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ATRIBUTOS BOOL CHAR COLON COMMA CTE_F CTE_I CTE_S DIV DO ELSE ELSEIF EQ EQUALS ESCRIBIR FALSE FLOAT FOR FUNCION GE GT ID IF INT LAND LBRACES LBRACKET LE LEE LOR LPAREN LT METODOS MINUS MOD MULT NE PLUS PRINCIPAL PROGRAMA RBRACES RBRACKET RETURN RPAREN SEMI TO TRUE VOID WHILE\n    programa : gotomain PROGRAMA addDir ID SEMI programa2\n    \n    programa2 : atributos programa2\n              | programa3\n    \n    programa3 : metodos programa3\n              | programa4 \n    \n    programa4 : setmainloc principal\n    \n    atributos : ATRIBUTOS addDir var\n    \n    metodos : METODOS funciones\n    \n    principal : PRINCIPAL LPAREN estatutos principal2\n    \n    principal2 : estatutos principal2\n               | RPAREN\n    \n    funciones : tipo funciones2\n              | VOID funciones2\n    \n    funciones2 : FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF atributos funciones3\n               | FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF funciones3\n               | FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES var addContF funciones3\n    \n    funciones3 : estatutos RBRACES addDir funciones\n               | estatutos RBRACES addDir\n               | estatutos funciones3\n    \n    parametros : tipo ID setCurrentID addValueVarT newParamType COMMA parametros\n               | tipo ID setCurrentID addValueVarT newParamType\n    \n    retornar : RETURN returnFlag LPAREN retornar2\n    \n    retornar2 : exp cuadRetornar COMMA retornar2\n              | exp cuadRetornar RPAREN\n    \n    var : tipo COLON var2\n     \n    var2 : ID setCurrentID addValueVarT var3\n         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET var3\n         | ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET LBRACKET CTE_I RBRACKET var3\n\n    \n    var3 : COMMA var2\n         | SEMI var\n         | SEMI    \n    \n    tipo : INT\n         | FLOAT\n         | CHAR\n         | BOOL\n    \n    exp : texp checarexp\n        | exp addOL exp \n    \n    texp : gexp checarexp\n         | texp addOL texp\n    \n    gexp : mexp checarexp\n         | gexp gexp2 gexp\n    \n    gexp2 : LT addO\n          | LE addO\n          | GT addO\n          | GE addO\n          | EQ addO\n          | NE addO\n    \n    mexp : t checarexp\n         | mexp mexp2 \n    \n    mexp2 : PLUS addO mexp \n          | MINUS addO mexp \n    \n    t : f checarexp\n      | t t2 \n    \n    t2 : MULT addO t \n       | DIV addO t \n       | MOD addO t \n    \n    f : LPAREN addFakeBottom exp RPAREN popFakeBottom\n      | CTE_I addIntType\n      | CTE_F addFloatType\n      | CTE_S addCharType\n      | TRUE addBoolType\n      | FALSE addBoolType\n      | llamada\n      | ID \n    \n    estatutos : estatutos2 estatutos\n              | estatutos2 \n    \n    estatutos2 : asigna\n               | llamada\n               | lee\n               | escribe\n               | condicion\n               | ciclow\n               | ciclof\n               | funciones\n               | retornar\n    \n    asigna : ID EQUALS asignarid exp SEMI\n    \n    llamada : ID callFunc LPAREN exp genParameter llamada2\n    \n    llamada2 : COMMA paramCounter exp genParameter llamada2\n             | verLastParam RPAREN\n    \n    lee : LEE LPAREN exp cuadlee lee2\n    \n    lee2 : COMMA exp cuadlee lee2\n         | RPAREN\n    \n    escribe : ESCRIBIR LPAREN exp cuadprint escribe2\n    \n    escribe2 : COMMA exp cuadprint escribe2\n             | RPAREN\n    \n    condicion : IF condicion2\n    \n    condicion2 : LPAREN exp RPAREN genGTF condicion3 ELSEIF genElse condicion2\n               | LPAREN exp RPAREN genGTF condicion3 ELSE genElse condicion3 \n               | LPAREN exp RPAREN genGTF condicion3 \n    \n    condicion3 : LBRACES estatutos fillGoto RBRACES \n    \n    ciclow : WHILE storeWhile LPAREN exp RPAREN genGTF DO LBRACES estatutos RBRACES fillWhile\n           | DO storeWhile LBRACES estatutos RBRACES WHILE LPAREN exp RPAREN genDoWhile\n    \n    ciclof : FOR ciclof2\n    \n    ciclof2 : ciclof3 ciclof2\n            | ciclof3\n    \n    ciclof3 : ID addForId EQUALS exp genFor TO exp compFor DO LBRACES estatutos RBRACES actFor\n    \n    addDir :\n    \n    setCurrentID :\n    \n    addValueVarT :\n    \n    addFakeBottom : \n    \n    popFakeBottom : \n    \n    addO : \n    \n    addOL : LOR\n          | LAND\n    \n    checarexp :\n    \n    addIntType : \n    \n    addFloatType : \n    \n    addCharType : \n    \n    addBoolType : \n    \n    asignarid : \n    \n    gotomain : \n    \n    setmainloc : \n    \n    cuadprint : \n    \n    cuadlee : \n    \n    returnFlag : \n    \n    cuadRetornar : \n    \n    genGTF : \n    \n    genElse : \n    \n    fillGoto : \n    \n    storeWhile : \n    \n    genDoWhile : \n    \n    fillWhile : \n    \n    addForId : \n    \n    genFor : \n    \n    compFor : \n    \n    actFor : \n    \n    newParamType : \n    \n    addParamCount : \n    \n    genParameter : \n    \n    addContF : \n    \n    callFunc : \n    \n    paramCounter : \n    \n    verLastParam : \n    '
    
_lr_action_items = {'PROGRAMA':([0,2,],[-111,3,]),'$end':([1,7,9,12,15,17,25,58,59,75,],[0,-1,-3,-5,-2,-4,-6,-9,-11,-10,]),'ID':([3,4,21,22,23,24,27,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,52,54,57,60,61,63,64,65,66,69,70,76,77,78,94,95,96,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,138,140,142,143,145,146,149,150,151,155,156,157,158,159,160,161,162,163,164,165,166,167,168,175,176,179,180,190,191,195,196,200,201,209,210,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-97,5,-32,-33,-34,-35,-7,-12,34,-13,46,55,46,46,-67,-68,-69,-70,-71,-72,-73,-74,-75,71,-25,46,-65,-110,91,91,-86,91,-93,71,91,91,-100,91,46,-94,91,91,91,-103,-104,91,91,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,91,-22,-26,55,-31,178,-76,-80,91,-82,-42,-43,-44,-45,-46,-47,91,91,91,91,91,-83,91,-85,-29,-30,-77,-132,-89,46,91,-24,91,-79,91,91,-23,-27,-130,-81,-84,46,46,-130,-87,-88,-90,-121,46,-15,46,46,-78,-122,-92,-28,-14,-97,-19,-16,-91,46,-18,-17,-126,-96,]),'SEMI':([5,55,73,80,81,82,83,84,85,86,87,88,89,90,91,99,101,108,110,118,119,122,123,127,128,129,130,131,132,152,153,154,179,182,184,185,186,187,188,197,201,202,236,242,],[6,-98,-99,-105,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,143,146,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,-37,-39,-41,-77,-101,-50,-51,-54,-55,-56,143,-79,-57,143,-78,]),'ATRIBUTOS':([6,8,27,54,140,143,175,176,213,214,226,246,],[10,10,-7,-25,-26,-31,-29,-30,-27,-130,10,-28,]),'METODOS':([6,8,11,18,27,29,31,54,140,143,175,176,213,238,246,247,248,249,250,254,256,],[13,13,13,-8,-7,-12,-13,-25,-26,-31,-29,-30,-27,-15,-28,-14,-97,-19,-16,-18,-17,]),'PRINCIPAL':([6,8,11,14,18,27,29,31,54,140,143,175,176,213,238,246,247,248,249,250,254,256,],[-112,-112,-112,26,-8,-7,-12,-13,-25,-26,-31,-29,-30,-27,-15,-28,-14,-97,-19,-16,-18,-17,]),'INT':([10,13,16,27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,74,95,96,100,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,241,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-97,21,21,-7,-12,-13,21,21,21,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,21,-65,-86,-93,-95,-97,21,-94,21,-22,-26,21,-76,-80,-82,-83,-85,-29,-30,-77,-89,21,-24,-79,-23,-27,21,-81,-84,21,21,-130,-87,-88,-90,-121,21,-15,21,21,21,-78,-122,-92,-28,-14,-97,-19,-16,-91,21,21,-17,-126,-96,]),'FLOAT':([10,13,16,27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,74,95,96,100,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,241,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-97,22,22,-7,-12,-13,22,22,22,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,22,-65,-86,-93,-95,-97,22,-94,22,-22,-26,22,-76,-80,-82,-83,-85,-29,-30,-77,-89,22,-24,-79,-23,-27,22,-81,-84,22,22,-130,-87,-88,-90,-121,22,-15,22,22,22,-78,-122,-92,-28,-14,-97,-19,-16,-91,22,22,-17,-126,-96,]),'CHAR':([10,13,16,27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,74,95,96,100,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,241,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-97,23,23,-7,-12,-13,23,23,23,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,23,-65,-86,-93,-95,-97,23,-94,23,-22,-26,23,-76,-80,-82,-83,-85,-29,-30,-77,-89,23,-24,-79,-23,-27,23,-81,-84,23,23,-130,-87,-88,-90,-121,23,-15,23,23,23,-78,-122,-92,-28,-14,-97,-19,-16,-91,23,23,-17,-126,-96,]),'BOOL':([10,13,16,27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,74,95,96,100,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,241,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-97,24,24,-7,-12,-13,24,24,24,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,24,-65,-86,-93,-95,-97,24,-94,24,-22,-26,24,-76,-80,-82,-83,-85,-29,-30,-77,-89,24,-24,-79,-23,-27,24,-81,-84,24,24,-130,-87,-88,-90,-121,24,-15,24,24,24,-78,-122,-92,-28,-14,-97,-19,-16,-91,24,24,-17,-126,-96,]),'VOID':([13,27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[20,-7,-12,-13,20,20,20,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,20,-65,-86,-93,-95,20,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,20,-24,-79,-23,-27,-130,-81,-84,20,20,-130,-87,-88,-90,-121,20,-15,20,20,-78,-122,-92,-28,-14,-97,-19,-16,-91,20,20,-17,-126,-96,]),'FUNCION':([19,20,21,22,23,24,],[30,30,-32,-33,-34,-35,]),'COLON':([21,22,23,24,28,],[-32,-33,-34,-35,33,]),'LPAREN':([26,34,46,47,48,49,50,53,56,61,62,63,64,66,67,72,76,77,78,91,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,150,155,156,157,158,159,160,161,162,163,164,165,167,180,193,195,200,205,209,210,219,],[32,-97,-131,63,64,66,-120,-115,74,-110,77,78,78,78,94,98,78,78,-100,-131,78,78,78,78,-103,-104,78,78,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,78,78,-42,-43,-44,-45,-46,-47,78,78,78,78,78,78,-132,209,78,78,-118,78,78,66,]),'LEE':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,47,47,47,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,47,-65,-86,-93,-95,47,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,47,-24,-79,-23,-27,-130,-81,-84,47,47,-130,-87,-88,-90,-121,47,-15,47,47,-78,-122,-92,-28,-14,-97,-19,-16,-91,47,-18,-17,-126,-96,]),'ESCRIBIR':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,48,48,48,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,48,-65,-86,-93,-95,48,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,48,-24,-79,-23,-27,-130,-81,-84,48,48,-130,-87,-88,-90,-121,48,-15,48,48,-78,-122,-92,-28,-14,-97,-19,-16,-91,48,-18,-17,-126,-96,]),'IF':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,49,49,49,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,49,-65,-86,-93,-95,49,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,49,-24,-79,-23,-27,-130,-81,-84,49,49,-130,-87,-88,-90,-121,49,-15,49,49,-78,-122,-92,-28,-14,-97,-19,-16,-91,49,-18,-17,-126,-96,]),'WHILE':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,171,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,50,50,50,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,50,-65,-86,-93,-95,50,-94,-22,-26,-31,-76,-80,-82,-83,-85,193,-29,-30,-77,-89,50,-24,-79,-23,-27,-130,-81,-84,50,50,-130,-87,-88,-90,-121,50,-15,50,50,-78,-122,-92,-28,-14,-97,-19,-16,-91,50,-18,-17,-126,-96,]),'DO':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,80,81,82,83,84,85,86,87,88,89,90,91,95,96,108,110,118,119,122,123,127,128,129,130,131,132,138,140,143,146,149,151,152,153,154,166,168,170,175,176,179,182,184,185,186,187,188,190,191,192,196,201,202,211,213,214,217,218,222,224,226,227,230,231,232,234,235,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,51,51,51,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,51,-65,-86,-93,-95,-105,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,51,-94,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,-22,-26,-31,-76,-80,-82,-37,-39,-41,-83,-85,-117,-29,-30,-77,-101,-50,-51,-54,-55,-56,-89,51,208,-24,-79,-57,-23,-27,-130,-81,-84,51,-125,51,-130,-87,-88,-90,-121,245,51,-15,51,51,-78,-122,-92,-28,-14,-97,-19,-16,-91,51,-18,-17,-126,-96,]),'FOR':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,52,52,52,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,52,-65,-86,-93,-95,52,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,52,-24,-79,-23,-27,-130,-81,-84,52,52,-130,-87,-88,-90,-121,52,-15,52,52,-78,-122,-92,-28,-14,-97,-19,-16,-91,52,-18,-17,-126,-96,]),'RETURN':([27,29,31,32,35,36,37,38,39,40,41,42,43,44,45,54,57,60,65,69,70,95,96,138,140,143,146,149,151,166,168,175,176,179,190,191,196,201,211,213,214,217,218,222,226,227,230,231,232,234,237,238,239,240,242,243,244,246,247,248,249,250,252,253,254,256,257,258,],[-7,-12,-13,53,53,53,-67,-68,-69,-70,-71,-72,-73,-74,-75,-25,53,-65,-86,-93,-95,53,-94,-22,-26,-31,-76,-80,-82,-83,-85,-29,-30,-77,-89,53,-24,-79,-23,-27,-130,-81,-84,53,53,-130,-87,-88,-90,-121,53,-15,53,53,-78,-122,-92,-28,-14,-97,-19,-16,-91,53,-18,-17,-126,-96,]),'RPAREN':([29,31,35,36,37,38,39,40,41,42,43,44,45,57,60,65,69,70,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,102,104,108,110,118,119,122,123,127,128,129,130,131,132,133,135,138,139,144,146,147,148,149,151,152,153,154,166,168,173,178,179,181,182,183,184,185,186,187,188,189,190,196,199,201,202,203,204,211,215,216,217,218,223,228,229,230,231,232,234,238,242,243,244,247,248,249,250,251,252,254,256,257,258,],[-12,-13,59,-66,-67,-68,-69,-70,-71,-72,-73,-74,-75,59,-65,-86,-93,-95,-114,-105,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-113,134,-94,-129,151,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,168,170,-22,-116,177,-76,-133,182,-80,-82,-37,-39,-41,-83,-85,196,-98,-77,201,-101,-114,-50,-51,-54,-55,-56,-113,-89,-24,-99,-79,-57,151,168,-23,-127,-129,-81,-84,234,-21,-133,-87,-88,-90,-121,-15,-78,-122,-92,-14,-97,-19,-16,-20,-91,-18,-17,-126,-96,]),'RBRACES':([29,31,36,37,38,39,40,41,42,43,44,45,60,65,69,70,96,136,138,146,149,151,166,168,179,190,196,201,207,211,217,218,221,230,231,232,233,234,238,239,242,243,244,247,248,249,250,252,254,255,256,257,258,],[-12,-13,-66,-67,-68,-69,-70,-71,-72,-73,-74,-75,-65,-86,-93,-95,-94,171,-22,-76,-80,-82,-83,-85,-77,-89,-24,-79,-119,-23,-81,-84,232,-87,-88,-90,243,-121,-15,248,-78,-122,-92,-14,-97,-19,-16,-91,-18,257,-17,-126,-96,]),'EQUALS':([46,71,97,],[61,-123,137,]),'LBRACES':([51,68,134,169,177,198,206,208,220,245,],[-120,95,-117,191,-128,214,-118,222,191,253,]),'LBRACKET':([55,73,99,197,],[-98,-99,141,212,]),'COMMA':([55,73,79,80,81,82,83,84,85,86,87,88,89,90,91,92,99,102,104,108,110,118,119,122,123,127,128,129,130,131,132,133,139,147,152,153,154,173,178,179,182,183,184,185,186,187,188,189,197,199,201,202,203,204,215,216,228,229,236,242,],[-98,-99,-114,-105,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-113,142,-129,150,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,167,-116,180,-37,-39,-41,195,-98,-77,-101,-114,-50,-51,-54,-55,-56,-113,142,-99,-79,-57,150,167,-127,-129,241,180,142,-78,]),'CTE_I':([61,63,64,66,76,77,78,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,141,150,155,156,157,158,159,160,161,162,163,164,165,167,180,195,200,209,210,212,],[-110,85,85,85,85,85,-100,85,85,85,85,-103,-104,85,85,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,85,174,85,-42,-43,-44,-45,-46,-47,85,85,85,85,85,85,-132,85,85,85,85,225,]),'CTE_F':([61,63,64,66,76,77,78,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,150,155,156,157,158,159,160,161,162,163,164,165,167,180,195,200,209,210,],[-110,86,86,86,86,86,-100,86,86,86,86,-103,-104,86,86,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,86,86,-42,-43,-44,-45,-46,-47,86,86,86,86,86,86,-132,86,86,86,86,]),'CTE_S':([61,63,64,66,76,77,78,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,150,155,156,157,158,159,160,161,162,163,164,165,167,180,195,200,209,210,],[-110,87,87,87,87,87,-100,87,87,87,87,-103,-104,87,87,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,87,87,-42,-43,-44,-45,-46,-47,87,87,87,87,87,87,-132,87,87,87,87,]),'TRUE':([61,63,64,66,76,77,78,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,150,155,156,157,158,159,160,161,162,163,164,165,167,180,195,200,209,210,],[-110,88,88,88,88,88,-100,88,88,88,88,-103,-104,88,88,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,88,88,-42,-43,-44,-45,-46,-47,88,88,88,88,88,88,-132,88,88,88,88,]),'FALSE':([61,63,64,66,76,77,78,94,98,103,105,106,107,109,111,112,113,114,115,116,117,120,121,124,125,126,137,150,155,156,157,158,159,160,161,162,163,164,165,167,180,195,200,209,210,],[-110,89,89,89,89,89,-100,89,89,89,89,-103,-104,89,89,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,-102,89,89,-42,-43,-44,-45,-46,-47,89,89,89,89,89,89,-132,89,89,89,89,]),'LOR':([79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,101,102,108,110,118,119,122,123,127,128,129,130,131,132,135,139,148,152,153,154,172,179,182,183,184,185,186,187,188,189,201,202,216,223,224,242,],[106,106,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,106,106,106,106,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,106,106,106,106,106,-41,106,-77,-101,106,-50,-51,-54,-55,-56,106,-79,-57,106,106,106,-78,]),'LAND':([79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,101,102,108,110,118,119,122,123,127,128,129,130,131,132,135,139,148,152,153,154,172,179,182,183,184,185,186,187,188,189,201,202,216,223,224,242,],[107,107,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,107,107,107,107,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,107,107,107,107,107,-41,107,-77,-101,107,-50,-51,-54,-55,-56,107,-79,-57,107,107,107,-78,]),'TO':([80,81,82,83,84,85,86,87,88,89,90,91,108,110,118,119,122,123,127,128,129,130,131,132,152,153,154,172,179,182,184,185,186,187,188,194,201,202,242,],[-105,-105,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-36,-38,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,-37,-39,-41,-124,-77,-101,-50,-51,-54,-55,-56,210,-79,-57,-78,]),'LT':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[112,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,112,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'LE':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[113,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,113,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'GT':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[114,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,114,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'GE':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[115,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,115,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'EQ':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[116,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,116,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'NE':([81,82,83,84,85,86,87,88,89,90,91,118,119,122,123,127,128,129,130,131,132,154,179,182,184,185,186,187,188,201,202,242,],[117,-105,-105,-105,-106,-107,-108,-109,-109,-63,-64,-40,-49,-48,-53,-52,-58,-59,-60,-61,-62,117,-77,-101,-50,-51,-54,-55,-56,-79,-57,-78,]),'PLUS':([82,83,84,85,86,87,88,89,90,91,119,122,123,127,128,129,130,131,132,179,182,184,185,186,187,188,201,202,242,],[120,-105,-105,-106,-107,-108,-109,-109,-63,-64,-49,-48,-53,-52,-58,-59,-60,-61,-62,-77,-101,120,120,-54,-55,-56,-79,-57,-78,]),'MINUS':([82,83,84,85,86,87,88,89,90,91,119,122,123,127,128,129,130,131,132,179,182,184,185,186,187,188,201,202,242,],[121,-105,-105,-106,-107,-108,-109,-109,-63,-64,-49,-48,-53,-52,-58,-59,-60,-61,-62,-77,-101,121,121,-54,-55,-56,-79,-57,-78,]),'MULT':([83,84,85,86,87,88,89,90,91,123,127,128,129,130,131,132,179,182,186,187,188,201,202,242,],[124,-105,-106,-107,-108,-109,-109,-63,-64,-53,-52,-58,-59,-60,-61,-62,-77,-101,124,124,124,-79,-57,-78,]),'DIV':([83,84,85,86,87,88,89,90,91,123,127,128,129,130,131,132,179,182,186,187,188,201,202,242,],[125,-105,-106,-107,-108,-109,-109,-63,-64,-53,-52,-58,-59,-60,-61,-62,-77,-101,125,125,125,-79,-57,-78,]),'MOD':([83,84,85,86,87,88,89,90,91,123,127,128,129,130,131,132,179,182,186,187,188,201,202,242,],[126,-105,-106,-107,-108,-109,-109,-63,-64,-53,-52,-58,-59,-60,-61,-62,-77,-101,126,126,126,-79,-57,-78,]),'RBRACKET':([174,225,],[197,236,]),'ELSEIF':([190,232,],[205,-90,]),'ELSE':([190,232,],[206,-90,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'programa':([0,],[1,]),'gotomain':([0,],[2,]),'addDir':([3,10,34,74,248,],[4,16,56,100,254,]),'programa2':([6,8,],[7,15,]),'atributos':([6,8,226,],[8,8,237,]),'programa3':([6,8,11,],[9,9,17,]),'metodos':([6,8,11,],[11,11,11,]),'programa4':([6,8,11,],[12,12,12,]),'setmainloc':([6,8,11,],[14,14,14,]),'funciones':([13,32,35,36,57,95,191,222,226,237,239,240,253,254,],[18,44,44,44,44,44,44,44,44,44,44,44,44,256,]),'tipo':([13,16,32,35,36,57,95,100,143,191,214,222,226,237,239,240,241,253,254,],[19,28,19,19,19,19,19,145,28,19,28,19,19,19,19,19,145,19,19,]),'principal':([14,],[25,]),'var':([16,143,214,],[27,176,227,]),'funciones2':([19,20,],[29,31,]),'estatutos':([32,35,36,57,95,191,222,226,237,239,240,253,],[35,57,60,57,136,207,233,239,239,239,239,255,]),'estatutos2':([32,35,36,57,95,191,222,226,237,239,240,253,],[36,36,36,36,36,36,36,36,36,36,36,36,]),'asigna':([32,35,36,57,95,191,222,226,237,239,240,253,],[37,37,37,37,37,37,37,37,37,37,37,37,]),'llamada':([32,35,36,57,63,64,66,76,77,94,95,98,103,105,109,111,137,150,161,162,163,164,165,167,191,195,200,209,210,222,226,237,239,240,253,],[38,38,38,38,90,90,90,90,90,90,38,90,90,90,90,90,90,90,90,90,90,90,90,90,38,90,90,90,90,38,38,38,38,38,38,]),'lee':([32,35,36,57,95,191,222,226,237,239,240,253,],[39,39,39,39,39,39,39,39,39,39,39,39,]),'escribe':([32,35,36,57,95,191,222,226,237,239,240,253,],[40,40,40,40,40,40,40,40,40,40,40,40,]),'condicion':([32,35,36,57,95,191,222,226,237,239,240,253,],[41,41,41,41,41,41,41,41,41,41,41,41,]),'ciclow':([32,35,36,57,95,191,222,226,237,239,240,253,],[42,42,42,42,42,42,42,42,42,42,42,42,]),'ciclof':([32,35,36,57,95,191,222,226,237,239,240,253,],[43,43,43,43,43,43,43,43,43,43,43,43,]),'retornar':([32,35,36,57,95,191,222,226,237,239,240,253,],[45,45,45,45,45,45,45,45,45,45,45,45,]),'var2':([33,142,],[54,175,]),'principal2':([35,57,],[58,75,]),'callFunc':([46,91,],[62,62,]),'condicion2':([49,219,],[65,230,]),'storeWhile':([50,51,],[67,68,]),'ciclof2':([52,70,],[69,96,]),'ciclof3':([52,70,],[70,70,]),'returnFlag':([53,],[72,]),'setCurrentID':([55,178,],[73,199,]),'asignarid':([61,],[76,]),'exp':([63,64,66,76,77,94,98,103,105,137,150,167,195,200,209,210,],[79,92,93,101,102,135,139,148,152,172,183,189,139,216,223,224,]),'texp':([63,64,66,76,77,94,98,103,105,109,137,150,167,195,200,209,210,],[80,80,80,80,80,80,80,80,80,153,80,80,80,80,80,80,80,]),'gexp':([63,64,66,76,77,94,98,103,105,109,111,137,150,167,195,200,209,210,],[81,81,81,81,81,81,81,81,81,81,154,81,81,81,81,81,81,81,]),'mexp':([63,64,66,76,77,94,98,103,105,109,111,137,150,161,162,167,195,200,209,210,],[82,82,82,82,82,82,82,82,82,82,82,82,82,184,185,82,82,82,82,82,]),'t':([63,64,66,76,77,94,98,103,105,109,111,137,150,161,162,163,164,165,167,195,200,209,210,],[83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,186,187,188,83,83,83,83,83,]),'f':([63,64,66,76,77,94,98,103,105,109,111,137,150,161,162,163,164,165,167,195,200,209,210,],[84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,]),'addForId':([71,],[97,]),'addValueVarT':([73,199,],[99,215,]),'addFakeBottom':([78,],[103,]),'cuadlee':([79,183,],[104,203,]),'addOL':([79,80,92,93,101,102,135,139,148,152,153,172,183,189,216,223,224,],[105,109,105,105,105,105,105,105,105,105,109,105,105,105,105,105,105,]),'checarexp':([80,81,82,83,84,],[108,110,118,122,127,]),'gexp2':([81,154,],[111,111,]),'mexp2':([82,184,185,],[119,119,119,]),'t2':([83,186,187,188,],[123,123,123,123,]),'addIntType':([85,],[128,]),'addFloatType':([86,],[129,]),'addCharType':([87,],[130,]),'addBoolType':([88,89,],[131,132,]),'cuadprint':([92,189,],[133,204,]),'retornar2':([98,195,],[138,211,]),'var3':([99,197,236,],[140,213,246,]),'parametros':([100,241,],[144,251,]),'genParameter':([102,216,],[147,229,]),'lee2':([104,203,],[149,217,]),'addO':([112,113,114,115,116,117,120,121,124,125,126,],[155,156,157,158,159,160,161,162,163,164,165,]),'escribe2':([133,204,],[166,218,]),'genGTF':([134,170,],[169,192,]),'cuadRetornar':([139,],[173,]),'llamada2':([147,229,],[179,242,]),'verLastParam':([147,229,],[181,181,]),'condicion3':([169,220,],[190,231,]),'genFor':([172,],[194,]),'addParamCount':([177,],[198,]),'paramCounter':([180,],[200,]),'popFakeBottom':([182,],[202,]),'genElse':([205,206,],[219,220,]),'fillGoto':([207,],[221,]),'addContF':([214,227,],[226,240,]),'newParamType':([215,],[228,]),'compFor':([224,],[235,]),'funciones3':([226,237,239,240,],[238,247,249,250,]),'genDoWhile':([234,],[244,]),'fillWhile':([243,],[252,]),'actFor':([257,],[258,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> programa","S'",1,None,None,None),
  ('programa -> gotomain PROGRAMA addDir ID SEMI programa2','programa',6,'p_programa','Dash-Compilador.py',214),
  ('programa2 -> atributos programa2','programa2',2,'p_programa2','Dash-Compilador.py',225),
  ('programa2 -> programa3','programa2',1,'p_programa2','Dash-Compilador.py',226),
  ('programa3 -> metodos programa3','programa3',2,'p_programa3','Dash-Compilador.py',230),
  ('programa3 -> programa4','programa3',1,'p_programa3','Dash-Compilador.py',231),
  ('programa4 -> setmainloc principal','programa4',2,'p_programa4','Dash-Compilador.py',235),
  ('atributos -> ATRIBUTOS addDir var','atributos',3,'p_atributos','Dash-Compilador.py',241),
  ('metodos -> METODOS funciones','metodos',2,'p_metodos','Dash-Compilador.py',247),
  ('principal -> PRINCIPAL LPAREN estatutos principal2','principal',4,'p_principal','Dash-Compilador.py',253),
  ('principal2 -> estatutos principal2','principal2',2,'p_principal2','Dash-Compilador.py',257),
  ('principal2 -> RPAREN','principal2',1,'p_principal2','Dash-Compilador.py',258),
  ('funciones -> tipo funciones2','funciones',2,'p_funciones','Dash-Compilador.py',264),
  ('funciones -> VOID funciones2','funciones',2,'p_funciones','Dash-Compilador.py',265),
  ('funciones2 -> FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF atributos funciones3','funciones2',12,'p_funciones2','Dash-Compilador.py',269),
  ('funciones2 -> FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES addContF funciones3','funciones2',11,'p_funciones2','Dash-Compilador.py',270),
  ('funciones2 -> FUNCION ID addDir LPAREN addDir parametros RPAREN addParamCount LBRACES var addContF funciones3','funciones2',12,'p_funciones2','Dash-Compilador.py',271),
  ('funciones3 -> estatutos RBRACES addDir funciones','funciones3',4,'p_funciones3','Dash-Compilador.py',276),
  ('funciones3 -> estatutos RBRACES addDir','funciones3',3,'p_funciones3','Dash-Compilador.py',277),
  ('funciones3 -> estatutos funciones3','funciones3',2,'p_funciones3','Dash-Compilador.py',278),
  ('parametros -> tipo ID setCurrentID addValueVarT newParamType COMMA parametros','parametros',7,'p_parametros','Dash-Compilador.py',284),
  ('parametros -> tipo ID setCurrentID addValueVarT newParamType','parametros',5,'p_parametros','Dash-Compilador.py',285),
  ('retornar -> RETURN returnFlag LPAREN retornar2','retornar',4,'p_retornar','Dash-Compilador.py',291),
  ('retornar2 -> exp cuadRetornar COMMA retornar2','retornar2',4,'p_retornar2','Dash-Compilador.py',295),
  ('retornar2 -> exp cuadRetornar RPAREN','retornar2',3,'p_retornar2','Dash-Compilador.py',296),
  ('var -> tipo COLON var2','var',3,'p_var','Dash-Compilador.py',302),
  ('var2 -> ID setCurrentID addValueVarT var3','var2',4,'p_var2','Dash-Compilador.py',306),
  ('var2 -> ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET var3','var2',7,'p_var2','Dash-Compilador.py',307),
  ('var2 -> ID setCurrentID addValueVarT LBRACKET CTE_I RBRACKET LBRACKET CTE_I RBRACKET var3','var2',10,'p_var2','Dash-Compilador.py',308),
  ('var3 -> COMMA var2','var3',2,'p_var3','Dash-Compilador.py',313),
  ('var3 -> SEMI var','var3',2,'p_var3','Dash-Compilador.py',314),
  ('var3 -> SEMI','var3',1,'p_var3','Dash-Compilador.py',315),
  ('tipo -> INT','tipo',1,'p_tipo','Dash-Compilador.py',321),
  ('tipo -> FLOAT','tipo',1,'p_tipo','Dash-Compilador.py',322),
  ('tipo -> CHAR','tipo',1,'p_tipo','Dash-Compilador.py',323),
  ('tipo -> BOOL','tipo',1,'p_tipo','Dash-Compilador.py',324),
  ('exp -> texp checarexp','exp',2,'p_exp','Dash-Compilador.py',333),
  ('exp -> exp addOL exp','exp',3,'p_exp','Dash-Compilador.py',334),
  ('texp -> gexp checarexp','texp',2,'p_texp','Dash-Compilador.py',338),
  ('texp -> texp addOL texp','texp',3,'p_texp','Dash-Compilador.py',339),
  ('gexp -> mexp checarexp','gexp',2,'p_gexp','Dash-Compilador.py',343),
  ('gexp -> gexp gexp2 gexp','gexp',3,'p_gexp','Dash-Compilador.py',344),
  ('gexp2 -> LT addO','gexp2',2,'p_gexp2','Dash-Compilador.py',348),
  ('gexp2 -> LE addO','gexp2',2,'p_gexp2','Dash-Compilador.py',349),
  ('gexp2 -> GT addO','gexp2',2,'p_gexp2','Dash-Compilador.py',350),
  ('gexp2 -> GE addO','gexp2',2,'p_gexp2','Dash-Compilador.py',351),
  ('gexp2 -> EQ addO','gexp2',2,'p_gexp2','Dash-Compilador.py',352),
  ('gexp2 -> NE addO','gexp2',2,'p_gexp2','Dash-Compilador.py',353),
  ('mexp -> t checarexp','mexp',2,'p_mexp','Dash-Compilador.py',357),
  ('mexp -> mexp mexp2','mexp',2,'p_mexp','Dash-Compilador.py',358),
  ('mexp2 -> PLUS addO mexp','mexp2',3,'p_mexp2','Dash-Compilador.py',362),
  ('mexp2 -> MINUS addO mexp','mexp2',3,'p_mexp2','Dash-Compilador.py',363),
  ('t -> f checarexp','t',2,'p_t','Dash-Compilador.py',367),
  ('t -> t t2','t',2,'p_t','Dash-Compilador.py',368),
  ('t2 -> MULT addO t','t2',3,'p_t2','Dash-Compilador.py',372),
  ('t2 -> DIV addO t','t2',3,'p_t2','Dash-Compilador.py',373),
  ('t2 -> MOD addO t','t2',3,'p_t2','Dash-Compilador.py',374),
  ('f -> LPAREN addFakeBottom exp RPAREN popFakeBottom','f',5,'p_f','Dash-Compilador.py',378),
  ('f -> CTE_I addIntType','f',2,'p_f','Dash-Compilador.py',379),
  ('f -> CTE_F addFloatType','f',2,'p_f','Dash-Compilador.py',380),
  ('f -> CTE_S addCharType','f',2,'p_f','Dash-Compilador.py',381),
  ('f -> TRUE addBoolType','f',2,'p_f','Dash-Compilador.py',382),
  ('f -> FALSE addBoolType','f',2,'p_f','Dash-Compilador.py',383),
  ('f -> llamada','f',1,'p_f','Dash-Compilador.py',384),
  ('f -> ID','f',1,'p_f','Dash-Compilador.py',385),
  ('estatutos -> estatutos2 estatutos','estatutos',2,'p_estatutos','Dash-Compilador.py',405),
  ('estatutos -> estatutos2','estatutos',1,'p_estatutos','Dash-Compilador.py',406),
  ('estatutos2 -> asigna','estatutos2',1,'p_estatutos2','Dash-Compilador.py',410),
  ('estatutos2 -> llamada','estatutos2',1,'p_estatutos2','Dash-Compilador.py',411),
  ('estatutos2 -> lee','estatutos2',1,'p_estatutos2','Dash-Compilador.py',412),
  ('estatutos2 -> escribe','estatutos2',1,'p_estatutos2','Dash-Compilador.py',413),
  ('estatutos2 -> condicion','estatutos2',1,'p_estatutos2','Dash-Compilador.py',414),
  ('estatutos2 -> ciclow','estatutos2',1,'p_estatutos2','Dash-Compilador.py',415),
  ('estatutos2 -> ciclof','estatutos2',1,'p_estatutos2','Dash-Compilador.py',416),
  ('estatutos2 -> funciones','estatutos2',1,'p_estatutos2','Dash-Compilador.py',417),
  ('estatutos2 -> retornar','estatutos2',1,'p_estatutos2','Dash-Compilador.py',418),
  ('asigna -> ID EQUALS asignarid exp SEMI','asigna',5,'p_asigna','Dash-Compilador.py',424),
  ('llamada -> ID callFunc LPAREN exp genParameter llamada2','llamada',6,'p_llamada','Dash-Compilador.py',446),
  ('llamada2 -> COMMA paramCounter exp genParameter llamada2','llamada2',5,'p_llamada2','Dash-Compilador.py',451),
  ('llamada2 -> verLastParam RPAREN','llamada2',2,'p_llamada2','Dash-Compilador.py',452),
  ('lee -> LEE LPAREN exp cuadlee lee2','lee',5,'p_lee','Dash-Compilador.py',458),
  ('lee2 -> COMMA exp cuadlee lee2','lee2',4,'p_lee2','Dash-Compilador.py',463),
  ('lee2 -> RPAREN','lee2',1,'p_lee2','Dash-Compilador.py',464),
  ('escribe -> ESCRIBIR LPAREN exp cuadprint escribe2','escribe',5,'p_escribe','Dash-Compilador.py',470),
  ('escribe2 -> COMMA exp cuadprint escribe2','escribe2',4,'p_escribe2','Dash-Compilador.py',475),
  ('escribe2 -> RPAREN','escribe2',1,'p_escribe2','Dash-Compilador.py',476),
  ('condicion -> IF condicion2','condicion',2,'p_condicion','Dash-Compilador.py',482),
  ('condicion2 -> LPAREN exp RPAREN genGTF condicion3 ELSEIF genElse condicion2','condicion2',8,'p_condicion2','Dash-Compilador.py',487),
  ('condicion2 -> LPAREN exp RPAREN genGTF condicion3 ELSE genElse condicion3','condicion2',8,'p_condicion2','Dash-Compilador.py',488),
  ('condicion2 -> LPAREN exp RPAREN genGTF condicion3','condicion2',5,'p_condicion2','Dash-Compilador.py',489),
  ('condicion3 -> LBRACES estatutos fillGoto RBRACES','condicion3',4,'p_condicion3','Dash-Compilador.py',494),
  ('ciclow -> WHILE storeWhile LPAREN exp RPAREN genGTF DO LBRACES estatutos RBRACES fillWhile','ciclow',11,'p_ciclow','Dash-Compilador.py',500),
  ('ciclow -> DO storeWhile LBRACES estatutos RBRACES WHILE LPAREN exp RPAREN genDoWhile','ciclow',10,'p_ciclow','Dash-Compilador.py',501),
  ('ciclof -> FOR ciclof2','ciclof',2,'p_ciclof','Dash-Compilador.py',507),
  ('ciclof2 -> ciclof3 ciclof2','ciclof2',2,'p_ciclof2','Dash-Compilador.py',512),
  ('ciclof2 -> ciclof3','ciclof2',1,'p_ciclof2','Dash-Compilador.py',513),
  ('ciclof3 -> ID addForId EQUALS exp genFor TO exp compFor DO LBRACES estatutos RBRACES actFor','ciclof3',13,'p_ciclof3','Dash-Compilador.py',518),
  ('addDir -> <empty>','addDir',0,'p_addDir','Dash-Compilador.py',537),
  ('setCurrentID -> <empty>','setCurrentID',0,'p_setCurrentID','Dash-Compilador.py',595),
  ('addValueVarT -> <empty>','addValueVarT',0,'p_addValueVarT','Dash-Compilador.py',604),
  ('addFakeBottom -> <empty>','addFakeBottom',0,'p_addFakeBottom','Dash-Compilador.py',619),
  ('popFakeBottom -> <empty>','popFakeBottom',0,'p_popFakeBottom','Dash-Compilador.py',626),
  ('addO -> <empty>','addO',0,'p_addO','Dash-Compilador.py',641),
  ('addOL -> LOR','addOL',1,'p_addOL','Dash-Compilador.py',648),
  ('addOL -> LAND','addOL',1,'p_addOL','Dash-Compilador.py',649),
  ('checarexp -> <empty>','checarexp',0,'p_checarexp','Dash-Compilador.py',656),
  ('addIntType -> <empty>','addIntType',0,'p_addIntType','Dash-Compilador.py',700),
  ('addFloatType -> <empty>','addFloatType',0,'p_addFloatType','Dash-Compilador.py',715),
  ('addCharType -> <empty>','addCharType',0,'p_addCharType','Dash-Compilador.py',730),
  ('addBoolType -> <empty>','addBoolType',0,'p_addBoolType','Dash-Compilador.py',745),
  ('asignarid -> <empty>','asignarid',0,'p_asignarid','Dash-Compilador.py',759),
  ('gotomain -> <empty>','gotomain',0,'p_gotomain','Dash-Compilador.py',767),
  ('setmainloc -> <empty>','setmainloc',0,'p_setmainloc','Dash-Compilador.py',773),
  ('cuadprint -> <empty>','cuadprint',0,'p_cuadprint','Dash-Compilador.py',784),
  ('cuadlee -> <empty>','cuadlee',0,'p_cuadlee','Dash-Compilador.py',796),
  ('returnFlag -> <empty>','returnFlag',0,'p_returnFlag','Dash-Compilador.py',813),
  ('cuadRetornar -> <empty>','cuadRetornar',0,'p_cuadRetornar','Dash-Compilador.py',822),
  ('genGTF -> <empty>','genGTF',0,'p_genGTF','Dash-Compilador.py',838),
  ('genElse -> <empty>','genElse',0,'p_genElse','Dash-Compilador.py',854),
  ('fillGoto -> <empty>','fillGoto',0,'p_fillGoto','Dash-Compilador.py',868),
  ('storeWhile -> <empty>','storeWhile',0,'p_storeWhile','Dash-Compilador.py',893),
  ('genDoWhile -> <empty>','genDoWhile',0,'p_genDoWhile','Dash-Compilador.py',901),
  ('fillWhile -> <empty>','fillWhile',0,'p_fillWhile','Dash-Compilador.py',916),
  ('addForId -> <empty>','addForId',0,'p_addForId','Dash-Compilador.py',936),
  ('genFor -> <empty>','genFor',0,'p_genFor','Dash-Compilador.py',947),
  ('compFor -> <empty>','compFor',0,'p_compFor','Dash-Compilador.py',975),
  ('actFor -> <empty>','actFor',0,'p_actFor','Dash-Compilador.py',1017),
  ('newParamType -> <empty>','newParamType',0,'p_newParamType','Dash-Compilador.py',1060),
  ('addParamCount -> <empty>','addParamCount',0,'p_addParamCount','Dash-Compilador.py',1070),
  ('genParameter -> <empty>','genParameter',0,'p_genParameter','Dash-Compilador.py',1083),
  ('addContF -> <empty>','addContF',0,'p_addContF','Dash-Compilador.py',1098),
  ('callFunc -> <empty>','callFunc',0,'p_callFunc','Dash-Compilador.py',1108),
  ('paramCounter -> <empty>','paramCounter',0,'p_paramCounter','Dash-Compilador.py',1120),
  ('verLastParam -> <empty>','verLastParam',0,'p_verLastParam','Dash-Compilador.py',1128),
]
