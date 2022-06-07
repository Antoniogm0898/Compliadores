# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

from faulthandler import disable
from dash import Dash, dcc, html, callback_context
from dash.dependencies import Input, Output, State


app = Dash(__name__)

app.layout = html.Div([
    # Titulo
    html.Div([
        html.H1(children='CompDash'),
        html.H3(children='Antonio Gonzalez A01194261'),
        html.H3(children='Gerardo Peart A01194337'),], 
        style={'width' : "100%", 
                'textAlign' : "center", 
                'backgroundColor' : '#8b9b9b', 
                'padding' : 10, 
                'color' : 'white', 
                'height' : '12vh'}),
    html.Div([
        # Columna 1
        html.Div(children=[
            html.Div(children = [
                # Txt file upload
                dcc.Upload( 
                    id = 'uploadTxt', 
                    children = ['INGRESA CODIGO EJECUTABLE AQUI'],
                    style = {'width': '100%',
                            'height': '40px',
                            'lineHeight': '40px',
                            'backgroundColor' : '#a9b3b4', 
                            'marginRight' : '10px', 
                            'marginLeft' : '10px'}
                ), 
                ], style = {"display": "flex", 
                            "justifyContent": "center",
                            'padding' : 10}
            ),
            html.Div(id = "txtFileName", style={'whiteSpace': 'pre-line'}),
            # Text Input
            dcc.Textarea(
                id='textInput',
                value='programa newCode;',
                style={'flex' : 1, 
                       'padding' : 10,
                       'height' : 200},
            ),
            # Execute buttons
            html.Div(children = [
            html.Button('Ejecutar', id='executeCode'),
            html.Button('Crear OBJ', id='createOBJ'),
            html.Button('Crear TXT', id='crearTXT'),
            ], style = {'display': 'flex', 
                        'flex-direction': 'row', 
                        'marginRight' : '10px', 
                        'marginTop' : '5px'}),
        ], style={'padding': 10, 
                  'flex': 1, 
                  'display': 'flex', 
                  'flex-direction': 'column', 
                  'backgroundColor' : '#d1d7d7'}),

        # Columna 2
        html.Div(children=[
            html.Div(children = [
                # Txt file upload
                dcc.Upload( 
                    id = 'uploadObj', 
                    children = ['INGRESA ARCHIVO OBJ AQUI'],
                    style = {'width': '100%',
                            'height': '40px',
                            'lineHeight': '40px',
                            'backgroundColor' : '#a9b3b4', 
                            'marginRight' : '10px', 
                            'marginLeft' : '10px'
                            }
                ), 
                ], style = {"display": "flex", 
                            "justifyContent": "center",
                            'padding' : 10}
            ),
            html.Div(id = "objFileName", style={'whiteSpace': 'pre-line'}),
            dcc.Textarea(
                id='textOutput',
                value='',
                style={'flex' : 1, 
                       'padding' : 10,
                       'height' : 200},
                disabled = True
            )
        ], style={'padding': 10, 
                  'flex': 1, 
                  'display': 'flex', 
                  'flex-direction': 'column', 
                  'backgroundColor' : '#d1d7d7', 
                  'height' : '82vh'}),
    ], style={'width' : "100%", 'textAlign' : "center", 'backgroundColor' : 'green',
              'display': 'flex', 'flex-direction': 'row'}),
], style={'backgroundColor' : '#878683', "overflowY": "auto", "overflowX": "auto"})

if __name__ == '__main__':
    app.run_server(debug=True)

@app.callback(Output('txtFileName', 'children'),
              Output('textInput', 'value'),
              Input('uploadTxt', 'contents'),
              State('uploadTxt', 'filename'))

def loadTextFile(content, filename):
    print("UPDATING?")
    if content is not None and ".txt" in filename:
        # Parseamos el archivo txt
        file = open("ejemplo.txt", "r")
        fullText =  ""
        for line in file:
            fullText = fullText + line
        # Guardamos el nombre del archivo para presentarlo
        filename = 'File : \t {}'.format(filename)
        print("ENTRE")
        print("Se debe cargar objeto txt", content, filename)
        return filename, fullText
    else:
        print("WIUIUWWIUWIU ERORORPRORO")

@app.callback(Input('executeCode'),
              Input('createOBJ'),
              Input('crearTXT')
)

def generateDocs(bt1, bt2, bt3):
    changed_id = [p['prop_id'] for p in callback_context.triggered][0]
    if 'executeCode' in changed_id:
        print("CARGAMOS TXT, CREAMOS OBJ Y EJECUTAMOS")
    elif 'createOBBJ' in changed_id:
        print('SOLO COMPILAMOS')
    else:
        print('DESCARGAMOS TXT DIRECTAMENTE')

if __name__ == '__main__':
    app.run_server(debug=True)