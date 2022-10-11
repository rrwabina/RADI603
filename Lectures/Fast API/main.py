import pandas as pd
import uvicorn
from fastapi import FastAPI
from fastapi.responses import HTMLResponse
import pandera as pa 
from pandera.typing import DataFrame

app = FastAPI()
ref = pd.read_csv("ref.csv")

@app.get("/", response_class=HTMLResponse)
def root():
    return """
            <html>
                <head>
                    <title>HTML Elements Reference</title>
                </head>
                <body>
                    <h1>Rama ECG Project</h1>
                    <p>This is the initial version.</p>
                </body>
            </html>
            """

@app.post('/infer/{text}')
def infer(text: str):
    res = ref[ref["input"] == text]
    if len(res.index) == 0:
        return "Unknown"
    else:
        return res["output"].values[0]

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port = 80, debug = True)