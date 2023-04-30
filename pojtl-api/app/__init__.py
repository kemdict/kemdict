from flask import Flask, request, Response
from kesi import Ku
from markupsafe import escape

app = Flask(__name__)


@app.route("/")
def home():
    return Response(
        """
Routes:

  GET: /toTL/<text>
  POST: /toTL
    data=<text>

  GET: /toPOJ/<text>
  POST: /toPOJ
    data=<text>

  GET: /count/<text>
  POST: /count
    data=<text>
""".strip(),
        mimetype="text/plain",
    )


@app.route("/toTL/<text>", methods=["GET"])
@app.route("/toTL", methods=["POST"])
def to_tl(text=None):
    if text is None:
        text = request.values["data"]
    return escape(Ku(text).TL().hanlo)


@app.route("/toPOJ/<text>", methods=["GET"])
@app.route("/toPOJ", methods=["POST"])
def to_poj(text=None):
    if text is None:
        text = request.values["data"]
    return escape(Ku(text).POJ().hanlo)


@app.route("/count/<text>", methods=["GET"])
@app.route("/count", methods=["POST"])
def count(text=None):
    if text is None:
        text = request.values["data"]
    return escape(len(list(Ku(text).thianji())))


@app.route("/robots.txt")
def robots():
    return Response(
        """
User-agent: *
Disallow: /
""".strip(),
        mimetype="text/plain",
    )
