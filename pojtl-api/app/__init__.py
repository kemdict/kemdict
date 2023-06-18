from flask import Flask, request, Response
from kesi import Ku
from markupsafe import escape

app = Flask(__name__)
app.logger.setLevel("INFO")


@app.errorhandler(404)
def page_not_found(_error):
    return "404", 404


@app.errorhandler(405)
def wrong_method(_error):
    return "wrong HTTP method", 405


@app.route("/")
def home():
    return Response(
        """
Routes:

  GET: /toTL/<text>
  POST: /toTL
    <data>

  GET: /toPOJ/<text>
  POST: /toPOJ
    <data>

  GET: /count/<text>
  POST: /count
    <data>
""".strip(),
        mimetype="text/plain",
    )


@app.route("/toTL/<text>", methods=["GET"])
@app.route("/toTL/", methods=["POST"])
def to_tl(text=None):
    if text is None:
        text = request.data.decode("utf-8")
    app.logger.info(f"/toTL/{text or ''}")
    return escape(Ku(text).TL().hanlo)


@app.route("/toPOJ/<text>", methods=["GET"])
@app.route("/toPOJ/", methods=["POST"])
def to_poj(text=None):
    if text is None:
        text = request.data.decode("utf-8")
    app.logger.info(f"/toPOJ/{text or ''}")
    return escape(Ku(text).POJ().hanlo)


@app.route("/count/<text>", methods=["GET"])
@app.route("/count/", methods=["POST"])
def count(text=None):
    if text is None:
        text = request.data.decode("utf-8")
    app.logger.info(f"/count/{text}")
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
