setup:
	uv sync

dev:
	uv run flask run

start: setup
	uv run gunicorn -w 2 "app:app"
