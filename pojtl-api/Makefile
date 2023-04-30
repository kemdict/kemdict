setup:
	poetry install

dev:
	poetry run flask run

start: setup
	poetry run gunicorn -w 2 "app:app"
