setup:
	poetry install

dev:
	poetry run flask run

start:
	poetry run gunicorn -w 2 "app:app"
