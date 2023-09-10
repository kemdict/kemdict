# Architecture

This document describes this code base.

Kemdict is both an alternative interface to several dictionaries, as well as an attempt to create my own dictionary — I refer to the latter as `kisaragi_dict`.

`dicts` is a subproject responsible for turning the raw data into a particular shape, with definitions of the same word from each dictionary being merged into the same object. This is then also turned into an SQLite database for quicker access.

`dicts/kisaragi` contains the dictionary data for `kisaragi_dict`. They are defined in Org files, and get turned into JSON by [./dicts/kisaragi/generate.el](./dicts/kisaragi/generate.el).

`web` is the frontend, made with Svelte and Astro. It has server side code and is hosted on a server. The server side loads the dictionary data and renders pages for the client.

Make is used as the task runner.

The project is built on GitHub Actions. After it's built, the action SSH's into the server, replaces the deployed server with the newly built server, then restarts the application there. Each deploy will cause a few seconds of downtime.
