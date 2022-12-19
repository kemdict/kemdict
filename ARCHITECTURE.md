# Architecture

This document describes this code base.

Kemdict is both an alternative interface to several dictionaries, as well as an attempt to create my own dictionary — I refer to the latter as `kisaragi_dict`.

`dicts` is a subproject responsible for turning the raw data into a particular shape, with definitions of the same word from each dictionary being merged into the same object. This is then also turned into an SQLite database for quicker access.

`kisaragi_dict` is an Org Mode file, and it is turned into JSON through Emacs Lisp.

`web` is the frontend, made with SvelteKit. It has server side code and is hosted on a server. The server side loads the dictionary data then returns what the client side needs to render words.

~~`mobile` is an older SvelteKit-based frontend that can be packaged up for mobile via Capacitor. This version has a goal of being offline.~~ This has been deleted. I still wish to create an offline mobile version.

Make is used as the task runner.

The project is built on GitHub Actions. After it's built, the action SSH's into the server, replaces the deployed server with the newly built server, then restarts the application there. Each deploy will cause a few seconds of downtime.
