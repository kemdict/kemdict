# Architecture

This document describes this code base.

Kemdict is both an alternative interface to several dictionaries, as well as an attempt to create my own dictionary — I refer to the latter as `kisaragi_dict`.

`dicts` is a subproject responsible for turning the raw data into a particular shape, with definitions of the same word from each dictionary being merged into the same object. This is then also turned into an SQLite database for quicker access.

`kisaragi_dict` is an Org Mode file, and it is turned into JSON through Emacs Lisp.

`web` is the frontend, made with SvelteKit. It has server side code and is hosted on Netlify. The server side loads the dictionary data then returns what the client side needs to render words.

`mobile` is an older SvelteKit-based frontend that can be packaged up for mobile via Capacitor. This version has a goal of being offline.

Make is used as the task runner.

The project is built on Netlify's CI to ensure we try to run the SQLite interface (`better-sqlite3`) on the same Node version.
