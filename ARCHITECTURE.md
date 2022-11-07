# Architecture

This document describes this code base.

Kemdict is an Eleventy project. It is both an alternative interface to several dictionaries, as well as an attempt to create my own dictionary — I refer to the latter as `kisaragi_dict`.

Dictionaries are combined beforehand before being fed to Eleventy. This combination step is done with Emacs Lisp.

`kisaragi_dict` is an Org Mode file, and it is turned into JSON through Emacs Lisp.

Kemdict uses Tailwind CSS. During build, Tailwind is called on the input file and placed into Eleventy's output directory.

Javascript meant to run in the browser are written in TypeScript then transpiled, by Babel, into ES5. The TypeScript compiler is not used: it is only utilized in my editor.

Currently there is no bundler in use, and Make is used as both the task runner and the build system.

Builds are initiated on GitHub Actions when I create a release. The workflow mostly does the usual stuff: install dependencies, build, deploy. Some notes:

- A `git fetch --depth=1 --tags` is initiated to get tag information, which is used for a page.
- Emacs is installed from Ubuntu repositories because it's faster.
- We raise `fs.file-max` (the system open files limit) before the build, and run the Eleventy build with `prlimit --nofile=<a large number>` because Eleventy opens too many files at once when generating this many pages.

## Project configuration files

- Make is used both as the task runner and as the build tool; there is no bundler in use.
- Dependencies are declared in Cask (Emacs Lisp) and `package.json` (Node JS).
- Prettier is configured to treat Nunjucks files as normal HTML.
- Tailwind CSS is configured with `tailwind.config.js` and `postcss.config.js`. This is a consequence of Tailwind's build tool utilizing PostCSS's ecosystem.
- Babel is set up in `.babelrc.json` to transpile TypeScript to ES5.
