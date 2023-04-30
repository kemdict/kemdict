# Hacky scripts

These are here because

- I want to read data from another server (specifically pojtl.kemdict.com) on the server side at request time
- We need top level await to not have to do it on the client side, but [Svelte doesn't yet seem to support it](https://github.com/sveltejs/svelte/issues/5501)
- So, uh… `spawnSync` it is then.
