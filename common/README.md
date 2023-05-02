# Common utils

This is linked as a dependency like: (where `../common` points to this folder)

```json
"dependencies": {
  "common": "link:../common"
}
```

After a `pnpm install`, `./node_modules/common` in the consuming package will point to this folder, which would be able to import the file declared in the `main` key.
