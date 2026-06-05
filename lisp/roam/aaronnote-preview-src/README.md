# Emacs Markdown Preview Source

This is the editable source for `../aaronnote-preview/`.

It is intentionally reduced to a read-only Markdown renderer. Emacs owns the
document, and `../aaronnote-web-host.mjs` owns transport, attachment routing,
input blocking, and cursor synchronization.

Build from this directory:

```sh
npm install
npm run build
```

The build writes directly to `../aaronnote-preview/`.
