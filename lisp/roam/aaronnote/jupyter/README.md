# Aaronnote Jupyter Kernel Server

This directory contains the minimal local Jupyter runtime used by Aaronnote
`@@cell` blocks. It is not a JupyterLab frontend integration.

The runtime provides:

- a private virtualenv under `jupyter/.venv`
- Jupyter Server and `ipykernel`
- optional local kernelspec templates, such as Sage
- isolated Jupyter config/data/runtime directories under `jupyter/.jupyter`

From `lisp/roam/aaronnote`:

```sh
npm run jupyter:bootstrap
npm run jupyter:server
```

`jupyter/scripts/run-jupyter-server.sh` starts `jupyter-server` for the
Aaronnote cell service; it does not start JupyterLab.
