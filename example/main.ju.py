# %% [markdown]
# # aaron-neopyter example notebook
#
# This file demonstrates the Jupytext percent format.
# Open in Emacs, run `aaron-neopyter-connect`, then `aaron-neopyter-open-notebook`.

# %%
import sys
print(f"Python {sys.version}")

# %% [markdown]
# ## Data exploration

# %%
import os

data = list(range(10))
print(data)

# %%
# Modify this cell and watch JupyterLab update without a manual reload.
doubled = [x * 2 for x in data]
print(doubled)

# %% [markdown]
# ## Computation

# %%
total = sum(doubled)
print(f"Total: {total}")

# %%
# Run this cell with C-c C-c to see kernel output in JupyterLab.
for i, val in enumerate(doubled):
    if val > 10:
        print(f"index {i}: {val}")
