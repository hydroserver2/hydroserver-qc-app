# Worker function parameters and returned values are constrained to serializable data
# https://docs.pyscript.net/2024.11.1/user-guide/workers/
# For this reason, clases can't be instantiated from here
# because we won't be able to return the instances