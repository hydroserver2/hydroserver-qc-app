print("Worker thread...")
from js import createObject, someFunction
from pyscript import when, Element
from pyodide.ffi import create_proxy

# Note: when binding events, the HTML component must be rendered and not hidden in the document
@when("click", "#my_button")
def click_handler(event):
    someFunction("Hello from Python!")

some_global = "This is a test global variable in Python"

# Expose globals
# createObject("pyGlobals", create_proxy(globals()))

# Signal start
Element("start").element.click()