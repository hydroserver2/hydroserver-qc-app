print("Worker thread...")
from js import someFunction
from pyscript import when, Element

# Note: when binding events, the HTML component must be rendered in the document
@when("click", "#my_button")
def click_handler(event):
    someFunction("Hello from Python!")

# Signal start
Element("start").element.click()