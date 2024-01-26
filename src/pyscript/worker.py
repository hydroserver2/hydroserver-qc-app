print("==== Worker thread ====")
import json
from js import someFunction, dataset
from pyscript import when, Element
from edit_service import EditService

# Note: when binding events, the HTML component must be rendered in the document
@when("click", "#my_button")
def click_handler(event):
    someFunction("Hello from Python!")

def handleDelete(data, index):
  if len(data) >= index + 1:
    del data[index]

def handleAddOne(data, index):
  if len(data) >= index + 1:
    data[index][1] = data[index][1] + 1

# Signal start
Element("start").element.click()

EditService("test", json.loads(dataset))