from edit_service import EditService, TimeUnit, FilterOperation, Operator
from pyscript import when, Element
from js import someFunction, dataset
import json

print("==== Worker thread ====")

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

edit_service = EditService("test", json.loads(dataset))

# Test operations

# Set filter
print("FILTERING...")
print(edit_service.filter({f'{FilterOperation.GTE.value}': 10.45}))

print("FINDING GAPS...")
# Find gaps
gap = [15, TimeUnit.MINUTE]

gaps = edit_service.find_gaps(gap[0], gap[1])
print(gaps)

print("FILLING GAPS...")
fill = [15, TimeUnit.MINUTE]
print(edit_service.fill_gap(gap, fill))

print("CHANGE VALUE")
print(edit_service.change_value([0, 1], Operator.DIV, 2))
