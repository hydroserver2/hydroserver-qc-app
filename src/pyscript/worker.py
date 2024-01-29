from edit_service import EditService, TimeUnit, FilterOperation
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
print("SETTING FILTER...")
edit_service.set_filter({f'{FilterOperation.GTE.value}': 10.45})
print(edit_service.filtered_df)

print("FINDING GAPS...")
# Find gaps
print(edit_service.find_gaps(16, TimeUnit.MINUTE))
