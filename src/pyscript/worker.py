from edit_service import EditService, FilterOperation
from pyscript import when, Element
from js import someFunction, dataset
import json

print("==== Worker thread ====")

# Note: when binding events, the HTML component must be rendered in the document
edit_service = EditService("test", json.loads(dataset))


@when("click", "#my_button")
def click_handler(event):
  someFunction("Hello from Python!")


def get_data_frame():
  # TODO: `edit_service._df` will return a JsProxy of a PyProxy, which we cannot use in JS.
  # https://www.jhanley.com/blog/pyscript-javascript-and-python-interoperability/
  # https://pyodide.org/en/stable/usage/api/python-api/ffi.html#
  # As a workaround we use `to_json` to serialize the data and send it to JS, but this cannot be made reactive.
  return edit_service._df.to_json()


def find_gaps(value, unit):
  return edit_service.find_gaps(value, unit).to_json()


def fill_gaps(gap, fill):
  return edit_service.fill_gap(gap, fill).to_json()


def delete_data_points(index):
  return edit_service.delete_points(index)


def set_filter(filter: dict[FilterOperation, float]):
  return edit_service.filter(json.loads(filter)).to_json()


def change_values(index, operator, value):
  return edit_service.change_values(index, operator, value).to_json()


def add_points(points):
  return edit_service.add_points(points)


# Signal start
Element("start").element.click()
