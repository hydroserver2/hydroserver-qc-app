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
  # https://www.jhanley.com/blog/pyscript-javascript-and-python-interoperability/
  # https://pyodide.org/en/stable/usage/api/python-api/ffi.html#
  return edit_service._df


def find_gaps(time_value, time_unit):
  return edit_service.find_gaps(time_value, time_unit).to_json()


def fill_gaps(gap, fill):
  return edit_service.fill_gap(gap, fill).to_json()


def delete_data_points(index):
  return edit_service.delete_points(index)


def set_filter(filter: dict[FilterOperation, float]):
  return edit_service.filter(json.loads(filter))


def change_values(index, operator, value):
  edit_service.change_values(index, operator, value)


def add_points(points):
  return edit_service.add_points(points)


def shift_points(index_list, time_value, time_unit):
  return edit_service.shift_points(index_list, time_value, time_unit)


def interpolate(index_list):
  return edit_service.interpolate(index_list)


def drift_correction(index_list, gap_width):
  return edit_service.drift_correction(index_list, gap_width)


# Signal start
Element("start").element.click()
