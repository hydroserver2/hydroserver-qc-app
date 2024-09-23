from edit_service import EditService, FilterOperation
from pyscript import when, Element
# from js import someFunction, dataset
# from js import someFunction
import json
from datetime import datetime


print("==== Worker thread ====")

# Note: when binding events, the HTML component must be rendered in the document
# edit_service = EditService("test", json.loads(dataset))

services = []

DATETIME_COL_INDEX = 0
VALUE_COL_INDEX = 1

# @when("click", "#my_button")
# def click_handler(event):
#   someFunction("Hello from Python!")

class edit_service_wrapper():
  def __init__(self, data) -> None:
    self.edit_service = EditService(json.loads(data))

  def get_data_frame(self):
    # https://www.jhanley.com/blog/pyscript-javascript-and-python-interoperability/
    # https://pyodide.org/en/stable/usage/api/python-api/ffi.html#
    return self.edit_service.get_dataframe()


  def find_gaps(self, time_value, time_unit):
    return self.edit_service.find_gaps(time_value, time_unit)


  def fill_gaps(self, gap, fill, interpolate_values):
    return self.edit_service.fill_gap(gap, fill, interpolate_values)


  def delete_data_points(self, index):
    return self.edit_service.delete_points(index)


  def set_filter(self, filter: dict[FilterOperation, float]):
    return self.edit_service.filter(json.loads(filter))


  def change_values(self, index_list, operator, value):
    self.edit_service.change_values(index_list.to_py(), operator, value)


  def add_points(self, points):
    # Parse date fields
    for i, p in enumerate(points):
      points[i][0] = datetime.strptime(
        p[0], "%Y-%m-%dT%H:%M:%SZ")

    return self.edit_service.add_points(points)


  def shift_points(self, index_list, time_value, time_unit):
    return self.edit_service.shift_points(index_list.to_py(), time_value, time_unit)


  def interpolate(self, index_list):
    return self.edit_service.interpolate(index_list.to_py())


  def drift_correction(self, start, end, gap_width):
    return self.edit_service.drift_correction(start, end, gap_width)


  def get_index_at(self, index):
    return self.edit_service.get_dataframe().index[index]


  def get_datetime_at(self, index):
    val = self.edit_service._df._mgr.arrays[DATETIME_COL_INDEX][0][index]
    return (val.value / 10 ** 6)


  def get_value_at(self, index):
    return self.edit_service._df._mgr.arrays[VALUE_COL_INDEX][0][index].item()
  

  def count(self):
    return len(self.get_data_frame().index)
  
  
  def get_date_column(self):
    timestamps = self.edit_service._df._mgr.arrays[DATETIME_COL_INDEX][0]
    return  [ts.value / 10 ** 6 for ts in timestamps]
  

  def get_value_column(self):
    values = self.edit_service._df._mgr.arrays[VALUE_COL_INDEX][0]
    return  [float(val) for val in values]


# Signal start
Element("start").element.click()