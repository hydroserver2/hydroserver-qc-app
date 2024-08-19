import sys
import pandas as pd
import numpy as np
from datetime import datetime
from enum import Enum

# Tools
# [x] Interpolate
# [x] Set a value to a constant
# [x] Change value by applying arithmetic (+, -, *, /)
# [x] Shift
# [x] Drift correction (linear)
# [x] Delete values
# [x] Fill values

# Automation
# [x] Gap filling


class TimeUnit(Enum):
  SECOND = 's'
  MINUTE = 'm'
  HOUR = 'h'
  DAY = 'D'
  WEEK = 'W'
  MONTH = 'M'
  YEAR = 'Y'


class FilterOperation(Enum):
  LT = 'LT'
  LTE = 'LTE'
  GT = 'GT'
  GTE = 'GTE'
  E = 'E'


class Operator(Enum):
  MULT = 'MULT'
  DIV = 'DIV'
  ADD = 'ADD'
  SUB = 'SUB'
  ASSIGN = 'ASSIGN'


class EditService():
  def __init__(self, series_id, data) -> None:
    self.series_id = series_id
    self.data = data
    self._filtered_df = None

    print("[EditService]: Initializing...")
    self._populate_series()

  def _populate_series(self) -> None:
    rows = self.data["dataArray"]
    cols = self.data["components"]

    # Parse date fields
    for i, r in enumerate(rows):
      rows[i][0] = datetime.strptime(
        r[0], "%Y-%m-%d %H:%M:%S")
    self._df = pd.DataFrame(rows, columns=cols)

  def get_dataframe(self):
    if self._filtered_df is None:
      return self._df
    return self._filtered_df

  def get_date_col(self):
    return self.data["components"][0]

  def get_value_col(self):
    return self.data["components"][1]

  ###################
  # Filters
  ###################

  def _has_filter(self, filter: dict[FilterOperation, float], key: FilterOperation) -> bool:
    return key.value in filter and (isinstance(filter[key.value], float) or isinstance(filter[key.value], int))

  def filter(self, filter: dict[FilterOperation, float]) -> None:
    """
    Executes the applied filters and returns the resulting DataFrame
    """

    query = []

    if self._has_filter(filter, FilterOperation.LT):
      query.append(
        f'`{self.get_value_col()}` < {filter[FilterOperation.LT.value]}')

    if self._has_filter(filter, FilterOperation.LTE):
      query.append(
        f'`{self.get_value_col()}` <= {filter[FilterOperation.LTE.value]}')

    if self._has_filter(filter, FilterOperation.GT):
      query.append(
        f'`{self.get_value_col()}` > {filter[FilterOperation.GT.value]}')

    if self._has_filter(filter, FilterOperation.GTE):
      query.append(
        f'`{self.get_value_col()}` >= {filter[FilterOperation.GTE.value]}')

    if self._has_filter(filter, FilterOperation.E):
      query.append(
        f'`{self.get_value_col()}` == {filter[FilterOperation.E.value]}')

    if len(query):
      self._filtered_df = self._df.query(" | ".join(query))
    else:
      self._filtered_df = None

  ###################
  # Gap Analysis
  ###################

  def find_gaps(self, time_value, time_unit: str):
    """
    :return Pandas DataFrame:
    """
    return self.get_dataframe()[self._df[self.get_date_col()].diff() > np.timedelta64(time_value, time_unit)]

  def fill_gap(self, gap, fill, interpolate_values):
    """
    :return Pandas DataFrame:
    """
    gaps_df = self.find_gaps(gap[0], gap[1])
    timegap = np.timedelta64(fill[0], fill[1])
    points = []
    index = []
    added_index = []

    for gap_row in gaps_df.iterrows():
      gap_end_index = gap_row[0]
      gap_start_index = gap_end_index - 1

      gap_start_date = self._df.iloc[gap_start_index][self.get_date_col()]
      gap_end_date = self._df.iloc[gap_end_index][self.get_date_col()]

      start = gap_start_date + timegap
      end = gap_end_date

      # Annotate the points that will fill this gap
      while start < end:
        points.append([start, -9999])
        index.append(gap_start_index)
        start = start + timegap

        if (interpolate_values):
          # Keep an index of the position where the points will end up
          added_index.append(gap_start_index + len(added_index) + 1)

    self.add_points(points, index)

    if (interpolate_values):
      self.interpolate(added_index)

    # Return the list of points that filled the gaps
    return pd.DataFrame(
      points, columns=[self.get_date_col(), self.get_value_col()])

  ######################################
  # Data point operations
  ######################################

  def add_points(self, points, index=None):
    """
    :return Pandas DataFrame:
    """

    # If an index list was provided, insert the points to the DataFrame at the corresponding index.
    # We do this by creating a dictionary of slices where the key is the index to insert at, and the value is an array of points to insert at that index
    # We iterate through the dictionary keys in reverse order, so that we can insert without altering the position of elements before
    if index is not None:
      # This is the most efficient way to insert into a DataFrame for a large dataset.

      # create a dictionary of points to insert at each index
      slices = {}
      for idx, value in enumerate(index):
        if not value in slices:
          slices[value] = []

        slices[value].append(points[idx])

      for s in sorted(slices.items(), reverse=True):
        # Split DataFrame and insert new row.
        idx = s[0] + 1
        val = s[1]
        df1 = self._df.iloc[:idx, :]
        df2 = self._df.iloc[idx:, :]

        points_df = pd.DataFrame(
          val, columns=[self.get_date_col(), self.get_value_col()])
        self._df = pd.concat([df1, points_df, df2]).reset_index(drop=True)

    else:
      # This way of inserting is not as efficient, but performance should be good enough given that the existing data in the DataFrame is pre-sorted.

      # Create a new dataframe with the points
      points_df = pd.DataFrame(
        points, columns=[self.get_date_col(), self.get_value_col()])

      # Concatenate both dataframes. New rows will be at the end.
      self._df = pd.concat([self._df, points_df])

      # Sort and reset index
      self._df = self._df.sort_values(self.get_date_col())
      self._df.reset_index(drop=True, inplace=True)

  def change_values(self, index_list, operator: str, value):

    def operation(x):
      if operator == Operator.MULT.value:
        return x * value
      elif operator == Operator.DIV.value:
        if value == 0:
          print("Error: cannot divide by 0")
          return x
        return x / value
      elif operator == Operator.ADD.value:
        return x + value
      elif operator == Operator.SUB.value:
        return x - value
      elif operator == Operator.ASSIGN.value:
        return value
      else:
        return x

    self._df.loc[index_list, self.get_value_col(
    )] = self._df.loc[index_list, self.get_value_col()].apply(operation)

  def delete_points(self, index_list):
    self._df.drop(index=index_list, inplace=True)
    self._df.reset_index(drop=True, inplace=True)

  def shift_points(self, index_list, time_value, time_unit):
    shift_value = np.timedelta64(time_value, time_unit)
    condition = self._df.index.isin(index_list)

    # Apply the shift
    self._df.loc[condition, self.get_date_col()] = self._df.loc[condition,
                                                                self.get_date_col()] + shift_value

    self._df = self._df.sort_values(self.get_date_col())
    self._df.reset_index(drop=True, inplace=True)

  def interpolate(self, index_list):
    condition = self._df.index.isin(index_list)
    self._df[self.get_value_col()].mask(condition, inplace=True)
    self._df[self.get_value_col()].interpolate(method="linear", inplace=True)

  def drift_correction(self, start, end, gap_width):
    # validate range
    if start >= end:
      print("Start and end index cannot overlap")
      return self._df
    elif end > len(self._df) - 1:
      print("End index out of range")
      return self._df
    elif start < 0:
      print("Start index must be greater than or equal to 0")
      return self._df

    points = self._df.iloc[start:end + 1]
    startdate = points.iloc[0][self.get_date_col()]
    enddate = points.iloc[-1][self.get_date_col()]

    x_l = (enddate - startdate).total_seconds()
    nodv = -9999
    # y_n = y_0 + G(x_i / x_l)

    def f(row):
      if row[self.get_value_col()] != nodv:
        return row[self.get_value_col()] + (gap_width * ((row[self.get_date_col()] - startdate).total_seconds() / x_l))
      else:
        return row[self.get_value_col()]

    self._df.loc[points.index, self.get_value_col()] = points.apply(f, axis=1)

    return self._df
