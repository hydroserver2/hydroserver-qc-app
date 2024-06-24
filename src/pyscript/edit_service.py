import pandas as pd
import numpy as np
from datetime import datetime
from enum import Enum

# Tools
# [ ] Interpolate
# [x] Set a value to a constant
# [x] Change value by applying arithmetic (+, -, *, /)
# [ ] Shift
# [ ] Drift correction (linear)
# [x] Delete values
# [x] Fill values

# Automation
# [x] Gap filling
# [ ] Setting rules based on sensor values - if stage gets below some value, then some sensor is out of the water
# [ ] Water temperature - identifying icing conditions
# [ ] Looking between stations


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

    print("[EditService]: Initializing...")
    self._populate_series()

  def _populate_series(self) -> None:
    rows = self.data["value"][0]["dataArray"][0:10]
    cols = self.data["value"][0]["components"]

    # Parse date fields
    for i, r in enumerate(rows):
      rows[i][0] = datetime.strptime(r[0], "%Y-%m-%dT%H:%M:%SZ")
    self._df = pd.DataFrame(rows, columns=cols)

  def get_date_col(self):
    return self.data["value"][0]["components"][0]

  def get_value_col(self):
    return self.data["value"][0]["components"][1]

  ###################
  # Filters
  ###################

  def _has_filter(self, filter: dict[FilterOperation, float], key: FilterOperation) -> bool:
    return key.value in filter and isinstance(filter[key.value], float)

  def filter(self, filter: dict[FilterOperation, float]) -> None:
    """
    Executes the applied filters and returns the resulting DataFrame
    """

    query = []

    if self._has_filter(filter, FilterOperation.LT):
      query.append(
        f'result < {filter[FilterOperation.LT.value]}')

    if self._has_filter(filter, FilterOperation.LTE):
      query.append(
        f'result <= {filter[FilterOperation.LTE.value]}')

    if self._has_filter(filter, FilterOperation.GT):
      query.append(
        f'result > {filter[FilterOperation.GT.value]}')

    if self._has_filter(filter, FilterOperation.GTE):
      query.append(
        f'result >= {filter[FilterOperation.GTE.value]}')

    if self._has_filter(filter, FilterOperation.E):
      query.append(
        f'result == {filter[FilterOperation.E.value]}')

    if len(query):
      return self._df.query(" & ".join(query))
    else:
      return self._df

  ###################
  # Gap Analysis
  ###################

  def find_gaps(self, time_value, time_unit: str):
    """
    :return Pandas DataFrame:
    """
    return self._df[self._df[self.get_date_col()].diff() > np.timedelta64(time_value, time_unit)]

  def fill_gap(self, gap, fill):
    """
    :return Pandas DataFrame:
    """
    gaps_df = self.find_gaps(gap[0], gap[1])
    timegap = np.timedelta64(fill[0], fill[1])
    points = []
    index = []

    for gap in gaps_df.iterrows():
      gap_end_index = gap[0]
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

    self.add_points(points, index)

    # Return the list of points that filled the gaps
    return pd.DataFrame(
      points, columns=[self.get_date_col(), self.get_value_col()])

  ###################
  # Data point operations
  ###################

  def add_points(self, points, index=None):
    """
    :return Pandas DataFrame:
    """

    # If an index list was provided, insert the points to the DataFrame at the corresponding index.
    # We do this by creating a dictionary of slices where the key is the index to insert at, and the value is an array of points to insert at that index
    # We iterate through the dictionary keys in reverse order, so that we can insert without altering the position of elements before
    if index is not None:
      # This is the most efficient way to insert into a DataFrame for a large dataset.

      points = [x for _, x in sorted(zip(index, points))]

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
        return x / value
      elif operator == Operator.ADD.value:
        return x + value
      elif operator == Operator.SUB.value:
        return x - value
      elif operator == Operator.ASSIGN.value:
        return value
      else:
        return x

    self._df.loc[self._df.index.isin(
        index_list), self.get_value_col()] = self._df.loc[self._df.index.isin(
            index_list), self.get_value_col()].apply(operation)

    return self._df

  def delete_points(self, index_list):
    self._df.drop(index=index_list, inplace=True)
    self._df.reset_index(drop=True, inplace=True)
