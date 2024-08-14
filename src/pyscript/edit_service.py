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

    print(sys.version)
    print(pd.__version__)

    print("[EditService]: Initializing...")
    self._populate_series()

  def _populate_series(self) -> None:
    rows = self.data["dataArray"]
    cols = self.data["components"]

    # Parse date fields
    for i, r in enumerate(rows):
      rows[i][0] = datetime.strptime(
        r[0], "%Y-%m-%d %H:%M:%S")  # from tsa_data.csv
    #   rows[i][0] = datetime.strptime(r[0], "%Y-%m-%dT%H:%M:%SZ")  # from data.json
    self._df = pd.DataFrame(rows, columns=cols)

  def get_date_col(self):
    return self.data["components"][0]

  def get_value_col(self):
    return self.data["components"][1]

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

    # def operation(x):
    #   if operator == Operator.MULT.value:
    #     return x * value
    #   elif operator == Operator.DIV.value:
    #     return x / value
    #   elif operator == Operator.ADD.value:
    #     return x + value
    #   elif operator == Operator.SUB.value:
    #     return x - value
    #   elif operator == Operator.ASSIGN.value:
    #     return value
    #   else:
    #     return x

    # self._df.loc[index_list, self.get_value_col()] = self._df.loc[index_list, self.get_value_col()].apply(
    #   operation)

    self._df.loc[index_list, self.get_value_col(
    )] = self._df.loc[index_list, self.get_value_col()] + 1

    # return self._df

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

  # def interpolate(self, start, end):
  #   masked_df = self._df[self.get_value_col()].mask(
  #     self._df.index.isin(self._df.index[start:end]))

  #   masked_df.interpolate(method="linear", inplace=True)
  #   self._df[self.get_value_col()] = masked_df

  def interpolate(self, index_list):
    condition = self._df.index.isin(index_list)
    self._df[self.get_value_col()].mask(condition, inplace=True)
    self._df[self.get_value_col()].interpolate(method="linear", inplace=True)

  # TODO: in progress...
  def drift_correction(self, index_list, gap_width):
    condition = self._df.index.isin(index_list)
    points = self._df.loc[condition, self.get_date_col()]
    startdate = points[self.get_date_col()].loc[0]
    print(startdate)
    pass
    x_l = (points.index[-1] - startdate).total_seconds()
    nodv = -9999
    # y_n = y_0 + G(x_i / x_l)

    def f(row):
      if row[self.get_value_col()] != nodv:
        return row[self.get_value_col()] + (gap_width * ((row.name - startdate).total_seconds() / x_l))
      else:
        return row[self.get_value_col()]

    points[self.get_value_col()] = points.apply(f, axis=1)

    print(points)

    # update_list = [{"value": row[self.get_value_col()], "id": index}
    #                for index, row in tmp_filter_list.iterrows()]

    # ids = tmp_filter_list.index.tolist()
    # self.memDB.update(update_list)

    # self._populate_series()

    # self.filtered_dataframe = self._series_points_df[self._series_points_df.index.isin(
    #   ids)]
