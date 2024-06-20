import pandas as pd
import numpy as np
from datetime import datetime
from enum import Enum

# Tools
# [x] Interpolate
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


class EditService():
  def __init__(self, series_id, data) -> None:
    self.series_id = series_id
    self.data = data

    print("Initializing Edit Service...")
    self._populate_series()

  def _populate_series(self) -> None:
    rows = self.data["value"][0]["dataArray"][0:10]
    # simulate data gaps
    rows.pop(4)
    rows.pop(3)
    rows.pop(1)
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

  def find_gaps(self, time_value, time_unit: TimeUnit):
    """
    :return Pandas DataFrame:
    """
    return self._df[self._df[self.get_date_col()].diff() > np.timedelta64(time_value, time_unit.value)]

  def fill_gap(self, gap, fill):
    """
    :return Pandas DataFrame:
    """
    gaps_df = self.find_gaps(gap[0], gap[1])
    timegap = np.timedelta64(fill[0], fill[1].value)
    points = []

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
        start = start + timegap

    return self.add_points(points)

  def add_points(self, points):
    """
    :return Pandas DataFrame:
    """

    # Create a new dataframe with the points
    points_df = pd.DataFrame(
      points, columns=[self.get_date_col(), self.get_value_col()])

    # Concatenate both dataframes. New rows will be at the end.
    df = pd.concat([self._df, points_df])
    df.reset_index(drop=True, inplace=True)

    return df

  def change_value(self, index_list, operator: Operator, value):
    def operation(x):
      if operator == Operator.MULT:
        return x * value
      elif operator == Operator.DIV:
        return x / value
      elif operator == Operator.ADD:
        return x + value
      elif operator == Operator.SUB:
        return x - value
      else:
        return x

    # TODO: return original
    self._df = self._df[self._df.index.isin(
      index_list)][self.get_value_col()].apply(operation)
    print(self._df)

  def delete_points(self):
    filtered_points = self.get_filtered_points()
    if not filtered_points.empty:
      values = filtered_points.index.tolist()

      self.memDB.delete(values)
      self._populate_series()
      self.filtered_dataframe = None

  def interpolate(self):
    '''
    In [75]: ser = Series(np.sort(np.random.uniform(size=100)))
    # interpolate at new_index
    In [76]: new_index = ser.index | Index([49.25, 49.5, 49.75, 50.25, 50.5, 50.75])
    In [77]: interp_s = ser.reindex(new_index).interpolate(method='pchip')
    '''

    tmp_filter_list = self.get_filtered_points()
    df = self._series_points_df
    issel = df.index.isin(tmp_filter_list.index)

    mdf = df["DataValue"].mask(issel)
    mdf.interpolate(method="time", inplace=True)
    tmp_filter_list["DataValue"] = mdf[issel]
    ids = tmp_filter_list.index.tolist()

    # update_list = [(row["DataValue"], row["ValueID"]) for index, row in tmp_filter_list.iterrows()]
    update_list = [{"value": row["DataValue"], "id": index}
                   for index, row in tmp_filter_list.iterrows()]

    self.memDB.update(update_list)

    self._populate_series()

    self.filtered_dataframe = self._series_points_df[self._series_points_df.index.isin(
      ids)]

  def drift_correction(self, gap_width):

    if self.isOneGroup():
      tmp_filter_list = self.get_filtered_points()
      startdate = tmp_filter_list.index[0]
      x_l = (tmp_filter_list.index[-1] - startdate).total_seconds()
      # nodv= self.memDB.series_service.get_variable_by_id(self.memDB.df["VariableID"][0])
      nodv = self.memDB.series.variable.no_data_value
      # y_n = y_0 + G(x_i / x_l)

      def f(row): return row["DataValue"] + (gap_width * ((row.name -
                                                           startdate).total_seconds() / x_l)) if row["DataValue"] != nodv else row["DataValue"]
      tmp_filter_list["DataValue"] = tmp_filter_list.apply(f, axis=1)

      update_list = [{"value": row["DataValue"], "id": index}
                     for index, row in tmp_filter_list.iterrows()]

      ids = tmp_filter_list.index.tolist()
      self.memDB.update(update_list)

      self._populate_series()

      self.filtered_dataframe = self._series_points_df[self._series_points_df.index.isin(
        ids)]
      return True
    return False

  def isOneGroup(self):

    issel = self._series_points_df.index.isin(self.get_filtered_points().index)

    found_group = False
    count = 0

    for x in issel:
      if x:
        if not found_group:
          found_group = True
          count = count + 1
      else:
        found_group = False

      if count > 1:
        return False
    if count == 1:
      return True
