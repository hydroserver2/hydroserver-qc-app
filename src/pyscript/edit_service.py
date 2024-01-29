import pandas as pd
import numpy as np
from datetime import datetime
from enum import Enum


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


class EditService():
  def __init__(self, series_id, data) -> None:
    self.series_id = series_id
    self.data = data
    print("Initializing Edit Service...")
    self._populate_series()
    self._filtered_df = None
    self.applied_filters: dict[FilterOperation, float] = {}

  def _populate_series(self) -> None:
    rows = self.data["value"][0]["dataArray"][0:10]
    # simulate gaps
    rows.pop(4)
    rows.pop(7)
    cols = self.data["value"][0]["components"]

    # Parse date fields
    for i, r in enumerate(rows):
      rows[i][0] = datetime.strptime(r[0], "%Y-%m-%dT%H:%M:%SZ")
    self._df = pd.DataFrame(rows, columns=cols)

  ###################
  # Filters
  ###################

  def set_filter(self, filter: dict[FilterOperation, float]) -> None:
    self.applied_filters = filter
    self._exec_filter()

  def _has_filter(self, key: FilterOperation) -> bool:
    return key.value in self.applied_filters and isinstance(self.applied_filters[key.value], float)

  def _exec_filter(self) -> None:
    """
    Executes the applied filer
    """

    query = []

    if self._has_filter(FilterOperation.LT):
      query.append(
        f'result < {self.applied_filters[FilterOperation.LT.value]}')

    if self._has_filter(FilterOperation.LTE):
      query.append(
        f'result <= {self.applied_filters[FilterOperation.LTE.value]}')

    if self._has_filter(FilterOperation.GT):
      query.append(
        f'result > {self.applied_filters[FilterOperation.GT.value]}')

    if self._has_filter(FilterOperation.GTE):
      query.append(
        f'result >= {self.applied_filters[FilterOperation.GTE.value]}')

    if self._has_filter(FilterOperation.E):
      query.append(
        f'result == {self.applied_filters[FilterOperation.E.value]}')

    if len(query):
      self.filtered_df = self._df.query(" & ".join(query))
    else:
      self.filtered_df = self._df

  def find_gaps(self, value, time_unit: TimeUnit):
    date_col = self.data["value"][0]["components"][0]

    threshold = np.timedelta64(value, time_unit.value)
    # filter rows that pass condition
    return self._df[self._df[date_col].diff() > threshold]
