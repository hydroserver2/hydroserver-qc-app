import pandas as pd

class EditService():
  def __init__(self, series_id, data) -> None:
    self.series_id = series_id
    self.data = data
    print("Initializing Edit Service...")
    self._populate_series()

  def _populate_series(self):
    rows = self.data["value"][0]["dataArray"]
    cols = self.data["value"][0]["components"]
    self._series_points_df = pd.DataFrame(rows, columns = cols)
    print(self._series_points_df.head(10))