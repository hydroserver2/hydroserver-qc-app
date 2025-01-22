import { Datastream, EnumDictionary } from '@/types'
import { Operator, TimeUnit, usePyStore } from '@/store/py'
import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { storeToRefs } from 'pinia'

import { fetchObservationsParallel } from '@/utils/observationsUtils'

export enum EnumEditOperations {
  ADD_POINTS = 'ADD_POINTS',
  CHANGE_VALUES = 'CHANGE_VALUES',
  DELETE_POINTS = 'DELETE_POINTS',
  DRIFT_CORRECTION = 'DRIFT_CORRECTION',
  INTERPOLATE = 'INTERPOLATE',
  SHIFT_DATETIMES = 'SHIFT_DATETIMES',
  FIND_GAPS = 'FIND_GAPS',
  FILL_GAPS = 'FILL_GAPS',
}

export enum EnumFilterOperations {
  FIND_GAPS = 'FIND_GAPS',
  PERSISTENCE = 'PERSISTENCE',
  RATE_OF_CHANGE = 'RATE_OF_CHANGE',
  VALUE_THRESHOLD = 'VALUE_THRESHOLD',
}

export class ObservationRecord {
  // A JsProxy of the pandas DataFrame
  dataFrame: any
  /** The generated dataset to be used in echarts */
  dataset: { dimensions: string[]; source: { [key: string]: any } } = {
    dimensions: [],
    source: {},
  }
  history: { method: EnumEditOperations; args?: any[]; icon: string }[]
  isLoading: boolean
  ds: Datastream

  constructor(ds: Datastream) {
    this.history = []
    this.ds = ds
    this.isLoading = true
  }

  async loadData(dataArray: any[]) {
    const { instantiateDataFrame } = usePyStore()
    const components = ['date', 'value', 'qualifier']
    this.dataFrame = await instantiateDataFrame(dataArray, components)
    this.isLoading = false
  }

  /**
   * Reloads the DataFrame
   */
  async reload() {
    const { instantiateDataFrame } = usePyStore()
    const { beginDate, endDate } = storeToRefs(useDataVisStore())

    const fetchedData = await fetchObservationsParallel(
      this.ds,
      beginDate.value,
      endDate.value
    )

    const components = ['date', 'value', 'qualifier']

    this.dataFrame = await instantiateDataFrame(fetchedData, components)
    this.history = []
  }

  /**
   * @param index
   * @returns
   */
  async reloadHistory(index: number) {
    const newHistory = this.history.slice(0, index + 1)
    await this.reload()

    await this.dispatch(newHistory.map((h) => [h.method, ...(h.args || [])]))
    return
  }

  /**
   * Remove a history item
   * @param index
   */
  async removeHistoryItem(index: number) {
    const newHistory = [...this.history]
    newHistory.splice(index, 1)
    await this.reload()
    await this.dispatch(newHistory.map((h) => [h.method, ...(h.args || [])]))
    return
  }

  /** This is an expensive operation and should be only executed when necessary */
  generateDataset() {
    console.log('generateDataset')
    const components = [
      'date',
      'value',
      //  'qualifier'
    ]
    this.dataset = {
      dimensions: components, // TODO: no longer needed?
      source: {
        x: (Array.from(this.dataFrame.get_date_column()) as number[]) || [],
        y: (Array.from(this.dataFrame.get_value_column()) as number[]) || [],
        // mode: 'lines+markers',
        // type: 'scatter',
        // qualifier: Array.from(
        //   this.dataFrame.get_qualifier_column()
        // ) as string[][],
      },
    }
  }

  get beginTime() {
    const beginDateTime = this.dataFrame?.get_datetime_at(0)
    return new Date(beginDateTime)
  }

  get endTime() {
    const endDateTime = this.dataFrame?.get_datetime_at(
      this.dataFrame.count() - 1
    )
    return new Date(endDateTime)
  }

  /** Dispatch an operation and log its signature in hisotry */
  async dispatch(
    action: EnumEditOperations | [EnumEditOperations, ...any][],
    ...args: any
  ) {
    const { editHistory } = storeToRefs(useEChartsStore())
    const actions: EnumDictionary<EnumEditOperations, Function> = {
      [EnumEditOperations.ADD_POINTS]: this._addDataPoints,
      [EnumEditOperations.CHANGE_VALUES]: this._changeValues,
      [EnumEditOperations.DELETE_POINTS]: this._deleteDataPoints,
      [EnumEditOperations.DRIFT_CORRECTION]: this._driftCorrection,
      [EnumEditOperations.INTERPOLATE]: this._interpolate,
      [EnumEditOperations.SHIFT_DATETIMES]: this._shift,
      [EnumEditOperations.FIND_GAPS]: this._findGaps,
      [EnumEditOperations.FILL_GAPS]: this._fillGaps,
    }

    // TODO: consolidate with icons in EditDrawer component
    const editIcons: EnumDictionary<EnumEditOperations, string> = {
      [EnumEditOperations.ADD_POINTS]: 'mdi-plus',
      [EnumEditOperations.CHANGE_VALUES]: 'mdi-pencil',
      [EnumEditOperations.DELETE_POINTS]: 'mdi-trash-can',
      [EnumEditOperations.DRIFT_CORRECTION]: 'mdi-chart-sankey',
      [EnumEditOperations.INTERPOLATE]: 'mdi-transit-connection-horizontal',
      [EnumEditOperations.SHIFT_DATETIMES]: 'mdi-calendar',
      [EnumEditOperations.FILL_GAPS]: 'mdi-keyboard-space',
      [EnumEditOperations.FIND_GAPS]: 'mdi-keyboard-space',
    }

    let response = []

    try {
      if (Array.isArray(action)) {
        for (let i = 0; i < action.length; i++) {
          const method = action[i][0]
          const args = action[i].slice(1, action[i].length)
          const res = await actions[method].apply(this, args)
          response.push(res)
          this.history.push({ method, args, icon: editIcons[method] })
        }
        editHistory.value = [...this.history]
      } else {
        response = await actions[action].apply(this, args)
        this.history.push({ method: action, args, icon: editIcons[action] })
        editHistory.value = [...this.history]
      }
    } catch (e) {
      console.log(
        `Failed to execute operation: ${action} with arguments: `,
        args
      )
      console.log(e)
    }

    this.generateDataset()
    return response
  }

  /** Filter operations do not transform the data */
  async dispatchFilter(
    action: EnumFilterOperations | [EnumFilterOperations, ...any][],
    ...args: any
  ) {
    const filters: EnumDictionary<EnumFilterOperations, Function> = {
      [EnumFilterOperations.FIND_GAPS]: this._findGaps,
      [EnumFilterOperations.VALUE_THRESHOLD]: this._valueThreshold,
      [EnumFilterOperations.PERSISTENCE]: this._persistence,
      [EnumFilterOperations.RATE_OF_CHANGE]: this._rateOfChange,
    }
    let response = []

    try {
      if (Array.isArray(action)) {
        for (let i = 0; i < action.length; i++) {
          const method = action[i][0]
          const args = action[i].slice(1, action[i].length)
          const res = await filters[method].apply(this, args)
          response.push(res)
        }
      } else {
        response = await filters[action].apply(this, args)
      }
    } catch (e) {
      console.log(
        `Failed to execute filter operation: ${action} with arguments: `,
        args
      )
      console.log(e)
    }
    return response
  }

  /**
   * @param index An array containing the list of index of values to perform the operations on.
   * @param operator The operator that will be applied
   * @param value The value to use in the operation
   * @returns The modified DataFrame
   */
  private _changeValues(index: number[], operator: Operator, value: number) {
    this.dataFrame.change_values(index, operator, value)
  }

  private _interpolate(index: number[]) {
    this.dataFrame.interpolate(index)
  }

  /**
   * Shifts the selected indexes by a constant
   * @param index The index list of entries to shift
   * @param amount Number of {@link TimeUnit}
   * @param unit {@link TimeUnit}
   * @returns
   */
  private _shift(index: number[], amount: number, unit: TimeUnit) {
    this.dataFrame.shift_points(index, amount, unit)
  }

  /**
   * Find gaps and fill them with placeholder value
   * @param gap Intervals to detect as gaps
   * @param fill Interval used to fill the detected gaps
   * @param interpolateValues If true, the new values will be linearly interpolated
   * @returns
   */
  private _fillGaps(
    gap: [number, TimeUnit],
    fill: [number, TimeUnit],
    interpolateValues: boolean,
    range?: [number, number]
  ) {
    return this.dataFrame.fill_gaps(gap, fill, interpolateValues, range)
  }

  /**
   *
   * @param index The index list of entries to shift
   */
  private _deleteDataPoints(index: number[]) {
    this.dataFrame.delete_data_points(index)
  }

  /**
   *
   * @param start The start index
   * @param end The end index
   * @param value The drift amount
   */
  private _driftCorrection(start: number, end: number, value: number) {
    this.dataFrame.drift_correction(start, end, value)
  }

  /**
   * @param dataPoints
   */
  private _addDataPoints(
    dataPoints: [
      string,
      number,
      Partial<{
        resultQualifiers: string[]
      }>,
    ][]
  ) {
    this.dataFrame.add_points(dataPoints)
  }

  // =======================
  // FILTER OPERATIONS
  // =======================

  /**
   * Filter by applying a set of logical operations
   * @param appliedFilters
   * @returns
   */
  private _valueThreshold(appliedFilters: { [key: string]: number }) {
    return this.dataFrame.set_filter(appliedFilters)
  }

  /**
   * Filter by applying a set of logical operations
   * @param appliedFilters
   * @returns
   */
  private _rateOfChange(comparator: string, value: number) {
    return this.dataFrame.rate_of_change(comparator, value)
  }

  /**
   * Find gaps in the data
   * @param value The time value
   * @param unit The time unit (TimeUnit)
   * @param range If specified, the gaps will be found only within the range
   * @returns
   */
  private _findGaps(value: number, unit: TimeUnit, range?: [number, number]) {
    return this.dataFrame.find_gaps(value, unit, range)
  }

  /**
   * Find points where the values are the same at least x times in a row
   * @param times The number of times in a row that points can be equal
   * @param range If specified, the points will be found only within the range
   * @returns
   */
  private _persistence(times: number, range?: [number, number]) {
    return this.dataFrame.persistence(times, range)
  }
}
