import { Datastream, EnumDictionary } from '@/types'
import { FilterOperation, Operator, TimeUnit, usePyStore } from '@/store/py'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'

import { useObservationStore } from '@/store/observations'

import {
  fetchObservationsParallel,
  fetchObservationsSync,
} from '@/utils/observationsUtils'
import { usePlotlyStore } from '@/store/plotly'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'

export enum EnumEditOperations {
  ADD_POINTS = 'ADD_POINTS',
  CHANGE_VALUES = 'CHANGE_VALUES',
  DELETE_POINTS = 'DELETE_POINTS',
  DRIFT_CORRECTION = 'DRIFT_CORRECTION',
  INTERPOLATE = 'INTERPOLATE',
  SHIFT_DATETIMES = 'SHIFT_DATETIMES',
  FILL_GAPS = 'FILL_GAPS',
}

export enum EnumFilterOperations {
  FIND_GAPS = 'FIND_GAPS',
  PERSISTENCE = 'PERSISTENCE',
  RATE_OF_CHANGE = 'RATE_OF_CHANGE',
  VALUE_THRESHOLD = 'VALUE_THRESHOLD',
}

const SECOND = 1
const MINUTE = SECOND * 60
const HOUR = MINUTE * 60
const DAY = HOUR * 24
const WEEK = DAY * 7
const MONTH = HOUR * 30
const YEAR = DAY * 365

const timeUnitMultipliers: EnumDictionary<TimeUnit, number> = {
  [TimeUnit.SECOND]: SECOND,
  [TimeUnit.MINUTE]: MINUTE,
  [TimeUnit.HOUR]: HOUR,
  [TimeUnit.DAY]: DAY,
  [TimeUnit.WEEK]: WEEK,
  [TimeUnit.MONTH]: MONTH,
  [TimeUnit.YEAR]: YEAR,
}

const components = ['date', 'value', 'qualifier']

export class ObservationRecord {
  // A JsProxy of the pandas DataFrame
  dataArray: [string, number, any][] = [] // Source of truth
  /** The generated dataset to be used for plotting */
  dataset: { dimensions: string[]; source: { [key: string]: number[] } } = {
    dimensions: components,
    source: {
      x: [],
      y: [],
    },
  }
  history: { method: EnumEditOperations; args?: any[]; icon: string }[]
  isLoading: boolean
  ds: Datastream

  constructor(ds: Datastream) {
    this.history = []
    this.ds = ds
    this.isLoading = true
  }

  loadData(dataArray: [string, number, any][]) {
    this.dataArray = dataArray

    // Clear the array
    this.dataset.source.x.length = 0
    this.dataset.source.y.length = 0

    this.dataArray.forEach((row, _index) => {
      if (!isNaN(row[1])) {
        this.dataset.source.x.push(Date.parse(row[0]))
        this.dataset.source.y.push(row[1])
      }
    })

    this.isLoading = false
  }

  /**
   * Reloads the dataset
   */
  async reload() {
    console.log('reload')
    const { beginDate, endDate } = storeToRefs(useDataVisStore())
    const { fetchObservationsInRange } = useObservationStore()

    await fetchObservationsInRange(this.ds, beginDate.value, endDate.value)

    this.loadData(this.dataArray)

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
  }

  get beginTime() {
    return new Date(Date.parse(this.dataArray[0][0]))
  }

  get endTime() {
    return new Date(Date.parse(this.dataArray[this.dataArray.length - 1][0]))
  }

  /** Dispatch an operation and log its signature in hisotry */
  async dispatch(
    action: EnumEditOperations | [EnumEditOperations, ...any][],
    ...args: any
  ) {
    const { editHistory } = storeToRefs(usePlotlyStore())
    const actions: EnumDictionary<EnumEditOperations, Function> = {
      [EnumEditOperations.ADD_POINTS]: this._addDataPoints,
      [EnumEditOperations.CHANGE_VALUES]: this._changeValues,
      [EnumEditOperations.DELETE_POINTS]: this._deleteDataPoints,
      [EnumEditOperations.DRIFT_CORRECTION]: this._driftCorrection,
      [EnumEditOperations.INTERPOLATE]: this._interpolate,
      [EnumEditOperations.SHIFT_DATETIMES]: this._shift,
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

    // TODO: trigger graph redraw
    // const { plotlyRef } = storeToRefs(usePlotlyStore())
    // await Plotly.update(plotlyRef.value, {}, {}, 0)
    return response
  }

  /** Filter operations do not transform the data and are not logged in history */
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
    const operation = (x: number) => {
      switch (operator) {
        case Operator.ADD:
          return x + value
        case Operator.ASSIGN:
          return value
        case Operator.DIV:
          return x / value
        case Operator.MULT:
          return x * value
        case Operator.SUB:
          return x - value
        default:
          return x
      }
    }

    index.forEach((index: number) => {
      this.dataset.source.y[index] = operation(this.dataset.source.y[index])
    })
  }

  private _interpolate(index: number[]) {
    console.log('_interpolate')
    const groups = this._getConsecutiveGroups(index)

    groups.forEach((g) => {
      const start = g[0]
      const end = g[g.length - 1]

      let lowerIndex = Math.max(0, start - 1)
      let upperIndex = Math.min(this.dataset.source.y.length - 1, end + 1)

      const xData = this.dataset.source.x
      const yData = this.dataset.source.y
      for (let i = 0; i < g.length; i++) {
        this.dataset.source.y[g[i]] = this._interpolateLinear(
          xData[g[i]],
          xData[lowerIndex],
          yData[lowerIndex],
          xData[upperIndex],
          yData[upperIndex]
        )
      }
    })
  }

  /** Interpolate existing values in the data source */
  private _interpolateLinear(
    datetime: number,
    lowerDatetime: number,
    lowerValue: number,
    upperDatetime: number,
    upperValue: number
  ) {
    const interpolatedValue =
      lowerValue +
      ((datetime - lowerDatetime) * (upperValue - lowerValue)) /
        (upperDatetime - lowerDatetime)

    return interpolatedValue
  }

  /**
   * Shifts the selected indexes by a constant
   * @param index The index list of entries to shift
   * @param amount Number of {@link TimeUnit}
   * @param unit {@link TimeUnit}
   * @returns
   */
  private _shift(index: number[], amount: number, unit: TimeUnit) {
    // this.dataFrame.shift_points(index, amount, unit)
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
    const gaps = this._findGaps(gap[0], gap[1], range)
    const dataX = this.dataset.source.x
    const dataY = this.dataset.source.y
    const collection: any = {}

    for (let i = 0; i < gaps.length; i++) {
      const currentGap = gaps[i]
      const left = dataX[currentGap[0]]
      const right = dataX[currentGap[1]]
      const leftDatetime = left
      const rightDatetime = right

      const fillPoints = []
      const fillDelta = fill[0] * timeUnitMultipliers[fill[1]] * 1000
      let nextFillDatetime = leftDatetime + fillDelta

      while (nextFillDatetime < rightDatetime) {
        const val: number = interpolateValues
          ? this._interpolateLinear(
              nextFillDatetime,
              dataX[currentGap[0]],
              dataY[currentGap[0]],
              dataX[currentGap[1]],
              dataY[currentGap[1]]
            )
          : -9999

        fillPoints.push([nextFillDatetime, val])
        nextFillDatetime += fillDelta
      }

      collection[currentGap[0]] = fillPoints
    }

    const keys = Object.keys(collection)

    // insert in reverse order so we don't alter the array indexes
    for (let i = keys.length - 1; i >= 0; i--) {
      const insertIndex = +keys[i]

      this.dataset.source.x.splice(
        insertIndex + 1,
        0,
        ...collection[keys[i]].map((a) => a[0])
      )
      this.dataset.source.y.splice(
        insertIndex + 1,
        0,
        ...collection[keys[i]].map((a) => a[1])
      )
    }
  }

  /**
   *
   * @param index The index list of entries to shift
   */
  private _deleteDataPoints(index: number[]) {
    const groups = this._getConsecutiveGroups(index)

    for (let i = groups.length - 1; i >= 0; i--) {
      const group = groups[i]
      const start = group[0]

      this.dataset.source.x.splice(start, group.length)
      this.dataset.source.y.splice(start, group.length)
    }
  }

  /**
   *
   * @param start The start index
   * @param end The end index
   * @param value The drift amount
   */
  private _driftCorrection(start: number, end: number, value: number) {
    // this.dataFrame.drift_correction(start, end, value)
  }

  /** Traverses the index array and returns groups of consecutive values.
   * i.e.: `[0, 1, 3, 4, 6] => [[0, 1], [3, 4], [6]]`
   * Assumes the input array is sorted.
   * @param index: the index array (sorted)
   */
  private _getConsecutiveGroups(index: number[]): number[][] {
    const groups: number[][] = [[]]

    // Form groups of consecutive points to delete in order to minimize the number of splice operations
    index.reduce((acc: number[][], curr: number) => {
      const target: number[] = acc[acc.length - 1]

      if (!target.length || curr == target[target.length - 1] + 1) {
        target.push(curr)
      } else {
        acc.push([curr])
      }

      return acc
    }, groups)

    return groups
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
    // this.dataFrame.add_points(dataPoints)
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
    const selection: number[] = []

    this.dataset.source.y.forEach((value: number, index: number) => {
      if (
        appliedFilters.hasOwnProperty(FilterOperation.E) &&
        value == appliedFilters[FilterOperation.E]
      ) {
        selection.push(index)
      } else if (
        appliedFilters.hasOwnProperty(FilterOperation.GT) &&
        value > appliedFilters[FilterOperation.GT]
      ) {
        selection.push(index)
      } else if (
        appliedFilters.hasOwnProperty(FilterOperation.GTE) ||
        value >= appliedFilters[FilterOperation.GTE]
      ) {
        selection.push(index)
      } else if (
        appliedFilters.hasOwnProperty(FilterOperation.LT) &&
        value < appliedFilters[FilterOperation.LT]
      ) {
        selection.push(index)
      } else if (
        appliedFilters.hasOwnProperty(FilterOperation.LTE) &&
        value <= appliedFilters[FilterOperation.LTE]
      ) {
        selection.push(index)
      }
    })

    return selection
  }

  /**
   * Filter by applying a set of logical operations
   * @param appliedFilters
   * @returns
   */
  private _rateOfChange(comparator: string, value: number) {
    // return this.dataFrame.rate_of_change(comparator, value)
  }

  /**
   * Find gaps in the data
   * @param value The time value
   * @param unit The time unit (TimeUnit)
   * @param range If specified, the gaps will be found only within the range
   * @returns
   */
  private _findGaps(
    value: number,
    unit: TimeUnit,
    range?: [number, number]
  ): [number, number][] {
    const selection: [number, number][] = []
    const dataX = this.dataset.source.x
    let start = 0
    let end = dataX.length

    if (range?.[0] && range?.[1]) {
      start = range[0]
      end = range[1]
    }

    let prevDatetime = dataX[start]

    for (let i = start + 1; i < end; i++) {
      const curr = dataX[i]
      const delta = curr - prevDatetime // milliseconds

      if (delta > value * timeUnitMultipliers[unit] * 1000) {
        selection.push([i - 1, i])
      }
      prevDatetime = curr
    }

    return selection
  }

  /**
   * Find points where the values are the same at least x times in a row
   * @param times The number of times in a row that points can be equal
   * @param range If specified, the points will be found only within the range
   * @returns
   */
  private _persistence(times: number, range?: [number, number]) {
    let selection: number[] = []
    let dataY = this.dataset.source.y
    let start = 0
    let end = dataY.length
    if (range?.[0] && range?.[1]) {
      start = range[0]
      end = range[1]
    }

    let prev = dataY[start]
    let stack = []

    for (let i = start + 1; i < end; i++) {
      const curr = dataY[i]
      if (curr != prev || i === end) {
        if (stack.length >= times) {
          selection = [...selection, ...stack]
        }
        stack = []
      } else {
        stack.push(i)
      }
    }

    return selection
  }
}
