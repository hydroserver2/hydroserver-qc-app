import { Datastream, EnumDictionary } from '@/types'
import {
  FilterOperation,
  FilterOperationFn,
  Operator,
  RateOfChangeComparator,
  RateOfChangeOperation,
  TimeUnit,
} from '@/store/userInterface'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'

import { useObservationStore } from '@/store/observations'
import { usePlotlyStore } from '@/store/plotly'
import { shiftDatetime, timeUnitMultipliers } from '../formatDate'

/**
// @see https://github.com/javascriptdata/danfojs/issues/594
 */
// @ts-ignore Only way to comsume Danfo.js in a Vite project.
import { DataFrame } from 'danfojs/dist/danfojs-browser/src'
// @ts-ignore To use as type
import { DataFrame as DataFrameType } from 'danfojs'

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

const components = [
  'date',
  'value',
  // 'qualifier'
]

// TODO: try these operations with https://danfo.jsdata.org/api-reference/dataframe/danfo.dataframe.query
// Assess if they are faster
/**
 * @deprecated Danfo.js operations not significantly faster
 */
export class ObservationRecord {
  // A JsProxy of the pandas DataFrame
  /** The generated dataset to be used for plotting */
  dataset!: DataFrameType
  history: { method: EnumEditOperations; args?: any[]; icon: string }[]
  isLoading: boolean
  ds: Datastream

  constructor(ds: Datastream) {
    const { observationsRaw } = storeToRefs(useObservationStore())

    this.history = []
    this.ds = ds
    this.isLoading = true

    this.loadData(observationsRaw.value[this.ds.id])
  }

  loadData(dataArrays: { datetimes: number[]; dataValues: number[] }) {
    if (!dataArrays) {
      return
    }

    this.dataset = new DataFrame(dataArrays)

    this.isLoading = false
  }

  /**
   * Reloads the dataset
   */
  async reload() {
    const { beginDate, endDate } = storeToRefs(useDataVisStore())
    const { fetchObservationsInRange } = useObservationStore()
    const { observationsRaw } = storeToRefs(useObservationStore())

    await fetchObservationsInRange(this.ds, beginDate.value, endDate.value)

    this.history.length = 0
    this.loadData(observationsRaw.value[this.ds.id])
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

  get beginTime(): Date | null {
    if (!this.dataset.datetimes.values.length) {
      return null
    }
    return new Date(this.dataset.datetimes.values[0])
  }

  get endTime(): Date | null {
    if (!this.dataset.datetimes.values.length) {
      return null
    }
    return new Date(
      this.dataset.datetimes.values[this.dataset.datetimes.values.length - 1]
    )
  }

  get dataX() {
    return this.dataset.datetimes.values
  }

  get dataY() {
    return this.dataset['dataValues']['values']
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
          const actionArgs = action[i].slice(1, action[i].length)
          const res = await actions[method].apply(this, actionArgs)

          response.push(res)
          this.history.push({
            method,
            args: actionArgs,
            icon: editIcons[method],
          })
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
    const groups = this._getConsecutiveGroups(index)

    groups.forEach((g) => {
      const start = g[0]
      const end = g[g.length - 1]

      let lowerIndex = Math.max(0, start - 1)
      let upperIndex = Math.min(this.dataset.source.y.length - 1, end + 1)

      const xData = this.dataset.datetimes.values
      const yData = this.dataset['dataValues']['values']
      for (let i = 0; i < g.length; i++) {
        this.dataset.source.y[g[i]] = this._interpolateLinear(
          xData[g[i]],
          xData[lowerIndex],
          // @ts-ignore
          yData[lowerIndex],
          xData[upperIndex],
          // @ts-ignore
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
   * Shifts the selected indexes by specified amount of units. Elements are reinserted according to their datetime.
   * @param index The index of the elements to shift
   * @param amount Number of {@link TimeUnit}
   * @param unit {@link TimeUnit}
   * @returns
   */
  private _shift(index: number[], amount: number, unit: TimeUnit) {
    const xData = this.dataset.datetimes.values
    const yData = this.dataset['dataValues']['values']

    // Collection that will be re-added using `_addDataPoints`
    const collection: [number, number][] = index.map((i) => [
      shiftDatetime(xData[i], amount, unit),
      // @ts-ignore
      yData[i],
    ])
    this._deleteDataPoints(index)
    this._addDataPoints(collection)
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
    const dataX = this.dataset.datetimes.values
    const dataY = this.dataset['dataValues']['values']

    for (let i = gaps.length - 1; i >= 0; i--) {
      const currentGap = gaps[i]
      const leftDatetime = dataX[currentGap[0]]
      const rightDatetime = dataX[currentGap[1]]
      const fillX: number[] = []
      const fillY: number[] = []

      // TODO: number of seconds in a year or month is not constant
      // Use setMonth and setFullYear instead
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

        fillX.push(nextFillDatetime)
        fillY.push(val)
        nextFillDatetime += fillDelta
      }

      dataX.splice(currentGap[0] + 1, 0, ...fillX)
      dataY.splice(currentGap[0] + 1, 0, ...fillY)
    }
  }

  /**
   *
   * @param index Array of indexes to be deleted
   */
  private async _deleteDataPoints(index: number[]) {
    console.log(new Date(Date.now()))
    await this.dataset.drop({
      index,
      inplace: true,
    })
    await this.dataset.resetIndex({ inplace: true })
    console.log(new Date(Date.now()))
  }

  /**
   * TODO: JavaScript engine has an argument limit of 65536 (actually 60k in practice)
    Which means our splice method can only insert back elements by 60k at a time
    @see https://bugs.webkit.org/show_bug.cgi?id=80797
    @see https://stackoverflow.com/a/22747272
    @see https://tc39.es/ecma262/multipage/indexed-collections.html#sec-array.prototype.splice
    TODO: this is still too slow

   * @param target 
   * @param elements 
   * @param start 
   */
  private _pagedInsert(target: any[], elements: any[], start: number) {
    const ARG_LIMIT = 60000

    while (elements.length > ARG_LIMIT) {
      const chunk = elements.splice(0, ARG_LIMIT)
      target.splice(start, 0, ...chunk)
      start += chunk.length
    }
    // Append remainder
    target.splice(start, 0, ...elements)
  }

  /**
   *
   * @param start The start index
   * @param end The end index
   * @param value The drift amount
   */
  private _driftCorrection(start: number, end: number, value: number) {
    const xData = this.dataset.source.x
    const yData = this.dataset.source.y

    const startDatetime = xData[start]
    const endDatetime = xData[end]
    const extent = endDatetime - startDatetime

    for (let i = start; i < end; i++) {
      // y_n = y_0 + G(x_i / extent)
      this.dataset.source.y[i] =
        yData[i] + value * ((xData[i] - startDatetime) / extent)
    }
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
   * Adds data points. Their insert index is determined using `_findLowerBound` in the x-axis.
   * @param dataPoints
   */
  private _addDataPoints(dataPoints: [number, number][]) {
    dataPoints.sort((a, b) => b[0] - a[0])

    let lowerBound = this._findLowerBound(dataPoints[0][0])
    const toInsertX: number[] = []
    const toInsertY: number[] = []

    /** Iterate through the points to add and find their insert index. Minimize the number of splice operations because they are costly. */
    for (let i = 0; i < dataPoints.length; i++) {
      const d = dataPoints[i]
      if (this.dataset.source.x[lowerBound] > d[0]) {
        // lowerBound crossed, insert the collected items
        this.dataset.source.x.splice(lowerBound + 1, 0, ...toInsertX)
        this.dataset.source.y.splice(lowerBound + 1, 0, ...toInsertY)
        toInsertX.length = 0
        toInsertY.length = 0
        lowerBound = this._findLowerBound(d[0])
      }

      toInsertX.splice(0, 0, d[0])
      toInsertY.splice(0, 0, d[1])
    }
    // Leftovers in last iteration
    this.dataset.source.x.splice(lowerBound + 1, 0, ...toInsertX)
    this.dataset.source.y.splice(lowerBound + 1, 0, ...toInsertY)
  }

  private _findLowerBound(target: number) {
    const xData = this.dataset.source.x
    let low = 0
    let high = xData.length
    while (low < high) {
      const mid = (low + high) >>> 1
      if (xData[mid] < target) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    return low
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
        Object.keys(appliedFilters).some((key) => {
          return FilterOperationFn[key as FilterOperation]?.(
            value,
            appliedFilters[key]
          )
        })
      ) {
        selection.push(index)
      }
    })

    return selection
  }

  /**
   *
   * @param comparator
   * @param value
   * @returns
   */
  private _rateOfChange(comparator: string, value: number) {
    const selection: number[] = []
    const dataY = this.dataset.source.y

    for (let i = 0 + 1; i < dataY.length; i++) {
      const prev = dataY[i - 1]
      const curr = dataY[i]
      const rate = (curr - prev) / Math.abs(prev)

      if (
        RateOfChangeComparator[comparator as RateOfChangeOperation]?.(
          rate,
          value
        )
      ) {
        selection.push(i)
      }
    }
    return selection
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
