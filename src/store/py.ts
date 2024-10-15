import { defineStore, storeToRefs } from 'pinia'
import { _Window } from '@/types'
import { Ref, ref } from 'vue'
import { Subject } from 'rxjs'
import { useEChartsStore } from '@/store/echarts'
import { useDataVisStore } from './dataVisualization'

const _window = window as _Window

export enum TimeUnit {
  SECOND = 's',
  MINUTE = 'm',
  HOUR = 'h',
  DAY = 'D',
  WEEK = 'W',
  MONTH = 'M',
  YEAR = 'Y',
}

export enum Operator {
  ADD = 'ADD',
  SUB = 'SUB',
  MULT = 'MULT',
  DIV = 'DIV',
  ASSIGN = 'ASSIGN',
}

export enum FilterOperation {
  LT = 'LT',
  LTE = 'LTE',
  GT = 'GT',
  GTE = 'GTE',
  E = 'E',
  START = 'START',
  END = 'END',
}

export enum InterpolationMethods {
  LINEAR = 'LINEAR',
}

export enum DriftCorrectionMethods {
  LINEAR = 'LINEAR',
}

export const usePyStore = defineStore('py', () => {
  const interpreter: Ref<any> = ref(null)
  const $initialized = new Subject<boolean>()
  const startEl = document.getElementById('start') // Used to detect when PyScript has finished initializing
  const isLoading = ref(false) // TODO: make use
  const { graphSeriesArray, selectedSeriesIndex, brushSelections } =
    storeToRefs(useEChartsStore())
  const { selectedData } = storeToRefs(useDataVisStore())
  const { updateVisualization } = useEChartsStore()

  // Change Values
  const operators = [...Object.keys(Operator)]
  const selectedOperator = ref(0)
  const operationValue = ref(0.1)

  // GAP ANALYSYS
  const interpolateValues = ref(false)
  const selectedInterpolationMethod = ref(InterpolationMethods.LINEAR)
  const gapUnits = [...Object.keys(TimeUnit)]
  const selectedGapUnit = ref(gapUnits[1])
  const gapAmount = ref(30)

  // DRIFT CORRECTION
  const selectedDriftCorrectionMethod = ref(DriftCorrectionMethods.LINEAR)
  const driftGapWidth = ref(1)

  // SHIFT VALUES
  const shiftUnits = [...Object.keys(TimeUnit)]
  const selectedShiftUnit = ref(shiftUnits[1])
  const shiftAmount = ref(15)

  // /**
  //  * Delete rows from the DataFrame
  //  * @param index An array containing the list of index of values to perform the operations on.
  //  * @returns
  //  */
  // const deleteDataPoints = (index: number[]) => {
  //   return interpreter.value.globals.get('delete_data_points')?.(index)
  // }

  /** Instantiates a new Pandas DataFrame and returns the instance */
  const instantiateDataFrame = (data: any) => {
    const wrapperClass = interpreter.value.globals.get('edit_service_wrapper')
    const instance = wrapperClass(data)
    return instance
  }

  // ============= EDIT DATA =================

  const changeValues = () => {
    if (!selectedData.value.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    // TODO
    const index = selectedData.value.map(
      (point: { date: Date; value: number; index: number }) =>
        df.get_index_at(point.index)
    )
    isLoading.value = true

    setTimeout(() => {
      df.change_values(
        index,
        Operator[operators[selectedOperator.value] as Operator],
        +operationValue.value
      )
      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    })
  }

  const interpolate = () => {
    if (!selectedData.value.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    const index = selectedData.value.map(
      (point: { date: Date; value: number; index: number }) =>
        df.get_index_at(point.index)
    )

    isLoading.value = true
    setTimeout(() => {
      // TODO: value error when interpolating values lesser than 1
      df.interpolate(index)
      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    })
  }

  const shift = () => {
    if (!selectedData.value.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    const index = selectedData.value.map(
      (point: { date: Date; value: number; index: number }) =>
        df.get_index_at(point.index)
    )
    isLoading.value = true

    setTimeout(() => {
      df.shift_points(
        index,
        shiftAmount.value,
        // @ts-ignore
        TimeUnit[selectedShiftUnit.value]
      )
      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    })
  }

  const deleteDataPoints = () => {
    if (!selectedData.value.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    const index = selectedData.value.map(
      (point: { date: Date; value: number; index: number }) =>
        df.get_index_at(point.index)
    )
    isLoading.value = true

    setTimeout(() => {
      df.delete_data_points(index)
      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    })
  }

  const driftCorrection = () => {
    if (!selectedData.value.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    const index = selectedData.value.map(
      (point: { date: Date; value: number; index: number }) =>
        df.get_index_at(point.index)
    )
    isLoading.value = true

    setTimeout(() => {
      const groups: number[][] = [[]]
      const sorted = index.sort((a, b) => a - b)

      sorted.reduce((acc: number[][], curr: number) => {
        const target: number[] = acc[acc.length - 1]

        if (!target.length || curr == target[target.length - 1] + 1) {
          target.push(curr)
        } else {
          acc.push([curr])
        }

        return acc
      }, groups)

      groups.forEach((g) => {
        const start = g[0]
        const end = g[g.length - 1]
        df.drift_correction(start, end, +driftGapWidth.value)
      })

      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    }, 0)
  }

  const addDataPoints = (
    dataPoints: [
      datetime: string,
      value: number,
      qualifier: Partial<{ resultQualifiers: string[] }>
    ][]
  ) => {
    if (!dataPoints || !dataPoints.length) {
      return
    }

    const df = graphSeriesArray.value[selectedSeriesIndex.value].data.dataFrame

    // Convert input localized datetimes to UTC
    const transformedDataPoints = dataPoints.map((point) => {
      const matches = point[0].match(
        /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})$/
      )
      if (matches) {
        const year = parseInt(matches[1])
        const month = parseInt(matches[2]) - 1
        const day = parseInt(matches[3])
        const hour = parseInt(matches[4])
        const minute = parseInt(matches[5])
        const second = parseInt(matches[6])
        const date = new Date(year, month, day, hour, minute, second)
        return [date.toISOString().substring(0, 19) + 'Z', point[1], point[2]]
      } else {
        throw new Error('Invalid date format.')
      }
    })

    setTimeout(() => {
      df.add_points(transformedDataPoints)
      brushSelections.value = []
      selectedData.value = []
      updateVisualization()
      isLoading.value = false
    })
  }

  // /**
  //  * Find gaps in the data
  //  * @param value The time value
  //  * @param unit The time unit (TimeUnit)
  //  * @returns
  //  */
  // const findGaps = (value: number, unit: TimeUnit) => {
  //   return interpreter.value.globals.get('find_gaps')?.(value, unit)
  // }

  // /**
  //  * Find gaps and fill them with placeholder value
  //  * @param gap Intervals to detect as gaps
  //  * @param fill Interval used to fill the detected gaps
  //  * @param interpolateValues If true, the new values will be linearly interpolated
  //  * @returns
  //  */
  // const fillGaps = (
  //   gap: [number, TimeUnit],
  //   fill: [number, TimeUnit],
  //   interpolateValues: boolean
  // ) => {
  //   return interpreter.value.globals.get('fill_gaps')?.(
  //     gap,
  //     fill,
  //     interpolateValues
  //   )
  // }

  // /**
  //  * Shifts the selected indexes by a constant
  //  * @param index The index list of entries to shift
  //  * @param timeUnit {@link TimeUnit}
  //  * @param timeValue Number of {@link TimeUnit}
  //  * @returns
  //  */
  // const shift = (index: number[], timeValue: number, timeUnit: TimeUnit) => {
  //   interpreter.value.globals.get('shift_points')?.(index, timeValue, timeUnit)
  // }

  // /**
  //  * Retrieve the DataFrame object from EditService as json
  //  * @returns
  //  */
  // const getDataFrame = () => {
  //   return interpreter.value.globals.get('get_data_frame')?.()
  // }

  // /**
  //  * @param filter A dictionary of key(FilterOperation) - value pairs
  //  * @returns
  //  */
  // const setFilter = (filter: { [key: string]: number }) => {
  //   return interpreter.value.globals.get('set_filter')?.(JSON.stringify(filter))
  // }

  // /**
  //  * @param points An array [date, value, qualifierCode] representing the points to add.
  //  * @returns
  //  */
  // const addPoints = (points: [string, number, any][]) => {
  //   return interpreter.value.globals.get('add_points')?.(points)
  // }

  // /**
  //  * @param points An array [date, value, qualifierCode] representing the points to add.
  //  * @returns The length of the current DataFrame
  //  */
  // const count = () => {
  //   return interpreter.value.globals.get('count')?.()
  // }

  /**
   *
  //  * @param index An array containing the list of index of values to perform the operations on.
  //  * @param operator The operator that will be applied
  //  * @param value The value to use in the operation
  //  * @returns The modified DataFrame
  //  */
  // const changeValues = (index: number[], operator: Operator, value: number) => {
  //   return interpreter.value.globals.get('change_values')?.(
  //     index,
  //     operator,
  //     value
  //   )
  // }

  // /**
  //  * @param range
  //  */
  // const interpolate = (index: number[]) => {
  //   return interpreter.value.globals.get('interpolate')?.(index)
  // }

  // /**
  //  * @param index
  //  * @returns The value for the row at the given index
  //  */
  // const getValueAt = (index: number) => {
  //   return interpreter.value.globals.get('get_value_at')?.(index)
  // }

  // /**
  //  * @param index
  //  * @returns The value for the row at the given index
  //  */
  // const getIndexAt = (index: number) => {
  //   return interpreter.value.globals.get('get_index_at')?.(index)
  // }

  // /**
  //  * @param index
  //  * @returns The datetime for the row at the given index
  //  */
  // const getDatetimeAt = (index: number) => {
  //   return interpreter.value.globals.get('get_datetime_at')?.(index)
  // }

  // const driftCorrection = (start: number, end: number, gapWidth: number) => {
  //   return interpreter.value.globals.get('drift_correction')?.(
  //     start,
  //     end,
  //     gapWidth
  //   )
  // }

  if (startEl) {
    const init = () => {
      setTimeout(() => {
        interpreter.value = _window.pyscript.interpreter
        $initialized.next(true)

        // Cleanup
        startEl?.removeEventListener('click', init)
        startEl?.remove()
      }, 0)
    }
    startEl.onclick = init
  } else {
    throw 'Failed to detect PyScript initialization'
  }

  return {
    // Getters
    interpreter,
    $initialized,

    // Actions
    deleteDataPoints,
    // findGaps,
    // fillGaps,
    changeValues,
    // getDataFrame,
    // setFilter,
    shift,
    shiftUnits,
    selectedShiftUnit,
    shiftAmount,
    interpolate,
    selectedInterpolationMethod,
    driftCorrection,
    driftGapWidth,
    selectedDriftCorrectionMethod,
    // getValueAt,
    // getDatetimeAt,
    // getIndexAt,
    instantiateDataFrame,
    addDataPoints,
    // count,
    operators,
    selectedOperator,
    operationValue,
  }
})
