import { defineStore } from 'pinia'
import { _Window } from '@/types'
import { Ref, ref } from 'vue'
import { Subject } from 'rxjs'

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
  const $initialized = new Subject<boolean>()

  // Change Values
  const operators = [...Object.keys(Operator)]
  const selectedOperator = ref(0)
  const operationValue = ref(0.1)

  // GAP ANALYSYS
  const interpolateValues = ref(false)
  const selectedInterpolationMethod = ref(InterpolationMethods.LINEAR)
  const gapUnits = [...Object.keys(TimeUnit)]
  const selectedGapUnit = ref(gapUnits[1])
  const gapAmount = ref(15)

  // FILL
  const fillUnits = [...Object.keys(TimeUnit)]
  const selectedFillUnit = ref(fillUnits[1])
  const fillAmount = ref(15)

  // DRIFT CORRECTION
  const selectedDriftCorrectionMethod = ref(DriftCorrectionMethods.LINEAR)
  const driftGapWidth = ref(1)

  // SHIFT VALUES
  const shiftUnits = [...Object.keys(TimeUnit)]
  const selectedShiftUnit = ref(shiftUnits[1])
  const shiftAmount = ref(15)

  /** Instantiates a new Pandas DataFrame and returns the instance */
  // const instantiateDataFrame = (dataArray: any[], components: string[]) => {
  //   const dataString = JSON.stringify({
  //     dataArray,
  //     components,
  //   })
  //   const wrapperClass = _window.edit_service_wrapper
  //   const instance = wrapperClass(dataString) // because it's from the main thread we don't need to await
  //   return instance
  // }

  addEventListener('py:all-done', () => {
    console.log('Python execution is complete.')
    $initialized.next(true)
  })

  // ============= EDIT DATA =================

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
  //  * @param filter A dictionary of key(FilterOperation) - value pairs
  //  * @returns
  //  */
  // const setFilter = (filter: { [key: string]: number }) => {
  //   return interpreter.value.globals.get('set_filter')?.(JSON.stringify(filter))
  // }

  // /**
  //  * @param points An array [date, value, qualifierCode] representing the points to add.
  //  * @returns The length of the current DataFrame
  //  */
  // const count = () => {
  //   return interpreter.value.globals.get('count')?.()
  // }

  return {
    // Getters
    $initialized,
    shiftUnits,
    selectedShiftUnit,
    shiftAmount,
    selectedInterpolationMethod,
    driftGapWidth,
    selectedDriftCorrectionMethod,
    // instantiateDataFrame,
    operators,
    selectedOperator,
    operationValue,
    interpolateValues,
    selectedGapUnit,
    gapAmount,
    gapUnits,
    selectedFillUnit,
    fillAmount,
    fillUnits,
  }
})
