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
  MULT = 'MULT',
  DIV = 'DIV',
  ADD = 'ADD',
  SUB = 'SUB',
  ASSIGN = 'ASSIGN',
}

export enum FilterOperation {
  LT = 'LT',
  LTE = 'LTE',
  GT = 'GT',
  GTE = 'GTE',
  E = 'E',
}

export const usePyStore = defineStore('py', () => {
  const interpreter: Ref<any> = ref(null)
  const $initialized = new Subject<boolean>()
  const startEl = document.getElementById('start') // Used to detect when PyScript has finished initializing

  /**
   * Delete rows from the DataFrame
   * @param index An array containing the list of index of values to perform the operations on.
   * @returns
   */
  const deleteDataPoints = (index: number[]) => {
    return interpreter.value.globals.get('delete_data_points')?.(index)
  }

  /**
   * Find gaps in the data
   * @param value The time value
   * @param unit The time unit (TimeUnit)
   * @returns
   */
  const findGaps = (value: number, unit: TimeUnit) => {
    return interpreter.value.globals.get('find_gaps')?.(value, unit)
  }

  /**
   * Find gaps and fill them with placeholder value
   * @param gap Intervals to detect as gaps
   * @param fill Interval used to fill the detected gaps
   * @returns
   */
  const fillGaps = (gap: [number, TimeUnit], fill: [number, TimeUnit]) => {
    return interpreter.value.globals.get('fill_gaps')?.(gap, fill)
  }

  /**
   * Shifts the selected indexes by a constant
   * @param index The index list of entries to shift
   * @param timeUnit {@link TimeUnit}
   * @param timeValue Number of {@link TimeUnit}
   * @returns
   */
  const shift = (index: number[], timeValue: number, timeUnit: TimeUnit) => {
    interpreter.value.globals.get('shift_points')?.(index, timeValue, timeUnit)
  }

  /**
   * Retrieve the DataFrame object from EditService as json
   * @returns
   */
  const getDataFrame = () => {
    return interpreter.value.globals.get('get_data_frame')?.()
  }

  /**
   * @param filter A dictionary of key(FilterOperation) - value pairs
   * @returns
   */
  const setFilter = (filter: { [key: string]: number }) => {
    return interpreter.value.globals.get('set_filter')?.(JSON.stringify(filter))
  }

  /**
   *
   * @param index An array containing the list of index of values to perform the operations on.
   * @param operator The operator that will be applied
   * @param value The value to use in the operation
   * @returns The modified DataFrame
   */
  const changeValues = (index: number[], operator: Operator, value: number) => {
    return interpreter.value.globals.get('change_values')?.(
      index,
      operator,
      value
    )
  }

  /**
   * @param range
   */
  const interpolate = (index: number[]) => {
    return interpreter.value.globals.get('interpolate')?.(index)
  }

  /**
   * @param index
   * @returns The value for the row at the given index
   */
  const getValueAt = (index: number) => {
    return interpreter.value.globals.get('get_value_at')?.(index)
  }

  /**
   * @param index
   * @returns The datetime for the row at the given index
   */
  const getDatetimeAt = (index: number) => {
    return interpreter.value.globals.get('get_datetime_at')?.(index)
  }

  const driftCorrection = (index: number[], gapWidth: number) => {
    return interpreter.value.globals.get('drift_correction')?.(index, gapWidth)
  }

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
    findGaps,
    fillGaps,
    changeValues,
    getDataFrame,
    setFilter,
    shift,
    interpolate,
    driftCorrection,
    getValueAt,
    getDatetimeAt,
  }
})
