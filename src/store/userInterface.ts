import { EnumDictionary } from '@/types'
import { defineStore } from 'pinia'
import { ref } from 'vue'

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
  LT = 'Less than',
  LTE = 'Less than or equal to',
  GT = 'Greater than',
  GTE = 'Greater than or equal to',
  E = 'Equal',
  START = 'Start datetime',
  END = 'End datetime',
}

export const FilterOperationFn: EnumDictionary<
  FilterOperation,
  (value: number, toCompare: number) => boolean
> = {
  [FilterOperation.LT]: (value: number, toCompare: number) => {
    return value < toCompare
  },
  [FilterOperation.LTE]: (value: number, toCompare: number) => {
    return value <= toCompare
  },
  [FilterOperation.GT]: (value: number, toCompare: number) => {
    return value > toCompare
  },
  [FilterOperation.GTE]: (value: number, toCompare: number) => {
    return value >= toCompare
  },
  [FilterOperation.E]: (value: number, toCompare: number) => {
    return value == toCompare
  },
  [FilterOperation.START]: (value: number, toCompare: number) => {
    return value == toCompare
  },
  [FilterOperation.END]: (value: number, toCompare: number) => {
    return value == toCompare
  },
}

export enum RateOfChangeOperation {
  LT = 'Less than',
  LTE = 'Less than or equal to',
  GT = 'Greater than',
  GTE = 'Greater than or equal to',
  E = 'Equal',
}

export const RateOfChangeComparator: EnumDictionary<
  RateOfChangeOperation,
  (value: number, toCompare: number) => boolean
> = {
  [RateOfChangeOperation.LT]: (value: number, toCompare: number) => {
    return value < toCompare
  },
  [RateOfChangeOperation.LTE]: (value: number, toCompare: number) => {
    return value <= toCompare
  },
  [RateOfChangeOperation.GT]: (value: number, toCompare: number) => {
    return value > toCompare
  },
  [RateOfChangeOperation.GTE]: (value: number, toCompare: number) => {
    return value >= toCompare
  },
  [RateOfChangeOperation.E]: (value: number, toCompare: number) => {
    return value == toCompare
  },
}

export enum InterpolationMethods {
  LINEAR = 'LINEAR',
}

export enum DriftCorrectionMethods {
  LINEAR = 'LINEAR',
}

export enum DrawerType {
  File = 'File',
  Edit = 'Edit',
  Select = 'Select',
  None = '',
}

type View = DrawerType.Edit | DrawerType.Select

export const useUIStore = defineStore('userInterface', () => {
  // Navigation Drawer
  const selectedDrawer = ref<DrawerType>(DrawerType.Select)
  const isDrawerOpen = ref(true)

  // View
  const currentView = ref<View>(DrawerType.Select)

  // Selection view content
  const cardHeight = ref(40)
  const tableHeight = ref(35)

  const onRailItemClicked = (title: DrawerType) => {
    if (selectedDrawer.value === title) {
      isDrawerOpen.value = !isDrawerOpen.value
    } else {
      selectedDrawer.value = title
      if (title === DrawerType.Edit) currentView.value = DrawerType.Edit
      if (title === DrawerType.Select) currentView.value = DrawerType.Select
      isDrawerOpen.value = true
    }
  }

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

  return {
    selectedDrawer,
    isDrawerOpen,
    currentView,
    cardHeight,
    tableHeight,
    onRailItemClicked,
    shiftUnits,
    selectedShiftUnit,
    shiftAmount,
    selectedInterpolationMethod,
    driftGapWidth,
    selectedDriftCorrectionMethod,
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
