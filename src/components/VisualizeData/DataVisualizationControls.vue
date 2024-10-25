<template>
  <v-container>
    <v-card class="mb-4">
      <v-card-title class="text-body-1">
        Filters
        <v-badge
          v-if="Object.keys(appliedFilters).length"
          :content="Object.keys(appliedFilters).length"
          inline
          color="blue"
        ></v-badge>
      </v-card-title>
      <v-divider></v-divider>
      <template v-if="Object.keys(appliedFilters).length">
        <v-card-text class="d-flex gap-1">
          <div class="d-flex gap-1">
            <v-chip
              border="double blue"
              variant="outlined"
              closable
              color="blue"
              v-for="key of Object.keys(appliedFilters)"
              :key="key"
              @click:close="removeFilter(key)"
              >{{ key }}: {{ appliedFilters[key] }}</v-chip
            >
          </div>
          <v-spacer></v-spacer>
          <v-btn @click="clearFilters" variant="flat">Clear</v-btn>
        </v-card-text>

        <v-divider></v-divider>
      </template>

      <v-card-text>
        <div class="d-flex gap-1">
          <v-select
            label="Operation"
            :items="filterOperators"
            v-model="selectedFilter"
            v-bind="commonAttrs"
          ></v-select>
          <v-text-field
            label="Value"
            v-model="filterValue"
            step="0.1"
            type="number"
            width="30"
            v-bind="commonAttrs"
          >
          </v-text-field>
          <v-btn
            color="blue-grey-lighten-1"
            @click="onAddFilter(selectedFilter, filterValue)"
            prepend-icon="mdi-plus"
            >Add Filter</v-btn
          >
        </div>
      </v-card-text>
    </v-card>
  </v-container>

  <div class="table-actions">
    <v-card>
      <v-card-title class="text-body-1"> Change Values </v-card-title>
      <v-divider></v-divider>
      <v-card-text>
        <div class="d-flex gap-1">
          <v-select
            label="Operation"
            :items="operators"
            v-model="selectedOperator"
          ></v-select>
          <v-text-field
            label="Value"
            v-model="operationValue"
            type="number"
            width="30"
          >
          </v-text-field>
        </div>
        <v-btn
          :disabled="!selected.length"
          block
          @click="onChangeValues(selected)"
          color="blue-grey-lighten-1"
          >Apply Operation</v-btn
        >
      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
        <v-btn
          :disabled="!selected.length"
          block
          color="blue-grey-lighten-1"
          @click="onInterpolate(selected)"
          >Interpolate</v-btn
        >
      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
        <v-text-field
          label="Drift"
          type="number"
          class="mb-4"
          step="0.1"
          v-model="driftGapWidth"
          v-bind="commonAttrs"
        >
        </v-text-field>

        <v-btn
          :disabled="!selected.length"
          block
          color="blue-grey-lighten-1"
          @click="onDriftCorrection"
          >Apply Drift Correction</v-btn
        >
      </v-card-text>
    </v-card>

    <v-card>
      <v-card-title class="text-body-1"> Shift</v-card-title>
      <v-divider></v-divider>
      <v-card-text>
        <div class="d-flex gap-1 mb-4">
          <v-select
            label="Time Unit"
            :items="shiftUnits"
            v-model="selectedShiftUnit"
            v-bind="commonAttrs"
          ></v-select>
          <v-text-field
            width="30"
            label="Amount"
            type="number"
            v-model="shiftAmount"
            v-bind="commonAttrs"
          >
          </v-text-field>
        </div>
        <v-btn
          :disabled="!selected.length"
          color="blue-grey-lighten-1"
          block
          @click="onShift(selected)"
          >Apply Shift</v-btn
        >
      </v-card-text>
    </v-card>

    <v-card>
      <v-card-title class="text-body-1">Gap Analysis</v-card-title>
      <v-divider></v-divider>
      <v-card-text>
        <div class="d-flex gap-1">
          <v-select
            label="Gap Unit"
            :items="gapUnits"
            v-model="selectedGapUnit"
            v-bind="commonAttrs"
          ></v-select>
          <v-text-field
            width="30"
            label="Amount"
            type="number"
            v-model="gapAmount"
            v-bind="commonAttrs"
          >
          </v-text-field>
        </div>
        <div class="mt-4 d-flex gap-1">
          <v-select
            label="Fill Unit"
            :items="fillUnits"
            v-model="selectedFillUnit"
            v-bind="commonAttrs"
          ></v-select>
          <v-text-field
            width="30"
            label="Amount"
            type="number"
            v-model="fillAmount"
            v-bind="commonAttrs"
          >
          </v-text-field>
        </div>
      </v-card-text>
      <v-divider></v-divider>
      <v-card-text class="d-flex flex-column gap-1">
        <v-btn block color="blue-grey-lighten-1" @click="onFindGaps"
          >Find Gaps</v-btn
        >
      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
        <v-btn block color="blue-grey-lighten-1" @click="onFillGaps"
          >Find & Fill Gaps</v-btn
        >
        <div class="text-right">
          <v-checkbox
            label="Interpolate Values"
            v-model="interpolateValues"
            color="light-blue"
            v-bind="commonAttrs"
          ></v-checkbox>
        </div>
      </v-card-text>
    </v-card>

    <v-card>
      <v-card-title class="text-body-1"> Other Operations </v-card-title>
      <v-divider></v-divider>
      <v-card-text>
        <v-btn
          :disabled="!selected.length"
          block
          @click="onDeleteDataPoints(selected)"
          color="red"
          prepend-icon="mdi-delete"
        >
          Delete points
        </v-btn>
      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
        <v-btn
          prepend-icon="mdi-play"
          color="blue-grey-lighten-1"
          block
          @click="onRunTests"
          >Run All</v-btn
        >
      </v-card-text>
    </v-card>
  </div>

  <div>
    <div class="d-flex pa-2 align-center">
      <v-icon color="grey" class="mr-2">mdi-history</v-icon> Logs
      <v-spacer></v-spacer>
      <v-btn @click="clearLogs" :disabled="!logger.length" variant="flat"
        >Clear</v-btn
      >
    </div>
    <v-divider></v-divider>
    <div class="text-caption text-center py-1 font-weight-light">
      Most recent
    </div>
    <v-divider></v-divider>
    <v-table density="compact">
      <tbody>
        <tr v-for="log in logger">
          <td class="font-weight-light">
            {{ new Date(log.datetime).toLocaleTimeString() }}
          </td>
          <td class="text-caption">{{ log.message }}</td>
          <td class="text-right" :class="getDurationColor(log.duration)">
            {{ log.duration.toFixed(0) }} ms
          </td>
        </tr>
      </tbody>
    </v-table>
    <v-divider v-if="logger.length"></v-divider>
  </div>
</template>

<script setup lang="ts">
import { Ref, ref } from 'vue'
import { usePyStore, FilterOperation, TimeUnit, Operator } from '@/store/py'
// @ts-ignore
import { _Window } from '@/types'
import { storeToRefs } from 'pinia'
import { useEChartsStore } from '@/store/echarts'
const { graphSeriesArray } = storeToRefs(useEChartsStore())
const { createVisualization } = useEChartsStore()

// Use stores
const py = usePyStore()
const initialized = ref(false)

const logger: Ref<
  { datetime: number; message: string; duration: number; isLoading?: boolean }[]
> = ref([])

const isLoading = ref(false)

const selected: Ref<number[]> = ref([])

const commonAttrs = {
  hideDetails: true,
}

const initializedSub = py.$initialized.subscribe(() => {
  const start = performance.now()
  initialized.value = true
  // parseDataFrame()
  initializedSub.unsubscribe()

  const end = performance.now()
  logger.value.unshift({
    datetime: Date.now(),
    message: `App Started`,
    duration: end - start,
  })
})

// CHANGE VALUES
const operators = [...Object.keys(Operator)]
const selectedOperator = ref(operators[2])
const operationValue = ref(1)

// SHIFT VALUES
const shiftUnits = [...Object.keys(TimeUnit)]
const selectedShiftUnit = ref(shiftUnits[1])
const shiftAmount = ref(5)

// GAP ANALYSYS
const interpolateValues = ref(false)
const gapUnits = [...Object.keys(TimeUnit)]
const selectedGapUnit = ref(gapUnits[1])
const gapAmount = ref(30)

// FILL
const fillUnits = [...Object.keys(TimeUnit)]
const selectedFillUnit = ref(fillUnits[1])
const fillAmount = ref(30)

// DRIFT
const driftGapWidth = ref(1)

// FILTERS
const filterOperators = [...Object.keys(FilterOperation)]
const selectedFilter = ref(filterOperators[2])
const filterValue = ref(12)

// TODO: move to store
const appliedFilters: Ref<{ [key: string]: number }> = ref({})

// =============
// METHODS
// =============

const measureEllapsedTime = (fn: () => any, message?: string): any => {
  if (message) {
    console.log(message)
  }
  const start = performance.now()
  const response = fn()
  const end = performance.now()
  console.log(`\tDone in ${(end - start).toFixed(2)} ms`)
  return response
}

const getIndexAt = (index: number) => {
  return graphSeriesArray.value[0].data.dataFrame.get_index_at(index)
}

const getDateTimeAt = (index: number) => {
  const datetime = new Date(
    graphSeriesArray.value[0].data.dataFrame.get_datetime_att(index)
  )

  return `${datetime.toLocaleDateString()} ${datetime.toLocaleTimeString()}`
}

const getValueAt = (index: number) => {
  return graphSeriesArray.value[0].data.dataFrame.get_value_at(index).toFixed(2)
}

const clearLogs = () => {
  logger.value = []
}

const getDurationColor = (duration: number) => {
  if (duration > 200) {
    return 'text-red'
  } else if (duration > 50) {
    return 'text-orange-darken-2'
  }
  return 'text-green'
}

// const getRowProps = (row: any) => {
//   if (selected.value.includes(row.index)) {
//     return { class: 'bg-blue-grey-lighten-2' }
//   }
//   return undefined
// }

const findGaps = () => {
  const start = performance.now()
  const gaps = graphSeriesArray.value[0].data.dataFrame.find_gaps(
    gapAmount.value,
    // @ts-ignore
    TimeUnit[selectedGapUnit.value]
  )
  const end = performance.now()
  console.log(gaps)
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Find gaps',
    duration: end - start,
  })
}

const onFindGaps = () => {
  isLoading.value = true
  setTimeout(() => {
    findGaps()
    createVisualization()
    isLoading.value = false
  }, 0)
}

const onFillGaps = () => {
  isLoading.value = true
  setTimeout(() => {
    fillGaps()
    createVisualization()
    isLoading.value = false
  }, 0)
}

const fillGaps = () => {
  const start = performance.now()
  const gaps = graphSeriesArray.value[0].data.dataFrame.fill_gaps(
    // @ts-ignore
    [+gapAmount.value, TimeUnit[selectedGapUnit.value]],
    // @ts-ignore
    [+fillAmount.value, TimeUnit[selectedFillUnit.value]],
    interpolateValues.value
  )
  const end = performance.now()
  console.log(gaps)
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Find & Fill gaps',
    duration: end - start,
  })
}

const shift = (index: number[]) => {
  const start = performance.now()
  // @ts-ignore
  graphSeriesArray.value[0].data.dataFrame.shift_points(
    index,
    +shiftAmount.value,
    // @ts-ignore
    TimeUnit[selectedShiftUnit.value]
  )
  const end = performance.now()
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Shift',
    duration: end - start,
  })
}

const onShift = (tableIndex: number[]) => {
  const index = tableIndex.map((i) =>
    graphSeriesArray.value[0].data.dataFrame.get_index_at(i)
  )
  isLoading.value = true
  setTimeout(() => {
    shift(index)
    createVisualization()
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const deleteDataPoints = (index: number[]) => {
  const start = performance.now()
  graphSeriesArray.value[0].data.dataFrame.delete_data_points(index)
  const end = performance.now()
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Delete data points',
    duration: end - start,
  })
}

const onDeleteDataPoints = (tableIndex: number[]) => {
  const index = tableIndex.map((i) =>
    graphSeriesArray.value[0].data.dataFrame.get_index_at(i)
  )
  isLoading.value = true
  setTimeout(() => {
    deleteDataPoints(index)
    createVisualization()
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const changeValues = (index: number[], operator: Operator, value: number) => {
  const start = performance.now()
  graphSeriesArray.value[0].data.dataFrame.change_values(index, operator, value)
  const end = performance.now()
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Change values',
    duration: end - start,
  })
}

const onChangeValues = (tableIndex: number[]) => {
  const index = tableIndex.map((i) =>
    graphSeriesArray.value[0].data.dataFrame.get_index_at(i)
  )
  isLoading.value = true
  setTimeout(() => {
    changeValues(
      index,
      // @ts-ignore
      Operator[selectedOperator.value],
      +operationValue.value
    )
    createVisualization()
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const onAddFilter = (key: string, value: number) => {
  isLoading.value = true
  setTimeout(() => {
    addFilter(key, value)
    createVisualization()
    // parseDataFrame()
    selected.value = []
    isLoading.value = false
  }, 0)
}

const addFilter = (key: string, value: number) => {
  const start = performance.now()
  appliedFilters.value[key] = +value
  graphSeriesArray.value[0].data.dataFrame.set_filter(appliedFilters.value)
  const end = performance.now()
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Set filter',
    duration: end - start,
  })
}

const removeFilter = (key: string) => {
  isLoading.value = true
  setTimeout(() => {
    const start = performance.now()
    delete appliedFilters.value[key]
    graphSeriesArray.value[0].data.dataFrame.set_filter(appliedFilters.value)
    const end = performance.now()
    createVisualization()
    // parseDataFrame()
    selected.value = []
    isLoading.value = false
    logger.value.unshift({
      datetime: Date.now(),
      message: 'Remove filter',
      duration: end - start,
    })
  }, 0)
}

const clearFilters = () => {
  isLoading.value = true
  setTimeout(() => {
    const start = performance.now()
    appliedFilters.value = {}
    graphSeriesArray.value[0].data.dataFrame.set_filter(appliedFilters.value)
    const end = performance.now()
    createVisualization()
    selected.value = []
    isLoading.value = false
    logger.value.unshift({
      datetime: Date.now(),
      message: 'Clear filters',
      duration: end - start,
    })
  }, 0)
}

const onInterpolate = (tableIndex: number[]) => {
  const index = tableIndex.map((i) =>
    graphSeriesArray.value[0].data.dataFrame.get_index_at(i)
  )
  isLoading.value = true
  setTimeout(() => {
    // TODO: value error when interpolating values lesser than 1
    interpolate(index)
    createVisualization()
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const interpolate = (index: number[]) => {
  const start = performance.now()
  graphSeriesArray.value[0].data.dataFrame.interpolate(index)
  const end = performance.now()
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Interpolate',
    duration: end - start,
  })
}

const onDriftCorrection = () => {
  if (!selected.value.length) {
    return
  }

  isLoading.value = true
  setTimeout(() => {
    const groups: number[][] = [[]]
    const index = [...selected.value].map((i) =>
      graphSeriesArray.value[0].data.dataFrame.get_index_at(i)
    )
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
      driftCorrection(start, end, +driftGapWidth.value)
    })

    createVisualization()
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const driftCorrection = (start: number, end: number, gapWidth: number) => {
  const s = performance.now()
  graphSeriesArray.value[0].data.dataFrame.drift_correction(
    start,
    end,
    gapWidth
  )
  const e = performance.now()
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Drift correction',
    duration: e - s,
  })
}

const randomIntFromInterval = (min: number, max: number) => {
  return Math.floor(Math.random() * (max - min + 1) + min)
}

const getNUniqueIndexes = (n: number) => {
  const collection: number[] = []
  while (
    collection.length < n &&
    graphSeriesArray.value[0].data.dataFrame.count() > n
  ) {
    const index = randomIntFromInterval(
      0,
      graphSeriesArray.value[0].data.dataFrame.count() - 1
    )
    if (!collection.includes(index)) {
      collection.push(index)
    }
  }

  collection.sort((a, b) => a - b)

  return collection
}

const onRunTests = () => {
  isLoading.value = true
  setTimeout(() => {
    runTests()
    isLoading.value = false
  }, 0)
}

const runTests = () => {
  console.log('============= STARTING TESTS =================')
  let indexes = getNUniqueIndexes(5)

  console.log(`[TEST]: Deleting ${indexes.length} data points...`)
  deleteDataPoints(indexes)
  console.log('\tDone')

  // console.log(`[TEST]: Setting filter...`)
  // addFilter(FilterOperation.GTE, 0.005)
  // clearFilters()
  // console.log('\tdone')

  console.log(`[TEST]: Interpolating first 10 deleted data points...`)
  interpolate(indexes.slice(0, 10))
  console.log('\tDone')

  indexes = getNUniqueIndexes(5)
  console.log(`[TEST]: Shifting DateTime of ${indexes.length} data points...`)
  shift(indexes)
  console.log('\tDone')

  console.log(`[TEST]: Finding gaps...`)
  findGaps()
  console.log('\tDone')

  console.log(`[TEST]: Filling gaps...`)
  fillGaps()
  console.log('\tDone')

  indexes = getNUniqueIndexes(5)
  console.log(`[TEST]: Changing values of ${indexes.length} data points...`)
  changeValues(indexes, Operator.ADD, 1)
  console.log('\tDone')

  console.log(`[TEST]: Applying drift correction of 1 to first 10 values...`)
  driftCorrection(0, 9, 1)
  console.log('\tDone')

  // parseDataFrame()
  createVisualization()
}
</script>

<style lang="scss" scoped>
.table-actions {
  padding: 1rem;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
</style>
