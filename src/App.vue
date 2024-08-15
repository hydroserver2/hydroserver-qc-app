<template>
  <v-app id="inspire">
    <v-app-bar class="px-3" color="grey-darken-3" flat>
      <v-app-bar-title>HydroServer QC</v-app-bar-title>
    </v-app-bar>

    <v-main class="bg-grey-lighten-3">
      <v-container>
        <v-card v-show="initialized" class="bg-blue-darken-2">
          <v-card-title>{{ data.value[0].components[0] }}</v-card-title>
          <v-card-subtitle class="d-flex py-2 align-center">
            <div>
              {{ selected.length }} item{{ selected.length == 1 ? '' : 's' }}
              selected
            </div>
            <v-spacer></v-spacer>
            <v-btn
              variant="flat"
              :disabled="!selected.length"
              @click="selected = []"
              >Unselect All</v-btn
            >
          </v-card-subtitle>
          <v-divider class="mt-2"></v-divider>

          <v-data-table
            v-model="selected"
            :items="timeseries.slice(0, 100)"
            height="70vh"
            hover
            item-value="index"
            items-per-page="15"
            :items-per-page-options="[15, 25, 50, 100]"
            show-select
          >
            <template v-slot:item.index="{ item }">
              {{ item.index }}
            </template>

            <template v-slot:item.datetime="{ item }">
              {{ getDateTimeAt(item.index) }}
            </template>

            <template v-slot:item.value="{ item }">
              {{ getValueAt(item.index) }}
            </template>
          </v-data-table>
        </v-card>
      </v-container>
    </v-main>

    <v-navigation-drawer location="left" width="350" class="bg-grey-lighten-4">
      <div class="table-actions">
        <v-card :disabled="!timeseries || !timeseries.length">
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
              color="blue"
              >Apply Operation</v-btn
            >
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn
              :disabled="!selected.length"
              block
              color="blue"
              @click="onInterpolate(selected)"
              >Interpolate</v-btn
            >
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn
              :disabled="!selected.length"
              block
              color="blue"
              @click="onDriftCorrection"
              >Drift Correction</v-btn
            >
            <v-text-field
              label="Gap Width"
              type="number"
              class="mt-2"
              step="0.1"
              v-model="driftGapWidth"
              v-bind="cmmonAttrs"
            >
            </v-text-field>
          </v-card-text>
        </v-card>

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1"> Shift</v-card-title>
          <v-divider></v-divider>
          <v-card-text>
            <div class="d-flex gap-1 mb-4">
              <v-select
                label="Time Unit"
                :items="shiftUnits"
                v-model="selectedShiftUnit"
                v-bind="cmmonAttrs"
              ></v-select>
              <v-text-field
                width="30"
                label="Amount"
                type="number"
                v-model="shiftAmount"
                v-bind="cmmonAttrs"
              >
              </v-text-field>
            </div>
            <v-btn
              :disabled="!selected.length"
              color="blue"
              block
              @click="onShift(selected)"
              >Apply Shift</v-btn
            >
          </v-card-text>
        </v-card>

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1">Gap Analysis</v-card-title>
          <v-divider></v-divider>
          <v-card-text>
            <div class="d-flex gap-1">
              <v-select
                label="Gap Unit"
                :items="gapUnits"
                v-model="selectedGapUnit"
                v-bind="cmmonAttrs"
              ></v-select>
              <v-text-field
                width="30"
                label="Amount"
                type="number"
                v-model="gapAmount"
                v-bind="cmmonAttrs"
              >
              </v-text-field>
            </div>
            <div class="mt-4 d-flex gap-1">
              <v-select
                label="Fill Unit"
                :items="fillUnits"
                v-model="selectedFillUnit"
                v-bind="cmmonAttrs"
              ></v-select>
              <v-text-field
                width="30"
                label="Amount"
                type="number"
                v-model="fillAmount"
                v-bind="cmmonAttrs"
              >
              </v-text-field>
            </div>
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text class="d-flex flex-column gap-1">
            <v-btn block color="blue" @click="findGaps">Find Gaps</v-btn>
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn block color="blue" @click="onFillGaps"
              >Find & Fill Gaps</v-btn
            >
            <div class="text-right">
              <v-checkbox
                label="Interpolate Values"
                v-model="interpolateValues"
                color="blue"
                v-bind="cmmonAttrs"
              ></v-checkbox>
            </div>
          </v-card-text>
        </v-card>

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1"> Other Operations </v-card-title>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn
              :disabled="!selected.length"
              block
              @click="onDeleteDataPoints(selected)"
              color="red"
            >
              Delete points
            </v-btn>
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn
              color="blue"
              block
              @click="setFilter({ [FilterOperation.GTE]: 10.45 })"
              >Set Filter</v-btn
            >
          </v-card-text>
          <v-divider></v-divider>
          <v-card-text>
            <v-btn color="blue" block @click="runTests">Run Tests</v-btn>
          </v-card-text>
        </v-card>
      </div>
    </v-navigation-drawer>

    <v-navigation-drawer location="right" width="400">
      <div class="d-flex pa-2 align-center">
        Logs
        <v-spacer></v-spacer>
        <v-btn @click="clearLogs" :disabled="!logger.length" variant="flat"
          >Clear</v-btn
        >
      </div>
      <v-divider></v-divider>
      <v-table>
        <tbody>
          <tr v-for="log in logger">
            <td class="text-medium-emphasis">
              {{ new Date(log.datetime).toLocaleTimeString() }}
            </td>
            <td class="text-caption">{{ log.message }}</td>
            <td class="text-right" :class="getDurationColor(log.duration)">
              {{ log.duration.toFixed(0) }} ms
            </td>
          </tr>
        </tbody>
      </v-table>
    </v-navigation-drawer>
  </v-app>

  <link
    href="https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900"
    rel="stylesheet"
  />
</template>

<script setup lang="ts">
import { Ref, ref, onBeforeMount } from 'vue'
import { usePyStore, FilterOperation } from '@/stores/py'
import data from '@/mock/data.json'
// @ts-ignore
import tsadata from '@/mock/tsa_data.csv'
import { _Window } from './types'
import { TimeUnit, Operator } from '@/stores/py'

// Use stores
const py = usePyStore()
const initialized = ref(false)

const timeseries: Ref<{ index: number; datetime: number; value: number }[]> =
  ref([])

const logger: Ref<
  { datetime: number; message: string; duration: number; isLoading?: boolean }[]
> = ref([])

const selected: Ref<number[]> = ref([])
const parsedData: Ref<any> = ref({
  components: [],
  dataArray: [],
})

const cmmonAttrs = {
  hideDetails: true,
}

const initializedSub = py.$initialized.subscribe(() => {
  const start = performance.now()
  initialized.value = true
  parseDataFrame()
  initializedSub.unsubscribe()

  const end = performance.now()
  logger.value.push({
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
const gapAmount = ref(15)

const fillUnits = [...Object.keys(TimeUnit)]
const selectedFillUnit = ref(fillUnits[1])
const fillAmount = ref(15)
const driftGapWidth = ref(1)

onBeforeMount(() => {
  parsedData.value.components = ['DateTime', ' Value']
  parsedData.value.dataArray = tsadata.map((item: any) => [
    item.DateTime,
    +item[' Value'],
  ])

  const tsaData = {
    components: ['DateTime', ' Value'],
    dataArray: tsadata.map((item: any) => [item.DateTime, +item[' Value']]),
  }
  tsaData.components = ['DateTime', ' Value']
  tsaData.dataArray = tsadata.map((item: any) => [
    item.DateTime,
    +item[' Value'],
  ])
  ;(window as _Window).dataset = JSON.stringify(tsaData) // Make the dataset available to the python script
  // ;(window as _Window).dataset = JSON.stringify(data.value[0]) // Make the dataset available to the python script
})

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

const getDateTimeAt = (index: number) => {
  const datetime = new Date(py.getDatetimeAt(index))

  return `${datetime.toLocaleDateString()} ${datetime.toLocaleTimeString()}`
}

const getValueAt = (index: number) => {
  return py.getValueAt(index).toFixed(2)
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

/**
 * Fetch the pandas DataFrame that was initialized in the EditService
 */
const parseDataFrame = () => {
  // const df = measureEllapsedTime(py.getDataFrame, 'Getting DataFrame...')

  // Using `pandas.DataFrame.iterrows`
  // timeseries.value = measureEllapsedTime(() => {
  //   return [...df.value.iterrows()].map((item: [number, any]) => {
  //     const tuple: [number, any] = [...item]
  //     const series = [...tuple[1]]
  //     return {
  //       index: tuple[0],
  //       // datetime: parseInt(tuple[0].value) / 10 ** 6, // miliseconds
  //       datetime: series[0], //.value, // nanoseconds
  //       value: series[1],
  //     }
  //   })
  // }, 'Deconstructing DataFrame using `pandas.DataFrame.iterrows`...')

  // Using `pandas.DataFrame.itertuples`
  // timeseries.value = measureEllapsedTime(() => {
  //   return [...df.value.itertuples()].map((item: any) => {
  //     // TODO: these properties need to
  //     const obj = {
  //       index: item.get(0),
  //       datetime: item.get(1),
  //       value: item.get(2),
  //     }

  //     return obj
  //   })
  // }, 'Deconstructing DataFrame using `pandas.DataFrame.itertuples`...')

  // Using `pandas.DataFrame.values`
  // timeseries.value = measureEllapsedTime(() => {
  //   return [...df.value.values].map((item: any) => {
  //     const obj = {
  //       index: item.get(0),
  //       datetime: item.get(1),
  //       value: item.get(2),
  //     }

  //     return obj
  //   })
  // }, 'Deconstructing DataFrame using `pandas.DataFrame.values`...')

  // Fastest!!!
  // Using `pandas.DataFrame._mgr`
  // Cells are populated using `getValueAt` and `getDateTimeAt`
  timeseries.value = measureEllapsedTime(() => {
    return new Array(py.getDataFrame().length)
      .fill(null)
      .map((_value, index) => {
        return {
          index: index,
          // We just need the properties to shape the table component
          datetime: 0,
          value: 0,
        }
      })
  }, 'Deconstructing DataFrame using `pandas.DataFrame._mgr`...')

  // Using `pandas.DataFrame.get`
  // timeseries.value = measureEllapsedTime(() => {
  //   const dateCol = df['DateTime']
  //   const valueCol = df[' Value']

  //   const collection = []

  //   for (let i = 0; i < df.value.length; i++) {
  //     collection.push({
  //       index: i,
  //       datetime: dateCol.get(i),
  //       value: valueCol.get(i),
  //     })
  //   }
  //   return collection
  // }, 'Deconstructing DataFrame using `pandas.DataFrame.get`...')

  // Using `pandas.DataFrame.iloc`
  // timeseries.value = measureEllapsedTime(() => {
  //   const collection: any = []

  //   for (let i = 0; i < df.value.length; i++) {
  //     const row = df.value.iloc.__getitem__(i)
  //     collection.push({
  //       index: i,
  //       datetime: row.get(0),
  //       value: row.get(1),
  //     })
  //   }
  //   return collection
  // }, 'Deconstructing DataFrame using `pandas.DataFrame.iloc`...')

  // Using numpy
  // timeseries.value = measureEllapsedTime(() => {
  //   return [...df.value.to_numpy()].map((item: any, index: number) => {
  //     const tuple = [...item]
  //     return {
  //       index,
  //       datetime: tuple[0],
  //       value: tuple[1],
  //     }
  //   })
  // }, 'Deconstructing DataFrame using numpy...')

  // // Using JSON serialization
  // timeseries.value = measureEllapsedTime(() => {
  //   const parsed = JSON.parse(df.value.to_json())
  //   const data = []
  //   const keys = Object.keys(parsed)
  //   const length = Object.entries(parsed[keys[0]]).length

  //   // Iterate the JSON serialized data to build the timeseries
  //   for (let i = 0; i < length; i++) {
  //     data.push({
  //       index: i,
  //       datetime: parsed[keys[0]][i],
  //       value: parsed[keys[1]][i],
  //       // selectable: true,
  //     })
  //   }
  //   return data
  // }, 'Deconstructing DataFrame using JSON serialization...')
}

const findGaps = () => {
  const start = performance.now()
  const gaps = py.findGaps(gapAmount.value, TimeUnit[selectedGapUnit.value])
  const end = performance.now()
  console.log(gaps)
  logger.value.push({
    datetime: Date.now(),
    message: 'Find gaps',
    duration: end - start,
  })
}

const onFillGaps = () => {
  fillGaps()
  parseDataFrame()
}

const fillGaps = () => {
  const start = performance.now()
  const gaps = py.fillGaps(
    [gapAmount.value, TimeUnit[selectedGapUnit.value]],
    [fillAmount.value, TimeUnit[selectedFillUnit.value]],
    interpolateValues.value
  )
  const end = performance.now()
  console.log(gaps)
  logger.value.push({
    datetime: Date.now(),
    message: 'Find & Fill gaps',
    duration: end - start,
  })
}

const shift = (index: number[]) => {
  const start = performance.now()
  py.shift(index, shiftAmount.value, TimeUnit[selectedShiftUnit.value])
  const end = performance.now()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Shift',
    duration: end - start,
  })
}

const onShift = (index: number[]) => {
  shift(index)
  parseDataFrame()
}

const deleteDataPoints = (index: number[]) => {
  const start = performance.now()
  py.deleteDataPoints(index)
  const end = performance.now()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Delete data points',
    duration: end - start,
  })
}

const onDeleteDataPoints = (index: number[]) => {
  deleteDataPoints(index)
  parseDataFrame()
}

const changeValues = (index: number[]) => {
  const start = performance.now()
  py.changeValues(index, Operator.ADD, 1)
  const end = performance.now()
  logger.value.push({
    datetime: Date.now(),
    message: 'Change values',
    duration: end - start,
  })
}

const onChangeValues = (index: number[]) => {
  changeValues(index)
  parseDataFrame()
}

const setFilter = (filter: { [key: string]: number }) => {
  const start = performance.now()
  const filteredResults = py.setFilter(filter)
  const end = performance.now()
  console.log(filteredResults)
  logger.value.push({
    datetime: Date.now(),
    message: 'Set filter',
    duration: end - start,
  })
}

const onInterpolate = (index: number[]) => {
  interpolate(index)
  parseDataFrame()
}

const interpolate = (index: number[]) => {
  const start = performance.now()
  py.interpolate(index)
  const end = performance.now()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Interpolate',
    duration: end - start,
  })
}

const onDriftCorrection = () => {
  if (!selected.value.length) {
    return
  }

  const groups: number[][] = [[]]
  const sorted = [...selected.value].sort((a, b) => a - b)

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

  parseDataFrame()
}

const driftCorrection = (start: number, end: number, gapWidth: number) => {
  const s = performance.now()
  py.driftCorrection(start, end, gapWidth)
  const e = performance.now()
  selected.value = []
  logger.value.push({
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
  while (collection.length < n) {
    const index = randomIntFromInterval(0, parsedData.value.dataArray.length)
    if (!collection.includes(index)) {
      collection.push(index)
    }
  }

  collection.sort((a, b) => a - b)

  return collection
}

const runTests = () => {
  console.log('============= STARTING TESTS =================')
  let indexes = getNUniqueIndexes(50)

  console.log(`Deleting ${indexes.length} data points...`)
  deleteDataPoints(indexes)
  console.log('Done')

  console.log(`Setting filter...`)
  setFilter({ [FilterOperation.GTE]: 10.45 })
  console.log('done')

  console.log(`Interpolating first 10 deleted data points...`)
  interpolate(indexes.slice(0, 10))
  console.log('Done')

  console.log(`Finding gaps...`)
  findGaps()
  console.log('Done')

  console.log(`Filling gaps...`)
  fillGaps()
  console.log('Done')

  indexes = getNUniqueIndexes(50)
  console.log(`Changing values of ${indexes.length} data points...`)
  changeValues(indexes)
  console.log('Done')

  indexes = getNUniqueIndexes(50)
  console.log(`Shifting DateTime of ${indexes.length} data points...`)
  shift(indexes)
  console.log('Done')

  console.log(`Applying drift correction of 1 to first 10 values...`)
  driftCorrection(0, 9, 1)
  console.log('Done')

  parseDataFrame()
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
