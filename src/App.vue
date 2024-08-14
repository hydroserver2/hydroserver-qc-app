<template>
  <v-app id="inspire">
    <v-app-bar class="px-3" color="grey-lighten-4" height="72" flat>
      <v-app-bar-title>HydroServer QC</v-app-bar-title>
    </v-app-bar>

    <v-main>
      <v-container>
        <v-card v-show="initialized">
          <v-card-title>{{ data.value[0].components[0] }} </v-card-title>
          <v-card-subtitle class="d-flex py-2 align-center">
            <div>
              {{ selected.length }} item{{ selected.length == 1 ? '' : 's' }}
              selected
            </div>
            <v-spacer></v-spacer>
            <v-btn :disabled="!selected.length" @click="selected = []"
              >Unselect All</v-btn
            >
          </v-card-subtitle>
          <v-divider class="mt-2"></v-divider>

          <v-card-text>
            <v-data-table-virtual
              v-model="selected"
              :items="timeseries.slice(0, 100)"
              height="400"
              hover
              item-value="index"
              items-per-page="15"
              :items-per-page-options="[15, 25, 50, 100]"
              show-select
            >
              <template v-slot:item.index="{ value }">
                {{ value }}
              </template>

              <template v-slot:item.datetime="{ value }">
                {{
                  new Date(parseInt(value.value) / 10 ** 6).toLocaleDateString()
                }}
                {{
                  new Date(parseInt(value.value) / 10 ** 6).toLocaleTimeString()
                }}
              </template>

              <template v-slot:item.value="{ value }">
                {{ value }}
              </template>
            </v-data-table-virtual>
          </v-card-text>
        </v-card>
      </v-container>
    </v-main>

    <v-navigation-drawer location="left" width="350">
      <div class="table-actions">
        <v-btn
          :disabled="!selected.length"
          block
          @click="onDeleteDataPoints(selected)"
          color="red"
        >
          Delete points
        </v-btn>
        <v-btn
          :disabled="!selected.length"
          block
          @click="onChangeValues(selected)"
          >Change Values (Add +1)</v-btn
        >
        <v-btn :disabled="!selected.length" block @click="onShift(selected)"
          >Shift Datetimes</v-btn
        >
        <v-btn
          :disabled="!selected.length"
          block
          @click="onInterpolate(selected)"
          >Interpolate Values</v-btn
        >
        <v-btn
          :disabled="!selected.length || true"
          block
          @click="onDriftCorrection(selected, 1)"
          >Drift Correction</v-btn
        >

        <v-divider></v-divider>

        <!-- <v-btn class="d-block" block id="my_button">Python Event</v-btn> -->
        <v-btn block @click="findGaps">Find Gaps</v-btn>
        <v-btn block @click="onFillGaps()">Fill Gaps</v-btn>
        <v-btn block @click="setFilter()">Set Filter</v-btn>

        <v-spacer></v-spacer>
        <v-divider></v-divider>

        <v-btn color="primary" block @click="runTests">Run Tests</v-btn>
      </div>
    </v-navigation-drawer>

    <v-navigation-drawer location="right" width="400">
      <v-table>
        <tbody>
          <tr v-for="log in logger">
            <td class="text-medium-emphasis">
              {{ new Date(log.datetime).toLocaleTimeString() }}
            </td>
            <td class="text-caption">{{ log.message }}</td>
            <td class="text-medium-emphasis">
              {{ log.duration.toFixed(2) }} ms
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

const timeseries: Ref<
  { index: number; datetime: number; value: number; raw: any }[]
> = ref([])

const logger: Ref<
  { datetime: number; message: string; duration: number; isLoading?: boolean }[]
> = ref([])

const selected: Ref<number[]> = ref([])
const parsedData: Ref<any> = ref({
  components: [],
  dataArray: [],
})

const initializedSub = py.$initialized.subscribe(() => {
  initialized.value = true
  fetchDataFrame()
  initializedSub.unsubscribe()

  logger.value.push({
    datetime: Date.now(),
    message: 'App started',
    duration: 0,
  })
})

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
  console.log(tsaData)
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

/**
 * Fetch the pandas DataFrame that was initialized in the EditService
 */
const fetchDataFrame = () => {
  const df = measureEllapsedTime(py.getDataFrame, 'Getting DataFrame...')

  // Using `pandas.DataFrame.iterrows`
  // timeseries.value = measureEllapsedTime(() => {
  //   return [...df.iterrows()].map((item: [number, any]) => {
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
  //   return [...df.itertuples()].map((item: any) => {
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
  //   return [...df.values].map((item: any) => {
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
  timeseries.value = measureEllapsedTime(() => {
    const valuesArray = [...df._mgr.arrays.get(1).get(0)]
    const dateTimesArray = [...df._mgr.arrays.get(0).get(0)]

    return valuesArray.map((value, index) => {
      return {
        index: index,
        datetime: dateTimesArray[index], // In nanoseconds, unwrap with parseInt(datetime.value) / 10 ** 6,
        value: value,
      }
    })
  }, 'Deconstructing DataFrame using `pandas.DataFrame._mgr`...')

  // Using `pandas.DataFrame.get`
  // timeseries.value = measureEllapsedTime(() => {
  //   const dateCol = df['DateTime']
  //   const valueCol = df[' Value']

  //   const collection = []

  //   for (let i = 0; i < df.length; i++) {
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

  //   for (let i = 0; i < df.length; i++) {
  //     const row = df.iloc.__getitem__(i)
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
  //   return [...df.to_numpy()].map((item: any, index: number) => {
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
  //   const parsed = JSON.parse(df.to_json())
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
  const gaps = py.findGaps(15, TimeUnit.MINUTE)
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
  fetchDataFrame()
}

const fillGaps = () => {
  const start = performance.now()
  const gaps = py.fillGaps([15, TimeUnit.MINUTE], [15, TimeUnit.MINUTE])
  const end = performance.now()
  console.log(gaps)
  logger.value.push({
    datetime: Date.now(),
    message: 'Fill gaps',
    duration: end - start,
  })
}

const shift = (index: number[]) => {
  const start = performance.now()
  py.shift(index, 10, TimeUnit.MINUTE)
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
  fetchDataFrame()
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
  fetchDataFrame()
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
  fetchDataFrame()
}

const setFilter = () => {
  const start = performance.now()
  const filteredResults = py.setFilter({ [FilterOperation.GTE]: 10.45 })
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
  fetchDataFrame()
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

const onDriftCorrection = (index: number[], gapWidth: number) => {
  driftCorrection(index, gapWidth)
  fetchDataFrame()
}

const driftCorrection = (index: number[], gapWidth: number) => {
  const start = performance.now()
  py.driftCorrection(index, gapWidth)
  const end = performance.now()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Drift correction',
    duration: end - start,
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

  fetchDataFrame()
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
