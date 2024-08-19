<template>
  <v-app id="inspire">
    <v-app-bar class="px-3" color="blue-grey-darken-4" flat>
      <v-app-bar-title>HydroServer QC</v-app-bar-title>
    </v-app-bar>

    <v-main class="bg-grey-lighten-3">
      <v-container>
        <v-card class="mb-4">
          <v-card-title class="text-body-1 bg-grey-lighten-3">
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
          <!-- WORK IN PROGRESS: DATETIME RANGE PICKERS -->
          <v-tabs v-model="filterTab">
            <v-tab value="logical">Logical</v-tab>
            <v-tab value="daterange">Date Range</v-tab>
          </v-tabs>

          <v-card-text>
            <v-tabs-window v-model="filterTab">
              <v-tabs-window-item value="logical">
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
              </v-tabs-window-item>

              <v-tabs-window-item value="daterange">
                <div class="d-flex gap-1">
                  <v-menu
                    v-model="showPickerMenu"
                    :close-on-content-click="false"
                    min-width="50px"
                  >
                    <template #activator="{ props }">
                      <v-text-field
                        clearable
                        v-bind="{ ...commonAttrs, ...props }"
                        prepend-inner-icon="mdi-calendar-clock"
                        :model-value="datePickerValue"
                        label="From"
                      >
                      </v-text-field>
                    </template>

                    <v-card>
                      <v-tabs v-model="activePickerTab" class="bg-blue-grey">
                        <v-tab value="date">
                          <v-icon>mdi-calendar</v-icon>
                        </v-tab>
                        <v-spacer />
                        <v-tab value="time">
                          <v-icon>mdi-clock-outline</v-icon>
                        </v-tab>
                      </v-tabs>

                      <v-window v-model="activePickerTab">
                        <v-window-item value="date">
                          <v-date-picker
                            :model-value="datePickerValue"
                            header="Select date"
                            tile
                          ></v-date-picker>
                        </v-window-item>

                        <v-window-item value="time">
                          <v-time-picker
                            :model-value="timePickerValue"
                            v-bind="commonAttrs"
                            use-seconds
                            ampm-in-title
                            scrollable
                          />
                        </v-window-item>
                      </v-window>

                      <v-card-actions>
                        <v-spacer></v-spacer>
                        <v-btn variant="text" @click="showPickerMenu = false">
                          Cancel
                        </v-btn>
                      </v-card-actions>
                    </v-card>
                  </v-menu>

                  <v-menu
                    v-model="showPickerMenu"
                    :close-on-content-click="false"
                    min-width="50px"
                  >
                    <template #activator="{ props }">
                      <v-text-field
                        clearable
                        v-bind="{ ...commonAttrs, ...props }"
                        prepend-inner-icon="mdi-calendar-clock"
                        :model-value="pickerInputValue"
                        label="To"
                      >
                      </v-text-field>
                    </template>

                    <v-card>
                      <v-tabs v-model="activePickerTab" class="bg-blue-grey">
                        <v-tab value="date">
                          <v-icon>mdi-calendar</v-icon>
                        </v-tab>
                        <v-spacer />
                        <v-tab value="time">
                          <v-icon>mdi-clock-outline</v-icon>
                        </v-tab>
                      </v-tabs>

                      <v-window v-model="activePickerTab">
                        <v-window-item value="date">
                          <v-date-picker
                            :model-value="datePickerValue"
                            header="Select date"
                            tile
                          ></v-date-picker>
                        </v-window-item>

                        <v-window-item value="time">
                          <v-time-picker
                            :model-value="timePickerValue"
                            v-bind="commonAttrs"
                            use-seconds
                            ampm-in-title
                            scrollable
                          />
                        </v-window-item>
                      </v-window>

                      <v-card-actions>
                        <v-spacer></v-spacer>
                        <v-btn variant="text" @click="showPickerMenu = false">
                          Cancel
                        </v-btn>
                      </v-card-actions>
                    </v-card>
                  </v-menu>
                </div>
              </v-tabs-window-item>
            </v-tabs-window>
          </v-card-text>
        </v-card>

        <v-card v-show="initialized" class="bg-blue-grey-darken-2">
          <v-card-title>
            {{ data.value[0].components[0] }}
          </v-card-title>
          <v-card-subtitle>{{ timeseries.length }} Data Points</v-card-subtitle>
          <v-card-text class="d-flex py-2 align-center gap-2">
            <div>
              {{ selected.length }} item{{ selected.length == 1 ? '' : 's' }}
              selected
            </div>
            <v-btn
              variant="text"
              :disabled="!selected.length"
              @click="selected = []"
              >Unselect All</v-btn
            >
            <v-spacer></v-spacer>
            <v-switch
              v-model="renderPreview"
              label="Render Preview"
              title="If toggled on, only the first 100 data points will be rendered."
              hide-details
              inset
              color="light-blue-lighten-3"
              class="text-white"
            ></v-switch>
          </v-card-text>
          <v-divider class="mt-2"></v-divider>

          <v-data-table
            v-model="selected"
            :items="renderPreview ? timeseries.slice(0, 100) : timeseries"
            :loading="isLoading"
            height="60vh"
            hover
            item-value="index"
            items-per-page="100"
            :items-per-page-options="[15, 25, 50, 100]"
            show-select
            fixed-header
            :rowProps="getRowProps"
          >
            <template v-slot:item.index="{ item }">
              {{ getIndexAt(item.index) }}
            </template>

            <template v-slot:item.datetime="{ item }">
              {{ getDateTimeAt(getIndexAt(item.index)) }}
            </template>

            <template v-slot:item.value="{ item }">
              {{ getValueAt(getIndexAt(item.index)) }}
            </template>
          </v-data-table>
        </v-card>
      </v-container>
    </v-main>

    <v-navigation-drawer location="left" width="350">
      <div class="table-actions">
        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1 bg-grey-lighten-3">
            Change Values
          </v-card-title>
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

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1 bg-grey-lighten-3">
            Shift</v-card-title
          >
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

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1 bg-grey-lighten-3"
            >Gap Analysis</v-card-title
          >
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

        <v-card :disabled="!timeseries || !timeseries.length">
          <v-card-title class="text-body-1 bg-grey-lighten-3">
            Other Operations
          </v-card-title>
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
    </v-navigation-drawer>

    <v-navigation-drawer location="right" width="400">
      <div class="d-flex pa-2 align-center">
        <v-icon color="grey" class="mr-2">mdi-history</v-icon> Logs
        <v-spacer></v-spacer>
        <v-btn @click="clearLogs" :disabled="!logger.length" variant="flat"
          >Clear</v-btn
        >
      </div>
      <v-divider></v-divider>
      <div
        class="text-caption text-center py-1 font-weight-light bg-grey-lighten-4"
      >
        Most recent
      </div>
      <v-divider></v-divider>
      <v-table density="compact">
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
      <v-divider v-if="logger.length"></v-divider>
    </v-navigation-drawer>
  </v-app>

  <link
    href="https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900"
    rel="stylesheet"
  />
</template>

<script setup lang="ts">
import { Ref, ref, onBeforeMount, watch } from 'vue'
import { usePyStore, FilterOperation } from '@/stores/py'
import data from '@/mock/data.json'
// @ts-ignore
import tsadata from '@/mock/tsa_data.csv'
import { _Window } from './types'
import { TimeUnit, Operator } from '@/stores/py'
import { VTimePicker } from 'vuetify/labs/VTimePicker'

// Use stores
const py = usePyStore()
const initialized = ref(false)

const timeseries: Ref<{ index: number; datetime: number; value: number }[]> =
  ref([])

const logger: Ref<
  { datetime: number; message: string; duration: number; isLoading?: boolean }[]
> = ref([])

const isLoading = ref(false)
const renderPreview = ref(true)
const filterTab = ref(0)

const selected: Ref<number[]> = ref([])
const parsedData: Ref<any> = ref({
  components: [],
  dataArray: [],
})

const commonAttrs = {
  hideDetails: true,
}

const initializedSub = py.$initialized.subscribe(() => {
  const start = performance.now()
  initialized.value = true
  parseDataFrame()
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
const gapAmount = ref(15)

// FILL
const fillUnits = [...Object.keys(TimeUnit)]
const selectedFillUnit = ref(fillUnits[1])
const fillAmount = ref(15)

// DRIFT
const driftGapWidth = ref(1)

// FILTERS
const filterOperators = [...Object.keys(FilterOperation)]
const selectedFilter = ref(filterOperators[2])
const filterValue = ref(12)
const appliedFilters: Ref<{ [key: string]: number }> = ref({})
// WORK IN PROGRESS: DATETIME PICKER
const showPickerMenu = ref(false)
const pickerInputValue = ref(null)
const datePickerValue = ref(new Date('2018-03-02'))
const timePickerValue = ref('11:15')
const activePickerTab = ref('date')

watch(showPickerMenu, (show: boolean) => {
  if (!show) {
    // menu is closing then reset the activeTab
    activePickerTab.value = 'date'
  }
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
  // tsaData.components = ['DateTime', ' Value']
  // tsaData.dataArray = tsadata.map((item: any) => [
  //   item.DateTime,
  //   +item[' Value'],
  // ])
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

const getIndexAt = (index: number) => {
  return py.getIndexAt(index)
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

const getRowProps = (row: any) => {
  if (selected.value.includes(row.index)) {
    return { class: 'bg-blue-grey-lighten-2' }
  }
  return undefined
}

const findGaps = () => {
  const start = performance.now()
  // @ts-ignore
  const gaps = py.findGaps(gapAmount.value, TimeUnit[selectedGapUnit.value])
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
    // parseDataFrame()
    isLoading.value = false
  }, 0)
}

const onFillGaps = () => {
  isLoading.value = true
  setTimeout(() => {
    fillGaps()
    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const fillGaps = () => {
  const start = performance.now()
  const gaps = py.fillGaps(
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
  py.shift(index, +shiftAmount.value, TimeUnit[selectedShiftUnit.value])
  const end = performance.now()
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Shift',
    duration: end - start,
  })
}

const onShift = (tableIndex: number[]) => {
  const index = tableIndex.map((i) => py.getIndexAt(i))
  isLoading.value = true
  setTimeout(() => {
    shift(index)
    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const deleteDataPoints = (index: number[]) => {
  const start = performance.now()
  py.deleteDataPoints(index)
  const end = performance.now()
  selected.value = []
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Delete data points',
    duration: end - start,
  })
}

const onDeleteDataPoints = (tableIndex: number[]) => {
  const index = tableIndex.map((i) => py.getIndexAt(i))
  isLoading.value = true
  setTimeout(() => {
    deleteDataPoints(index)
    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const changeValues = (index: number[], operator: Operator, value: number) => {
  const start = performance.now()
  py.changeValues(index, operator, value)
  const end = performance.now()
  logger.value.unshift({
    datetime: Date.now(),
    message: 'Change values',
    duration: end - start,
  })
}

const onChangeValues = (tableIndex: number[]) => {
  const index = tableIndex.map((i) => py.getIndexAt(i))
  isLoading.value = true
  setTimeout(() => {
    changeValues(
      index,
      // @ts-ignore
      Operator[selectedOperator.value],
      +operationValue.value
    )
    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const onAddFilter = (key: string, value: number) => {
  isLoading.value = true
  setTimeout(() => {
    addFilter(key, value)
    parseDataFrame()
    selected.value = []
    isLoading.value = false
  }, 0)
}

const addFilter = (key: string, value: number) => {
  const start = performance.now()
  appliedFilters.value[key] = +value
  py.setFilter(appliedFilters.value)
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
    py.setFilter(appliedFilters.value)
    const end = performance.now()
    parseDataFrame()
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
    py.setFilter(appliedFilters.value)
    const end = performance.now()
    parseDataFrame()
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
  const index = tableIndex.map((i) => py.getIndexAt(i))
  isLoading.value = true
  setTimeout(() => {
    interpolate(index)
    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const interpolate = (index: number[]) => {
  const start = performance.now()
  py.interpolate(index)
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
    const index = [...selected.value].map((i) => py.getIndexAt(i))
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

    parseDataFrame()
    isLoading.value = false
  }, 0)
}

const driftCorrection = (start: number, end: number, gapWidth: number) => {
  const s = performance.now()
  py.driftCorrection(start, end, gapWidth)
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
  while (collection.length < n) {
    const index = randomIntFromInterval(0, parsedData.value.dataArray.length)
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
  let indexes = getNUniqueIndexes(50)

  console.log(`[TEST]: Deleting ${indexes.length} data points...`)
  deleteDataPoints(indexes)
  console.log('\tDone')

  console.log(`[TEST]: Setting filter...`)
  addFilter(FilterOperation.GTE, 10.45)
  clearFilters()
  console.log('\tdone')

  console.log(`[TEST]: Interpolating first 10 deleted data points...`)
  interpolate(indexes.slice(0, 10))
  console.log('\tDone')

  indexes = getNUniqueIndexes(50)
  console.log(`[TEST]: Shifting DateTime of ${indexes.length} data points...`)
  shift(indexes)
  console.log('\tDone')

  console.log(`[TEST]: Finding gaps...`)
  findGaps()
  console.log('\tDone')

  console.log(`[TEST]: Filling gaps...`)
  fillGaps()
  console.log('\tDone')

  indexes = getNUniqueIndexes(50)
  console.log(`[TEST]: Changing values of ${indexes.length} data points...`)
  changeValues(indexes, Operator.ADD, 1)
  console.log('\tDone')

  console.log(`[TEST]: Applying drift correction of 1 to first 10 values...`)
  driftCorrection(0, 9, 1)
  console.log('\tDone')

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
