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
              {{ selected.length }} item{{
                selected.length == 1 ? '' : 's'
              }}
              selected
            </div>
            <v-spacer></v-spacer>
            <v-btn :disabled="!selected.length" @click="selected = []"
              >Unselect All</v-btn
            >
          </v-card-subtitle>
          <v-divider class="mt-2"></v-divider>

          <v-card-text>
            <v-data-table
              v-model="selected"
              :items="timeseries"
              item-value="index"
              items-per-page="15"
              :items-per-page-options="[15, 25, 50, 100]"
              show-select
            >
              <template v-slot:item.index="{ value }">
                {{ value }}
              </template>

              <template v-slot:item.datetime="{ value }">
                {{ new Date(value).toLocaleDateString() }}
                {{ new Date(value).toLocaleTimeString() }}
              </template>

              <template v-slot:item.value="{ value }">
                {{ value }}
              </template>
            </v-data-table>
          </v-card-text>
        </v-card>
      </v-container>
    </v-main>

    <v-navigation-drawer location="left" width="350">
      <div class="table-actions">
        <v-btn
          :disabled="!selected.length"
          block
          @click="deleteDataPoints(selected)"
          color="red"
        >
          Delete points
        </v-btn>
        <v-btn
          :disabled="!selected.length"
          block
          @click="changeValues(selected)"
          >Change Values (Add +1)</v-btn
        >
        <v-btn :disabled="!selected.length" block @click="shift(selected)"
          >Shift Datetimes</v-btn
        >
        <v-btn :disabled="!selected.length" block @click="interpolate(selected)"
          >Interpolate Values</v-btn
        >
        <v-btn
          :disabled="!selected.length || true"
          block
          @click="driftCorrection(selected, 1)"
          >Drift Correction</v-btn
        >

        <v-divider></v-divider>

        <!-- <v-btn class="d-block" block id="my_button">Python Event</v-btn> -->
        <v-btn block @click="findGaps">Find Gaps</v-btn>
        <v-btn block @click="fillGaps">Fill Gaps</v-btn>
        <v-btn block @click="setFilter">Set Filter</v-btn>
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
            <td class="text-medium-emphasis">{{ log.duration }} ms</td>
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
import { _Window } from './types'
import { TimeUnit, Operator } from '@/stores/py'

// Use stores
const py = usePyStore()
const initialized = ref(false)

const timeseries: Ref<{ index: number; datetime: number; value: number }[]> =
  ref([])

const logger: Ref<{ datetime: number; message: string; duration: number }[]> =
  ref([])

const selected: Ref<number[]> = ref([])

const initializedSub = py.$initialized.subscribe(() => {
  initialized.value = true
  fetchDataFrame()
  initializedSub.unsubscribe()

  logger.value.push({
    datetime: Date.now(),
    message: 'App started',
    duration: 50,
  })
})

onBeforeMount(() => {
  ;(window as _Window).dataset = JSON.stringify(data) // Make the dataset available to the python script
})

// =============
// METHODS
// =============

/**
 * Fetch the pandas DataFrame that was initialized in the EditService
 */
const fetchDataFrame = () => {
  const df = JSON.parse(py.getDataFrame())
  const data = []
  const keys = Object.keys(df)
  const length = Object.entries(df[keys[0]]).length

  // TODO: PyScript cannot convert a Pandas DataFrame to a JS object. For now, we iterate the JSON serialized data to build the timeseries
  for (let i = 0; i < length; i++) {
    data.push({
      index: i,
      datetime: df[keys[0]][i],
      value: df[keys[1]][i],
      // selectable: true,
    })
  }

  timeseries.value = data
}

const findGaps = () => {
  const gaps = py.findGaps(15, TimeUnit.MINUTE)
  console.log(gaps)
  logger.value.push({
    datetime: Date.now(),
    message: 'Find gaps',
    duration: 50,
  })
}

const fillGaps = () => {
  const gaps = py.fillGaps([15, TimeUnit.MINUTE], [15, TimeUnit.MINUTE])
  console.log(gaps)
  fetchDataFrame()
  logger.value.push({
    datetime: Date.now(),
    message: 'Fill gaps',
    duration: 50,
  })
}

const shift = (index: number[]) => {
  py.shift(index, 10, TimeUnit.MINUTE)
  fetchDataFrame()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Shift',
    duration: 50,
  })
}

const deleteDataPoints = (index: number[]) => {
  py.deleteDataPoints(index)
  fetchDataFrame()
  selected.value = []
  logger.value.push({
    datetime: Date.now(),
    message: 'Delete data points',
    duration: 50,
  })
}

const changeValues = (index: number[]) => {
  py.changeValues(index, Operator.ADD, 1)
  fetchDataFrame()
  logger.value.push({
    datetime: Date.now(),
    message: 'Change values',
    duration: 50,
  })
}

const setFilter = () => {
  const filteredResults = py.setFilter({ [FilterOperation.GTE]: 10.45 })
  console.log(filteredResults)
  logger.value.push({
    datetime: Date.now(),
    message: 'Set filter',
    duration: 50,
  })
}

const interpolate = (index: number[]) => {
  py.interpolate(index)
  selected.value = []
  fetchDataFrame()
  logger.value.push({
    datetime: Date.now(),
    message: 'Interpolate',
    duration: 50,
  })
}

const driftCorrection = (index: number[], gapWidth: number) => {
  py.driftCorrection(index, gapWidth)
  selected.value = []
  fetchDataFrame()
  logger.value.push({
    datetime: Date.now(),
    message: 'Drift correction',
    duration: 50,
  })
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
