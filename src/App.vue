<template>
  <v-app>
    <v-app-bar>
      <v-app-bar-nav-icon @click="drawer = !drawer"></v-app-bar-nav-icon>

      <v-app-bar-title>HydroServer QC</v-app-bar-title>

      <v-btn
        :icon="
          theme.global.name.value === 'dark'
            ? 'mdi-weather-night'
            : 'mdi-lightbulb-on-outline'
        "
        @click="toggleTheme"
      ></v-btn>
    </v-app-bar>

    <v-navigation-drawer v-model="drawer" temporary>
      <!--  -->
    </v-navigation-drawer>

    <v-main>
      <v-container>
        <v-card v-show="initialized">
          <v-card-title>{{ data.value[0].components[0] }}</v-card-title>
          <v-divider></v-divider>
          <v-card-text class="text-right">
            <!-- <div>Selected: {{ selected }}</div> -->
            <div>
              <v-btn class="mr-4" @click="deleteDataPoints(selected)">
                Delete
              </v-btn>
              <v-btn class="mr-4" @click="changeValues(selected)">Add</v-btn>
              <v-btn class="mr-4" @click="shift(selected)">Shift</v-btn>
            </div>
          </v-card-text>
          <v-card-text>
            <v-data-table
              v-model="selected"
              :items="timeseries"
              item-value="index"
              items-per-page="100"
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

          <v-divider></v-divider>
          <v-card-actions>
            <v-spacer></v-spacer>
            <v-btn id="my_button">Trigger Event in Python</v-btn>
            <v-btn @click="findGaps">Find Gaps</v-btn>
            <v-btn @click="fillGaps">Fill Gaps</v-btn>
            <v-btn @click="setFilter">Set Filter</v-btn>
          </v-card-actions>
        </v-card>
      </v-container>
    </v-main>
  </v-app>
</template>

<script setup lang="ts">
import { Ref, ref, onBeforeMount } from 'vue'
import { usePyStore, FilterOperation } from '@/stores/py'
import { useTheme } from 'vuetify'
import data from '@/mock/data.json'
import { _Window } from './types'
import { TimeUnit, Operator } from '@/stores/py'

// Use stores
const theme = useTheme()
const py = usePyStore()
const initialized = ref(false)

const timeseries: Ref<{ index: number; datetime: number; value: number }[]> =
  ref([])

const drawer = ref(false)
const selected: Ref<number[]> = ref([])

function toggleTheme() {
  theme.global.name.value = theme.global.current.value.dark ? 'light' : 'dark'
}

const initializedSub = py.$initialized.subscribe(() => {
  initialized.value = true
  fetchDataFrame()
  initializedSub.unsubscribe()
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
}

const fillGaps = () => {
  const gaps = py.fillGaps([15, TimeUnit.MINUTE], [15, TimeUnit.MINUTE])
  console.log(gaps)
  fetchDataFrame()
}

const shift = (index: number[]) => {
  py.shift(index, 10, TimeUnit.MINUTE)
  fetchDataFrame()
}

const deleteDataPoints = (index: number[]) => {
  py.deleteDataPoints(index)
  fetchDataFrame()
}

const changeValues = (index: number[]) => {
  py.changeValues(index, Operator.ADD, 1)
  fetchDataFrame()
}

const setFilter = () => {
  const filteredResults = py.setFilter({ [FilterOperation.GTE]: 10.45 })
  console.log(filteredResults)
}
</script>

<style lang="scss" scoped></style>
