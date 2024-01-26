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
        <v-card v-show="py.initialized">
          <v-card-title>{{ data.value[0].components[0] }}</v-card-title>
          <v-divider></v-divider>
          <v-card-text>
            <v-table>
              <thead>
                <tr>
                  <th class="text-left">Date</th>
                  <th class="text-left">Value</th>
                  <th class="text-left">Actions</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="(point, index) in timeseries" :key="index">
                  <td>{{ point[0] }}</td>
                  <td>{{ point[1] }}</td>
                  <td>
                    <v-btn
                      class="mr-4"
                      @click="py.deleteDataPoint(timeseries, index)"
                    >
                      Delete </v-btn
                    ><v-btn @click="py.addOne(timeseries, index)">Add</v-btn>
                  </td>
                </tr>
              </tbody>
            </v-table>
          </v-card-text>
          <v-divider></v-divider>
          <v-card-actions>
            <v-spacer></v-spacer>
            <v-btn id="my_button">Trigger Event in Python</v-btn>
          </v-card-actions>
        </v-card>
      </v-container>
    </v-main>
  </v-app>
</template>

<script setup lang="ts">
import { Ref, ref, onBeforeMount } from 'vue'
import { usePyStore } from '@/stores/py'
import { useTheme } from 'vuetify'
import data from '@/mock/data.json'
import { _Window } from './types'

// Use stores
const theme = useTheme()
const py = usePyStore()

const timeseries: Ref<(string | number)[][]> = ref(
  data.value[0].dataArray.slice(0, 10)
)

const drawer = ref(false)

function toggleTheme() {
  theme.global.name.value = theme.global.current.value.dark ? 'light' : 'dark'
}

onBeforeMount(() => {
  ;(window as _Window).dataset = JSON.stringify(data)
})
</script>

<style lang="scss" scoped></style>
