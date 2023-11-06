<template>
  <div>
    <div v-show="py.initialized">
      <button id="my_button">Python DOM event handler</button>

      <br />

      Index: <input type="number" v-model="selectedIndex" />
      <br />
      Operation:
      <button @click="py.deleteDataPoint(timeseries, selectedIndex)">
        Delete
      </button>
      <button @click="py.addOne(timeseries, selectedIndex)">Add 1</button>

      <pre>{{ timeseries }}</pre>
    </div>
    <div>
      {{ !py.initialized ? 'Loading...' : '' }}
    </div>
  </div>
</template>

<script setup lang="ts">
import { Ref, ref } from 'vue'
import { usePyStore } from '@/stores/py'
import data from '@/mock/data.json'

const py = usePyStore()
const timeseries: Ref<(string | number)[][]> = ref(
  data.value[0].dataArray.slice(0, 10)
)
const selectedIndex = ref(0)
</script>

<style lang="scss" scoped></style>
