<template>
  <div>
    <div v-show="!isLoading">
      <button id="my_button">Python DOM event handler</button>
    </div>
    <div id="my-id">
      {{ isLoading ? 'Loading...' : '' }}
    </div>

    <br />

    Index: <input type="number" v-model="selectedIndex" />
    <br />
    Operation:
    <button @click="onDelete">Delete</button>
    <button @click="onAddOne">Add 1</button>

    <pre>{{ timeseries }}</pre>
  </div>
</template>

<script setup lang="ts">
console.log('Vue App setup...')

import { Ref, ref } from 'vue'
import { _Window } from '@/types'
import data from '@/mock/data.json'

const isLoading = ref(true)
const mainInterpreter: any = ref(null)
const _window = window as _Window
const timeseries: Ref<(string | number)[][]> = ref([])
const selectedIndex = ref(0)

timeseries.value = data.value[0].dataArray.slice(0, 5)

const init = async () => {
  setTimeout(() => {
    mainInterpreter.value = _window.pyscript.interpreter
    isLoading.value = false
  }, 0)
}

// Detect when PyScript has finished initializing
const startEl = document.getElementById('start')
if (startEl) {
  startEl.onclick = init
} else {
  throw 'Failed to detect PyScript initialization'
}

const onDelete = () => {
  const handleDelete = mainInterpreter.value.globals.get('handleDelete')
  handleDelete(timeseries.value, selectedIndex.value)
}

const onAddOne = () => {
  const handleAddOne = mainInterpreter.value.globals.get('handleAddOne')
  handleAddOne(timeseries.value, selectedIndex.value)
}
</script>

<style lang="scss" scoped></style>
@/mock/data
