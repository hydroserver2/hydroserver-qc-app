<template>
  <div>
    <div v-show="!isLoading">
      <button id="my_button">Run JS in Python</button>
      <button @click="handleMultiply">Run Python in JS</button>
    </div>
    <div id="my-id">
      {{ isLoading ? 'Loading...' : '' }}
    </div>
  </div>
</template>

<script setup lang="ts">
console.log('Vue App setup...')

import { ref } from 'vue'
import { _Window } from '@/types'

const isLoading = ref(true)
const mainInterpreter: any = ref(null)
const _window = window as _Window

const init = async () => {
  setTimeout(() => {
    mainInterpreter.value = _window.pyscript.interpreter
    isLoading.value = false

    // This is a way in which we can access any PyScript global
    console.log(_window.pyGlobals.get('some_global'))
  }, 0)
}

// Detect when PyScript has finished initializing
const startEl = document.getElementById('start')
if (startEl) {
  startEl.onclick = init
} else {
  throw 'Failed to detect PyScript initialization'
}

const handleMultiply = () => {
  const mult = mainInterpreter.value.globals.get('multiplyTwoNumbers')
  const a = 2
  const b = 4
  alert(`Multiplying ${a} and ${b} in Python: ` + mult?.(a, b))
}
</script>

<style lang="scss" scoped></style>
