# HydroServer Quality Control

This Vue 3 application makes use of [PyScript](https://docs.pyscript.net/2024.4.1/) in order to enable Python script execution in the browser.


# Bootstraping Python interpreter

```
<!-- index.html -->

<body>
  <!-- ... -->
  <div hidden id="start"></div>
  <!-- ... -->
</body>
</html>
```
```
// py.ts

import { defineStore } from 'pinia'
import { _Window } from '@/types'
import { Ref, ref } from 'vue'

const _window = window as _Window

export const usePyStore = defineStore('py', () => {
  const interpreter: Ref<any> = ref(null)
  const initialized = ref(false)
  const startEl = document.getElementById('start') // Used to detect when PyScript has finished initializing

  if (startEl) {
    const init = () => {
      setTimeout(() => {
        interpreter.value = _window.pyscript.interpreter
        initialized.value = true

        // Cleanup
        startEl?.removeEventListener('click', init)
        startEl?.remove()
      }, 0)
    }
    startEl.onclick = init
  } else {
    throw 'Failed to detect PyScript initialization'
  }

  return {
    // Getters
    interpreter,
    initialized,  // Indicates that Python interpreter has been initialized
  }
})
```
```
# worker.py

from pyscript import Element

# Signal start
Element("start").element.click()
```

## Python to JavaScript communication

```
# main.py

def printMessage(message):
  print(message)
```
```
// some-file.js

interpreter.value.globals.get('printMessage')?.('hello!') // alerts: "hello!"
```

## Triggering JavaScript events and calling functions from Python

```
# main.py

from js import someFunction, dataset

@when("click", "#my_button")
def handle_click(event):
  someFunction("Hello from Python!")
```
```
<!-- App.vue -->

<template>
   <v-btn id="my_button">Trigger Event in Python</v-btn>
</template>

<script setup lang="ts">
// ...
;(window as _Window).someFunction = (message: string) => {
  alert(message)
}
// ...
</script>
```

## Passing JSON data from JavaScript to Python

```
<!-- App.vue -->

<script setup lang="ts">
onBeforeMount(() => {
  ;(window as _Window).dataset = JSON.stringify({ a: 1, b: 2})
})
</script>
```

```
# main.py

import json

my_data = json.loads(dataset)
```
