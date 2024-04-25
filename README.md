# HydroServer Quality Control

This Vue 3 application makes use of [PyScript](https://docs.pyscript.net/2024.4.1/) in order to enable Python script execution in the browser.


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

## Accessing JavaScript objects from Python
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
