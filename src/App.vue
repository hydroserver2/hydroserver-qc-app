<template>
  <div>
    <div v-if="!isLoading">
      <button id="my_button">Run JS in Python</button>
      <button id="my_button2" @click="printInterpreter">
        Run Python in JS
      </button>
    </div>
    <div id="my-id">
      {{ isLoading ? "Loading..." : "" }}
    </div>
  </div>
</template>

<script setup lang="ts">
import {
  hooks,
  PyWorker,
} from "https://pyscript.net/snapshots/2023.09.1.RC2/core.js";

import { ref } from "vue";

const isLoading = ref(true);
const mainInterpreter: any = ref(null);
const workerInterpreter: any = ref(null);

console.log("Vue App setup...");
console.log(hooks);

const worker = PyWorker("/src/worker.py", { config: "/src/pyconfig.json" });

worker.sync.alert_message = (message: string) => {
  alert(message);
};

hooks.onInterpreterReady.add((utils: any, element: any) => {
  console.log("Main thread ready!");
  console.log(element);
  mainInterpreter.value = utils.interpreter;
  isLoading.value = false;
});

const printInterpreter = () => {
  const mult = mainInterpreter.value.globals.get("multiplyTwoNumbers");
  const a = 2;
  const b = 4;
  alert(`Multiplying ${a} and ${b} in Python: ` + mult?.(a, b));
};

hooks.onWorkerReady.add((utils: any, element: any) => {
  console.log("Worker thread ready!");
  console.log(element);
  console.log(utils);
  workerInterpreter.value = utils.interpreter;
  // element.sync.alert_message = (message: string) => {
  //   console.log(message);
  // };
});
</script>

<style lang="scss" scoped></style>
