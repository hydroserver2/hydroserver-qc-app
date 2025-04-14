<template>
  <div class="d-flex flex-column">
    <div class="d-flex">
      <div class="d-flex align-center gap-2">
        <v-switch
          v-model="areTooltipsEnabled"
          @update:model-value="handleRelayout"
          color="primary"
          label="Tooltips"
          :disabled="visiblePoints > tooltipsMaxDataPoints"
          hide-details
        />

        <v-progress-circular v-if="isUpdating" color="primary" indeterminate />

        <!-- <v-text-field
          type="number"
          label="Disable tooltips after"
          v-model="tooltipsMaxDataPoints"
          density="compact"
          hide-details
          suffix="data points"
          min="0"
          width="240"
          :loading="isUpdating"
        ></v-text-field> -->

        <!-- <label v-if="visiblePoints"
          >Showing <span class="text-red">{{ visiblePoints }}</span> data
          points</label
        > -->
      </div>

      <v-spacer></v-spacer>

      <v-chip
        v-if="selectedData?.length"
        color="grey-darken-2"
        variant="outlined"
        class="align-self-center"
        hide-details
      >
        <b class="mr-2 text-red">{{ selectedData?.length }}</b>
        Data Point{{ selectedData?.length === 1 ? '' : 's' }}
        selected
      </v-chip>
    </div>
    <v-divider></v-divider>
    <div ref="plot" class="flex-grow-1"></div>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'

// @ts-ignore no type definitions
import { usePlotlyStore } from '@/store/plotly'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { handleNewPlot, handleRelayout } from '@/utils/plotting/plotly'

const plot = ref<HTMLDivElement>()
const { isUpdating, areTooltipsEnabled, visiblePoints, tooltipsMaxDataPoints } =
  storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())

onMounted(async () => {
  handleNewPlot(plot.value)
})
</script>

<style scoped></style>
