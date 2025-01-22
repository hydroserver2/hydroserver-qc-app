<template>
  <div class="text-center">
    <v-chip color="grey-lighten-4" elevation="2" variant="elevated">
      <b class="mr-2 text-red">{{ selectedData?.points.length }}</b>
      Data Point{{ selectedData?.points.length === 1 ? '' : 's' }}
      selected
    </v-chip>
  </div>
  <div ref="plot"></div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'

// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { usePlotlyStore } from '@/store/plotly'
import { useDataSelection } from '@/composables/useDataSelection'
import { storeToRefs } from 'pinia'
import {
  handleClick,
  handleRelayout,
  handleSelected,
} from '@/utils/plotting/plotly'
import { useDataVisStore } from '@/store/dataVisualization'

const plot = ref<HTMLDivElement>()
const { graphSeriesArray, selectedSeries, plotlyOptions, plotlyRef } =
  storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())

onMounted(async () => {
  const myPlot = await Plotly.newPlot(
    plot.value,
    plotlyOptions.value.data,
    plotlyOptions.value.layout,
    plotlyOptions.value.config
  )

  plotlyRef.value = myPlot

  // myPlot.on('plotly_click', handleClick)
  // myPlot.on('plotly_relayout', handleRelayout)
  myPlot.on('plotly_selected', handleSelected)

  // https://plotly.com/javascript/plotlyjs-function-reference/#plotlyupdate
})
</script>

<style scoped></style>
