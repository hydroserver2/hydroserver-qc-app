<template>
  <div class="text-center">
    <v-chip color="grey-lighten-4" elevation="2" variant="elevated">
      <b class="mr-2 text-red">{{ selectedIndex.length }}</b>
      Data Point{{ selectedIndex.length === 1 ? '' : 's' }}
      selected
    </v-chip>
  </div>
  <div ref="plot" style="width: 1000px; height: 500px"></div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'

// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { usePlotlyStore } from '@/store/plotly'
import { useDataSelection } from '@/composables/useDataSelection'
import { storeToRefs } from 'pinia'

const plot = ref<HTMLDivElement>()
const { graphSeriesArray, selectedSeries, plotlyOptions } =
  storeToRefs(usePlotlyStore())
const { selectedIndex } = useDataSelection()

onMounted(async () => {
  console.log(plotlyOptions.value.datasets)

  const myPlot = await Plotly.newPlot(
    plot.value,
    plotlyOptions.value.data,
    plotlyOptions.value.layout,
    plotlyOptions.value.config
  )

  console.log(myPlot)

  myPlot.on('plotly_selected', function (eventData: any) {
    console.log(eventData)

    // Plotly.restyle(myPlot, 'marker.color', [colors], [0])
  })

  // TODO: bind selection control keys (Ctrl, Shift, etc)
  // TODO: too costly
  // myPlot
  //   .on('plotly_click', function (eventData: any) {
  //     if ('selectedpoints' in eventData.points[0].fullData) {
  //       const point = eventData.points[0]
  //       point.data.selectedpoints = [point.pointIndex]

  //       Plotly.update(
  //         myPlot,
  //         {
  //           selectedpoints: [point.pointIndex],
  //         },
  //         {},
  //         0
  //       )
  //     }
  //   })
  //   .on('plotly_relayout', (eventData: any) => {
  //     if ('selections' in eventData) {
  //       Plotly.update(
  //         myPlot,
  //         {},
  //         {
  //           selections: [],
  //         }
  //       )
  //     }
  //   })

  // https://plotly.com/javascript/plotlyjs-function-reference/#plotlyupdate
})
</script>

<style scoped></style>
