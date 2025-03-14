<template>
  <div class="d-flex flex-column">
    <div class="d-flex">
      <div class="d-flex align-center gap-2">
        <v-switch
          v-model="areTooltipsEnabled"
          color="primary"
          label="Tooltips"
          :disabled="isLargeDataset"
          hide-details
        />

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
import { ref, onMounted, Ref } from 'vue'

// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { usePlotlyStore } from '@/store/plotly'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import {
  handleClick,
  handleDeselect,
  handleDoubleClick,
  handleSelected,
} from '@/utils/plotting/plotly'
import { isEqual } from 'lodash-es'

const plot = ref<HTMLDivElement>()
const { plotlyOptions, plotlyRef } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
const areTooltipsEnabled = ref(true)
const isLargeDataset = ref(true)
const tooltipsMaxDataPoints = ref(10 * 1000)
const isUpdating = ref(false)
const visiblePoints: Ref<number> = ref(0)

onMounted(async () => {
  plotlyRef.value = await Plotly.newPlot(
    plot.value,
    plotlyOptions.value.traces,
    plotlyOptions.value.layout,
    plotlyOptions.value.config
  )

  // Binary search
  const findLowerBound = (target: number) => {
    const xData = plotlyRef.value?.data[0].x
    let low = 0
    let high = xData.length
    while (low < high) {
      const mid = (low + high) >>> 1
      // if (Date.parse(xData[mid]) < target) {
      if (xData[mid] < target) {
        low = mid + 1
      } else high = mid
    }
    return low
  }

  const handleRelayout = async (eventData: any) => {
    console.log('handleRelayout')
    selectedData.value = plotlyRef.value?.data[0].selectedpoints || null

    // Plotly fires the relayout event for basically everything.
    // We only need to handle it when panning or zooming
    if (
      isUpdating.value ||
      eventData?.dragmode || // Changing selected tool
      eventData?.selections || // Selecting points
      eventData?.['selections[0].x0'] || // Moving a selected area
      isEqual(eventData, {}) // Double click using pan tool
    ) {
      return
    }

    isUpdating.value = true

    try {
      let yMin = 0
      let yMax = 0
      let xMin = 0
      let xMax = 0

      const layoutUpdates = { ...plotlyOptions.value.layout }
      // Plotly will rewrite timestamps as datestrings. We need to convert them back to timestamps.
      if (typeof layoutUpdates.xaxis.range[0] == 'string') {
        layoutUpdates.xaxis.range[0] = Date.parse(layoutUpdates.xaxis.range[0])
        layoutUpdates.xaxis.range[1] = Date.parse(layoutUpdates.xaxis.range[1])
      }

      const currentRange = plotlyRef.value?.layout.xaxis.range.map(
        (d: string) => {
          if (typeof d == 'string') {
            return Date.parse(d)
          }
          return d
        }
      )

      layoutUpdates.xaxis.range = [
        Math.max(currentRange[0], layoutUpdates.xaxis.range[0]),
        Math.min(currentRange[1], layoutUpdates.xaxis.range[1]),
      ]

      xMin = layoutUpdates.xaxis.range[0]
      xMax = layoutUpdates.xaxis.range[1]

      // Find visible points count using binary search
      // Plotly does not return the indexes. We must find them using binary seach
      const startIdx = findLowerBound(xMin)
      const endIdx = findLowerBound(xMax)

      // auto scale y axis using data from the first trace
      const traceData = plotlyRef.value?.data[0]
      const yData = traceData.y as number[]

      // Find all y-values within the current x-axis range
      yMin = yData[startIdx]
      yMax = yData[endIdx]

      // Could use Math.max and Math.min and spread operator, but this is more memory efficient
      for (let i = startIdx + 1; i < endIdx; i++) {
        if (yMin > yData[i]) {
          yMin = yData[i]
        }

        if (yMax < yData[i]) {
          yMax = yData[i]
        }
      }

      visiblePoints.value = endIdx - startIdx

      // Calculate new y-axis range with padding
      if (visiblePoints.value && yMax !== yMin) {
        const padding = (yMax - yMin) * 0.1 // 10% padding

        layoutUpdates.yaxis = {
          ...plotlyOptions.value.layout.yaxis,
          range: [yMin - padding, yMax + padding],
          autorange: false,
        }
      }

      // Update axis range
      await Plotly.update(plotlyRef.value, {}, layoutUpdates)

      // Threshold check
      const newHoverState =
        visiblePoints.value > tooltipsMaxDataPoints.value ? 'skip' : 'x+y'
      isLargeDataset.value = newHoverState === 'skip'

      // Only update if state changed
      if (plotlyRef.value?.data[0].hoverinfo !== newHoverState) {
        if (newHoverState === 'x+y' && !areTooltipsEnabled.value) {
          return
        }

        await Plotly.restyle(plotlyRef.value, { hoverinfo: [newHoverState] }, 0)
      }
    } finally {
      isUpdating.value = false
    }
  }

  handleRelayout(null)

  plotlyRef.value?.on('plotly_redraw', handleRelayout)
  plotlyRef.value?.on('plotly_relayout', handleRelayout)
  plotlyRef.value?.on('plotly_click', handleClick)
  plotlyRef.value?.on('plotly_selected', handleSelected)
  plotlyRef.value?.on('plotly_deselec', handleDeselect)
  plotlyRef.value?.on('plotly_doubleclick', handleDoubleClick)

  // https://plotly.com/javascript/plotlyjs-function-reference/#plotlyupdate
})
</script>

<style scoped></style>
