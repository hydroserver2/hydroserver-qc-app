<template>
  <v-card class="elevation-0" :loading="updating">
    <template v-slot:loader="{ isActive }">
      <v-progress-linear color="primary" :active="isActive" indeterminate />
    </template>

    <v-card-text v-if="option && isDataAvailable">
      <div class="text-center">
        <v-chip color="grey-lighten-4" elevation="2" variant="elevated">
          <b class="mr-2 text-red">{{ selectedIndex.length }}</b>
          Data Point{{ selectedIndex.length === 1 ? '' : 's' }}
          selected
        </v-chip>
      </div>
      <v-chart
        ref="_echartsRef"
        :option="option"
        :style="{ height: `${cardHeight}vh` }"
        autoresize
        @datazoom="handleDataZoom"
        @legendSelectChanged="handleLegendSelected"
        @brushSelected="handleBrushSelected"
        @click="handleClick"
      />
    </v-card-text>

    <div v-if="!isDataAvailable" :style="{ 'min-height': `${cardHeight}vh` }">
      <v-card-text>
        <v-timeline align="start" density="compact">
          <v-timeline-item size="x-small" dot-color="primary">
            <div>
              <strong> Filter: </strong>
            </div>
            <div>
              Filter the datastream table items with the drawer on the left and
              the search bar on the top of the datastreams table.
            </div>
          </v-timeline-item>
          <v-timeline-item size="x-small" dot-color="blue-grey">
            <div>
              <strong> Select a datastream: </strong>
            </div>
            <div>
              Select the datastream you'd like to plot for quality control.
              Additionally, check the 'plot' checkbox to plot up to 4 other
              datastreams on top of the selected datastream. If two datastreams
              share the same observed property and unit, they'll share a y-axis.
            </div>
          </v-timeline-item>
          <v-timeline-item size="x-small" dot-color="secondary">
            <div>
              <strong> Adjust plot settings: </strong>
            </div>
            <div>
              Use the navigation drawer on the left to adjust the time range to
              cover the desired period you wish to observe and set viewing
              preferences for the plot.
            </div>
          </v-timeline-item>
          <v-timeline-item size="x-small" dot-color="orange-lighten-1">
            <div>
              <strong> Edit your datastream: </strong>
            </div>
            <div>
              Use the navigation rail on the far left to switch to the edit view
              (pencil icon) where you'll be able to apply edits to the dataset.
            </div>
          </v-timeline-item>
        </v-timeline>
      </v-card-text>

      <v-card-text
        v-if="plottedDatastreams.length && !updating"
        class="text-center"
      >
        <v-alert type="warning" dense>
          No data available for the selected date range. Please select a
          different date range to re-plot.
        </v-alert>
      </v-card-text>
    </div>
  </v-card>

  <v-dialog v-if="seriesDatastream" v-model="openStyleModal" width="40rem">
    <SeriesStyleCard
      :datastream-id="seriesDatastream.id"
      @submit="updateSeriesOption"
      @close="openStyleModal = false"
    />
  </v-dialog>
</template>

<script setup lang="ts">
import { useDataVisStore } from '@/store/dataVisualization'
import { ref, computed, watch, nextTick, onMounted } from 'vue'
import { storeToRefs } from 'pinia'
import VChart from 'vue-echarts'
import { useEChartsStore } from '@/store/echarts'
import SeriesStyleCard from '@/components/VisualizeData/SeriesStyleCard.vue'
import { LineSeriesOption } from 'echarts'
import { Datastream } from '@/types'
import { useDataSelection } from '@/composables/useDataSelection'

const props = defineProps({
  cardHeight: { type: Number, required: true },
})

const { selectedIndex, dispatchSelection, clearSelected } = useDataSelection()

const { loadingStates, plottedDatastreams, selectedQualifier, selectedData } =
  storeToRefs(useDataVisStore())
const openStyleModal = ref(false)
const seriesDatastream = ref<Datastream | null>(null)

const {
  dataZoomStart,
  dataZoomEnd,
  graphSeriesArray,
  echartsOption: option,
  selectedSeriesIndex,
  brushSelections,
  selectedSeries,
  echartsRef,
} = storeToRefs(useEChartsStore())

const _echartsRef = ref<typeof VChart | null>(null)

onMounted(() => {
  echartsRef.value = _echartsRef.value
})

const updating = computed(() =>
  Array.from(loadingStates.value.values()).some((isLoading) => isLoading)
)

const isDataAvailable = computed(() =>
  graphSeriesArray.value.some((series) => series.data?.dataFrame?.count?.() > 0)
)

function handleDataZoom(event: any) {
  let start, end

  if (event.batch && event.batch.length) {
    // Handle scroll wheel events
    start = event.batch[0].start
    end = event.batch[0].end
  } else if (event.start !== undefined && event.end !== undefined) {
    // Handle zoom box drag events
    start = event.start
    end = event.end
  } else {
    console.error('Unexpected event structure for dataZoom:', event)
    return
  }
  dataZoomStart.value = start
  dataZoomEnd.value = end
}

watch([() => props.cardHeight], ([newHeight], [oldHeight]) => {
  if (Math.abs(newHeight - oldHeight) < 0.2) return
  nextTick(() => {
    echartsRef.value?.resize()
  })
})

// TODO: Is there a better place to put this watcher?
// watch(selectedQualifier, () => {
//   option.value = createEChartsOption(graphSeriesArray.value)
// })

function handleLegendSelected(params: any) {
  console.log('handleLegendSelected')
  if (!params.name || !params.selected?.hasOwnProperty(params.name)) return
  const matchingDatastream = plottedDatastreams.value.find(
    (d) => d.name === params.name
  )
  if (!matchingDatastream) return

  seriesDatastream.value = matchingDatastream
  openStyleModal.value = true
  params.selected[params.name] = true
  if (option.value) option.value.legend = [{ selected: params.selected }]
}

const previousBrushAreas = ref<string | null>(null)

interface BrushArea {
  coordRange?: [number, number][]
}

/** This function assumes only the ECharts box select is being used. Manually check if each point is
 * within the boundaries of at least one selected area since ECharts doesn't keep the actual selections
 * alive if they're outside of the view window.
 */
function handleBrushSelected(params: any) {
  console.log('handleBrushSelected')

  if (selectedSeriesIndex.value === -1) {
    return
  }

  const selectedAreas = params.batch[0].areas
  if (selectedAreas.length <= 0) {
    clearSelected()
    return
  }

  const currentBrush = JSON.stringify(
    selectedAreas.map((a: BrushArea) => a.coordRange)
  )
  if (!currentBrush || currentBrush === previousBrushAreas.value) {
    dispatchSelection()
    return
  }

  previousBrushAreas.value = currentBrush

  const seriesData = selectedSeries.value.data.dataset

  // TODO: if keep option selected, merge selected instead
  clearSelected()

  seriesData.source['date'].forEach((_value: number, index: number) => {
    const x = seriesData.source['date'][index]
    const y = seriesData.source['value'][index]

    for (const area of selectedAreas) {
      if (area.coordRange) {
        if (area.brushType === 'rect') {
          const [rangeX, rangeY] = area.coordRange
          if (
            x >= rangeX[0] &&
            x <= rangeX[1] &&
            y >= rangeY[0] &&
            y <= rangeY[1]
          ) {
            selectedData.value[index] = {
              date: new Date(x),
              value: y,
              index,
            }
            break
          }
        } else if (area.brushType === 'lineY') {
          if (y >= area.coordRange[0] && y <= area.coordRange[1])
            selectedData.value[index] = {
              date: new Date(x),
              value: y,
              index,
            }
        }
      }
    }
  })

  dispatchSelection()
}

function handleClick(params: any) {
  if (!selectedData.value[params.dataIndex]) {
    selectedData.value[params.dataIndex] = {
      date: new Date(params.data[0]),
      value: params.data[1],
      index: params.dataIndex,
    }
  } else {
    delete selectedData.value[params.dataIndex]
  }

  dispatchSelection()
}

const updateSeriesOption = (updatedOptions: Partial<LineSeriesOption>) => {
  if (
    !option.value?.series ||
    !seriesDatastream.value ||
    !Array.isArray(option.value.series)
  )
    return

  // 1. Update ECharts series state
  option.value.series = option.value.series.map((series: any) => {
    // Hack: Assume an empty name is the line plot overlaying the selected scatter plot
    // TODO: Once Echarts allows selecting data on line plots, update this code.
    if (series.name && series.name !== seriesDatastream.value?.name)
      return series

    let seriesOption: LineSeriesOption = {
      ...series,
      ...updatedOptions,
      lineStyle: {
        ...series.lineStyle,
        ...updatedOptions.lineStyle,
      },
      itemStyle: {
        ...series.itemStyle,
        ...updatedOptions.itemStyle,
      },
    }

    if (!series.name) {
      seriesOption.showSymbol = false
    }

    // 2. Update series options in pinia store
    graphSeriesArray.value = graphSeriesArray.value.map((graphSeries) => {
      if (graphSeries.name === seriesDatastream.value?.name) {
        graphSeries.seriesOption = { ...seriesOption }
      }
      return graphSeries
    })

    return seriesOption
  })
}
</script>

<style scoped>
.v-card-text {
  padding: 0;
}
</style>
