<template>
  <v-card class="elevation-0" :loading="updating">
    <template v-slot:loader="{ isActive }">
      <v-progress-linear color="primary" :active="isActive" indeterminate />
    </template>

    <keep-alive>
      <v-card-text v-if="option && isDataAvailable">
        <v-chart
          ref="echartsRef"
          :option="option"
          @datazoom="handleDataZoom"
          autoresize
          :style="{ height: `${cardHeight}vh` }"
        />
      </v-card-text>
    </keep-alive>

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

  <v-dialog v-model="openStyleModal" width="40rem">
    <SeriesStyleCard
      datastream-name="datastreamName"
      @submit="updateSeriesStyle"
      @close="openStyleModal = false"
    />
  </v-dialog>
</template>

<script setup lang="ts">
import { useDataVisStore } from '@/store/dataVisualization'
import { ref, watch, computed, nextTick, onUnmounted } from 'vue'
import { storeToRefs } from 'pinia'
import VChart from 'vue-echarts'
import 'echarts'
import { useEChartsStore } from '@/store/echarts'
import { createEChartsOption } from '@/utils/plotting/echarts'
import SeriesStyleCard from '@/components/VisualizeData/SeriesStyleCard.vue'

const props = defineProps({
  cardHeight: { type: Number, required: true },
})

const { loadingStates, plottedDatastreams } = storeToRefs(useDataVisStore())
const { selectedQualifier } = storeToRefs(useDataVisStore())
const openStyleModal = ref(false)
const datastreamName = ref('')

const {
  dataZoomStart,
  dataZoomEnd,
  graphSeriesArray,
  echartsOption: option,
} = storeToRefs(useEChartsStore())

const echartsRef = ref<typeof VChart | null>(null)

const updating = computed(() =>
  Array.from(loadingStates.value.values()).some((isLoading) => isLoading)
)

const isDataAvailable = computed(() =>
  graphSeriesArray.value.some((series) => series.data && series.data.length > 0)
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
    if (echartsRef.value) echartsRef.value.resize()
  })
})

// TODO: Is there a better place to put this watcher?
watch(selectedQualifier, () => {
  option.value = createEChartsOption(graphSeriesArray.value)
})

let isLegendListenerCreated = false
watch(echartsRef, (newValue) => {
  if (newValue && !isLegendListenerCreated) {
    isLegendListenerCreated = true
    const echartsInstance = newValue.chart
    echartsInstance.on('legendselectchanged', (params: any) => {
      console.log('params', params)
      if (params.name && params.selected?.hasOwnProperty(params.name)) {
        datastreamName.value = params.name
        openStyleModal.value = true
        params.selected[params.name] = true
        echartsInstance.setOption(
          {
            legend: [
              {
                selected: params.selected,
              },
            ],
          },
          false
        )
      }
    })
  }
})

const updateSeriesStyle = ({
  lineStyle,
  symbol,
}: {
  lineStyle: string | undefined
  symbol: string | undefined
}) => {
  const echartsInstance = echartsRef.value!.chart
  if (!echartsInstance || !datastreamName.value) return

  const options = echartsInstance.getOption()

  options.series.forEach((series: any) => {
    if (series.name !== datastreamName.value) return
    if (lineStyle === 'none') lineStyle = undefined
    if (symbol === 'none') symbol = undefined

    series.lineStyle = {
      ...series.lineStyle,
      type: lineStyle,
      width: !!lineStyle ? 1 : 0,
    }

    series.symbol = symbol
    series.showSymbol = !!symbol
  })

  echartsInstance.setOption({ series: options.series }, false)
}

onUnmounted(() => {
  if (echartsRef.value && echartsRef.value.echarts) {
    echartsRef.value.echarts.off(
      'legendselectchanged',
      echartsRef.value.echarts._customLegendClickHandler
    )
  }
})
</script>

<style scoped>
.v-card-text {
  padding: 0;
}
</style>
