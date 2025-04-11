<template>
  <div class="fill-height">
    <v-progress-linear v-if="isUpdating" color="primary" indeterminate />

    <div v-if="isDataAvailable" class="fill-height">
      <Plot class="fill-height" />
    </div>

    <template v-else>
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

      <div v-if="plottedDatastreams.length && !isUpdating" class="text-center">
        <v-alert type="warning" dense>
          No data available for the selected date range. Please select a
          different date range to re-plot.
        </v-alert>
      </div>
    </template>
  </div>

  <v-dialog v-if="seriesDatastream" v-model="openStyleModal" width="40rem">
    <!-- <SeriesStyleCard
      :datastream-id="seriesDatastream.id"
      @submit="updateSeriesOption"
      @close="openStyleModal = false"
    /> -->
  </v-dialog>
</template>

<script setup lang="ts">
import { useDataVisStore } from '@/store/dataVisualization'
import { ref, computed, watch, nextTick, onMounted } from 'vue'
import { storeToRefs } from 'pinia'
import SeriesStyleCard from '@/components/VisualizeData/SeriesStyleCard.vue'
import { Datastream } from '@/types'
import { useDataSelection } from '@/composables/useDataSelection'
import { usePlotlyStore } from '@/store/plotly'
import Plot from '@/components/VisualizeData/Plot.vue'

const { plotlyOptions } = storeToRefs(usePlotlyStore())
const { dispatchSelection, clearSelected } = useDataSelection()

const { loadingStates, plottedDatastreams, selectedQualifier, selectedData } =
  storeToRefs(useDataVisStore())
const openStyleModal = ref(false)
const seriesDatastream = ref<Datastream | null>(null)

const isUpdating = computed(() =>
  Array.from(loadingStates.value.values()).some((isLoading) => isLoading)
)

const isDataAvailable = computed(() => {
  return plotlyOptions.value.traces?.length
})
</script>

<style scoped></style>
