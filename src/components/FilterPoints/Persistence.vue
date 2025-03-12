<template>
  <v-card>
    <v-card-title>Filter by persistent values</v-card-title>

    <v-divider></v-divider>

    <v-card-text>
      <v-timeline
        direction="horizontal"
        align="center"
        side="start"
        hide-opposite
      >
        <v-timeline-item dot-color="green" size="small">
          <div class="text-center">
            <div class="text-caption">From:</div>
            <strong>{{ startDateString }}</strong>
          </div>
        </v-timeline-item>

        <v-timeline-item dot-color="green" size="small">
          <div class="text-center">
            <div class="text-caption">To:</div>
            <strong>{{ endDateString }}</strong>
          </div>
        </v-timeline-item>
      </v-timeline>

      Select points where the values are the same at least:

      <v-text-field
        class="mt-2"
        v-model.number="times"
        type="number"
        max-width="15rem"
        suffix="times in a row"
        min="2"
      />
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onPersistence"
        >Filter</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { ref } from 'vue'

const times = ref(2)

import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumFilterOperations } from '@/utils/plotting/observationRecord'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'
import { useDataSelection } from '@/composables/useDataSelection'

const { selectedSeries } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
import { usePlotlyStore } from '@/store/plotly'
const { redraw, plotlyRef } = usePlotlyStore()
const { applySelection } = useDataSelection()

const emit = defineEmits(['close'])
const onPersistence = async () => {
  const selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.PERSISTENCE,
    times.value,
    selectedData.value
      ? [
          selectedData.value[0],
          selectedData.value[selectedData.value.length - 1],
        ]
      : undefined
  )

  console.log(selection)
  applySelection(selection)
  emit('close')
}

const startDateString = computed(() => {
  let dateStr = selectedSeries.value.data.beginTime
  if (selectedData.value) {
    const startIndex = selectedData.value[0]
    dateStr =
      plotlyRef?.data[0].x[startIndex] || selectedSeries.value.data.beginTime
  }

  return formatDate(new Date(Date.parse(dateStr)))
})

const endDateString = computed(() => {
  let dateStr = selectedSeries.value.data.endTime
  if (selectedData.value) {
    const endIndex = selectedData.value[selectedData.value.length - 1]
    dateStr =
      plotlyRef?.data[0].x[endIndex] || selectedSeries.value.data.endTime
  }

  return formatDate(new Date(Date.parse(dateStr)))
})
</script>
