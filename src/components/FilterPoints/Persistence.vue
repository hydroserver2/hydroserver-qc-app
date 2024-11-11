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
        v-model.number="valueThreshold"
        type="number"
        max-width="15rem"
        suffix="times in a row"
        min="1"
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

const valueThreshold = ref(2)

import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumFilterOperations } from '@/types'
import { useEChartsStore } from '@/store/echarts'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'
import { useDataSelection } from '@/composables/useDataSelection'

const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { selectedIndex, selectedRange } = useDataSelection()
const { updateVisualizationData } = useEChartsStore()

const emit = defineEmits(['close'])
const onPersistence = async () => {
  let selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.PERSISTENCE,
    valueThreshold.value,
    selectedRange.value
  )

  selectedData.value = {}
  selection = Array.from(selection)
  selection.forEach((index: number) => {
    selectedData.value[index] = {
      index: index,
      date: selectedSeries.value.data.dataFrame.get_datetime_at(index),
      value: selectedSeries.value.data.dataFrame.get_value_at(index),
    }
  })

  brushSelections.value = []
  updateVisualizationData()
  emit('close')
}

const startDateString = computed(() => {
  const startIndex = selectedIndex.value[0]
  const startDate = selectedData.value[startIndex]
    ? new Date(selectedData.value[startIndex].date)
    : selectedSeries.value.data.beginTime

  return formatDate(startDate)
})

const endDateString = computed(() => {
  const endIndex = selectedIndex.value[selectedIndex.value.length - 1]
  const endDate = selectedData.value[endIndex]
    ? new Date(selectedData.value[endIndex].date)
    : selectedSeries.value.data.endTime

  return formatDate(endDate)
})
</script>
