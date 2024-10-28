<template>
  <v-card>
    <v-card-title>Select Gaps</v-card-title>

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

      <p class="text-body-1 mb-4"><b>Find</b> gaps of at least:</p>
      <div class="d-flex gap-1">
        <v-text-field
          width="30"
          label="Amount"
          type="number"
          v-model="gapAmount"
        >
        </v-text-field>

        <v-select
          label="Gap Unit"
          :items="gapUnits"
          v-model="selectedGapUnit"
        ></v-select>
      </div>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onFindGaps"
        >Find Gaps</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { TimeUnit, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

const { gapUnits } = usePyStore()
const { gapAmount, selectedGapUnit } = storeToRefs(usePyStore())

import { EnumEditOperations } from '@/types'
import { useEChartsStore } from '@/store/echarts'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'

const { selectedSeries } = storeToRefs(useEChartsStore())
const { selectedData } = storeToRefs(useDataVisStore())

const emit = defineEmits(['close'])
const onFindGaps = async () => {
  const index = selectedData.value.map(
    (point: { date: Date; value: number; index: number }) =>
      selectedSeries.value.data.dataFrame.get_index_at(point.index)
  )

  const range =
    index.length > 1 ? [index[0], index[index.length - 1]] : undefined
  const selection = await selectedSeries.value.data.dispatch(
    EnumEditOperations.FIND_GAPS,
    +gapAmount.value,
    // @ts-ignore
    TimeUnit[selectedGapUnit.value],
    range
  )

  // TODO: select individual datapoints using this selection
  console.log(Array.from(selection))
  emit('close')
}

const startDateString = computed(() => {
  const startDate = selectedData.value[0]
    ? new Date(selectedData.value[0].date)
    : selectedSeries.value.data.beginTime

  return formatDate(startDate)
})

const endDateString = computed(() => {
  const endDate = selectedData.value[selectedData.value.length - 1]
    ? new Date(selectedData.value[selectedData.value.length - 1].date)
    : selectedSeries.value.data.endTime

  return formatDate(endDate)
})
</script>
