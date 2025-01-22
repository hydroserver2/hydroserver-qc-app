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
import { useEChartsStore } from '@/store/echarts'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'
import { useDataSelection } from '@/composables/useDataSelection'
import { EnumFilterOperations } from '@/utils/plotting/observationRecord'

const { gapUnits } = usePyStore()
const { gapAmount, selectedGapUnit } = storeToRefs(usePyStore())
const { selectedSeries } = storeToRefs(useEChartsStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { selectedIndex, selectedRange, applySelection } = useDataSelection()

const emit = defineEmits(['close'])
const onFindGaps = async () => {
  // TODO: this only returns the last point of each gap
  const selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.FIND_GAPS,
    +gapAmount.value,
    // @ts-ignore
    TimeUnit[selectedGapUnit.value],
    selectedRange.value
  )

  applySelection(selection)

  emit('close')
}

const startDateString = computed(() => {
  const startIndex = selectedIndex.value[0]
  const startDate = selectedData.value[startIndex]
    ? new Date(selectedData.value[startIndex].x)
    : selectedSeries.value.data.beginTime

  return formatDate(startDate)
})

const endDateString = computed(() => {
  const endIndex = selectedIndex.value[selectedIndex.value.length - 1]
  const endDate = selectedData.value[endIndex]
    ? new Date(selectedData.value[endIndex].x)
    : selectedSeries.value.data.endTime

  return formatDate(endDate)
})
</script>
