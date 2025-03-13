<template>
  <v-card>
    <v-card-title>Fill Gaps</v-card-title>

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
      <p class="text-body-1 mb-4"><b>Fill</b> these gaps with values every:</p>
      <div class="mt-4 d-flex gap-1">
        <v-text-field
          width="30"
          label="Amount"
          type="number"
          v-model="fillAmount"
        >
        </v-text-field>
        <v-select
          label="Fill Unit"
          :items="fillUnits"
          v-model="selectedFillUnit"
        ></v-select>
      </div>

      <v-checkbox
        label="Interpolate Fill Values"
        v-model="interpolateValues"
      ></v-checkbox>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onFillGaps"
        >Fill Gaps</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { TimeUnit, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

const { fillUnits, gapUnits } = usePyStore()
const {
  interpolateValues,
  gapAmount,
  selectedGapUnit,
  selectedFillUnit,
  fillAmount,
} = storeToRefs(usePyStore())

import { EnumEditOperations } from '@/utils/plotting/observationRecord'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'
import { useDataSelection } from '@/composables/useDataSelection'

import { usePlotlyStore } from '@/store/plotly'
const { createVisualization, redraw, updateOptions } = usePlotlyStore()
const { selectedSeries } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { plotlyRef } = usePlotlyStore()
const { clearSelected } = useDataSelection()

// const { selectedIndex, selectedRange } = useDataSelection()

const emit = defineEmits(['close'])
const onFillGaps = async () => {
  await selectedSeries.value.data.dispatch(
    EnumEditOperations.FILL_GAPS,
    // @ts-ignore
    [+gapAmount.value, TimeUnit[selectedGapUnit.value]],
    // @ts-ignore
    [+fillAmount.value, TimeUnit[selectedFillUnit.value]],
    interpolateValues.value,
    selectedData.value
      ? [
          selectedData.value[0],
          selectedData.value[selectedData.value.length - 1],
        ]
      : undefined
  )

  await clearSelected()
  redraw()
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
