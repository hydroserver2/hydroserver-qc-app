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
      <v-btn :disabled="isUpdating" @click="onFillGaps">Fill Gaps</v-btn>
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { TimeUnit, useUIStore } from '@/store/userInterface'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
const { fillUnits, gapUnits } = useUIStore()
const {
  interpolateValues,
  gapAmount,
  selectedGapUnit,
  selectedFillUnit,
  fillAmount,
} = storeToRefs(useUIStore())
import { EnumEditOperations } from '@/utils/plotting/observationRecord'
import { useDataSelection } from '@/composables/useDataSelection'

import { usePlotlyStore } from '@/store/plotly'
const { redraw } = usePlotlyStore()
const { selectedSeries, isUpdating } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { clearSelected, startDateString, endDateString } = useDataSelection()

const emit = defineEmits(['close'])
const onFillGaps = async () => {
  isUpdating.value = true

  setTimeout(async () => {
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
    isUpdating.value = false
    await redraw()
    emit('close')
  })
}
</script>
