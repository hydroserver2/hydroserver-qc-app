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
import { useDataSelection } from '@/composables/useDataSelection'
import { EnumFilterOperations } from '@/utils/plotting/observationRecord'
import { usePlotlyStore } from '@/store/plotly'

const { gapUnits } = usePyStore()
const { gapAmount, selectedGapUnit } = storeToRefs(usePyStore())
const { selectedSeries, isUpdating } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { dispatchSelection, startDateString, endDateString } = useDataSelection()

const emit = defineEmits(['close'])

const onFindGaps = async () => {
  isUpdating.value = true

  setTimeout(async () => {
    const gaps: [number, number][] =
      await selectedSeries.value.data.dispatchFilter(
        EnumFilterOperations.FIND_GAPS,
        +gapAmount.value,
        // @ts-ignore
        TimeUnit[selectedGapUnit.value],
        selectedData.value
          ? [
              selectedData.value[0],
              selectedData.value[selectedData.value.length - 1],
            ]
          : undefined
      )

    const selection = [...new Set(gaps.flat())]

    await dispatchSelection(selection)

    isUpdating.value = false
    emit('close')
  })
}
</script>
