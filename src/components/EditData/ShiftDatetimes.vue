<template>
  <v-card>
    <v-card-title>Shift Datetimes</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        {{ selectedIndex.length }} Data Point{{
          selectedIndex.length === 1 ? '' : 's'
        }}
        selected
      </div>
    </v-card-subtitle>

    <v-divider></v-divider>

    <v-card-text>
      <v-row>
        <v-col>
          <v-select
            label="Time Unit"
            :items="shiftUnits"
            v-model="selectedShiftUnit"
          />
        </v-col>
        <v-col>
          <v-text-field label="Amount" type="number" v-model="shiftAmount" />
        </v-col>
      </v-row>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onShiftDatetimes"
        >Shift Datetimes</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { TimeUnit, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumEditOperations } from '@/utils/plotting/observationRecord'

import { useEChartsStore } from '@/store/echarts'
import { useDataSelection } from '@/composables/useDataSelection'
const { selectedData } = storeToRefs(useDataVisStore())
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { updateVisualizationData } = useEChartsStore()

const { shiftUnits } = usePyStore()
const { selectedShiftUnit, shiftAmount } = storeToRefs(usePyStore())
const { selectedIndex } = useDataSelection()

const emit = defineEmits(['close'])

const onShiftDatetimes = async () => {
  if (!selectedIndex.value.length) {
    return
  }

  await selectedSeries.value.data.dispatch(
    EnumEditOperations.SHIFT_DATETIMES,
    selectedIndex.value,
    shiftAmount.value,
    // @ts-ignore
    TimeUnit[selectedShiftUnit.value]
  )
  brushSelections.value = []
  selectedData.value = {}
  updateVisualizationData()

  emit('close')
}
</script>
