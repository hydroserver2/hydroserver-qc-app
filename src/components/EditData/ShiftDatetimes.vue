<template>
  <v-card>
    <v-card-title>Shift Datetimes</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        {{ selectedData.length }} Data Point{{
          selectedData.length === 1 ? '' : 's'
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
import { usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumEditOperations } from '@/types'

import { useEChartsStore } from '@/store/echarts'
const { selectedData } = storeToRefs(useDataVisStore())
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { updateVisualization } = useEChartsStore()

const { shiftUnits } = usePyStore()
const { selectedShiftUnit, shiftAmount } = storeToRefs(usePyStore())

const emit = defineEmits(['close'])

const onShiftDatetimes = () => {
  if (!selectedData.value.length) {
    return
  }

  const index = selectedData.value.map(
    (point: { date: Date; value: number; index: number }) =>
      selectedSeries.value.data.dataFrame.get_index_at(point.index)
  )

  setTimeout(() => {
    selectedSeries.value.data.dispatch(
      EnumEditOperations.SHIFT_DATETIMES,
      index,
      shiftAmount.value,
      // @ts-ignore
      TimeUnit[selectedShiftUnit.value]
    )
    brushSelections.value = []
    selectedData.value = []
    updateVisualization()
  })

  emit('close')
}
</script>
