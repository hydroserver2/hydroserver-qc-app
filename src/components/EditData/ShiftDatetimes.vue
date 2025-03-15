<template>
  <v-card>
    <v-card-title>Shift Datetimes</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        {{ selectedData?.length }} Data Point{{
          selectedData?.length === 1 ? '' : 's'
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
import { usePlotlyStore } from '@/store/plotly'

const { selectedData } = storeToRefs(useDataVisStore())
const { selectedSeries } = storeToRefs(usePlotlyStore())
const { selectedShiftUnit, shiftAmount } = storeToRefs(usePyStore())
const { redraw } = usePlotlyStore()
const { shiftUnits } = usePyStore()

const emit = defineEmits(['close'])

const onShiftDatetimes = async () => {
  if (!selectedData.value?.length) {
    return
  }

  await selectedSeries.value.data.dispatch(
    EnumEditOperations.SHIFT_DATETIMES,
    selectedData.value,
    +shiftAmount.value,
    // @ts-ignore
    TimeUnit[selectedShiftUnit.value]
  )

  redraw()

  emit('close')
}
</script>
