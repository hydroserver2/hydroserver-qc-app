<template>
  <v-card>
    <v-card-title>Drift Correction</v-card-title>
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
      <v-radio-group
        hide-details
        color="primary"
        v-model="selectedDriftCorrectionMethod"
      >
        <v-radio
          label="Linear Drift Correction"
          :value="DriftCorrectionMethods.LINEAR"
        ></v-radio>
      </v-radio-group>

      <v-text-field
        label="Drift"
        type="number"
        class="mt-2"
        step="0.1"
        v-model="driftGapWidth"
      >
      </v-text-field>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onDriftCorrection"
        >Apply Drift Correction</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { DriftCorrectionMethods, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

const { driftGapWidth, selectedDriftCorrectionMethod } = storeToRefs(
  usePyStore()
)

import { EnumEditOperations } from '@/types'
import { useEChartsStore } from '@/store/echarts'
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { updateVisualization } = useEChartsStore()

const { selectedData } = storeToRefs(useDataVisStore())
const emit = defineEmits(['close'])

const onDriftCorrection = () => {
  if (!selectedData.value.length) {
    return
  }

  const index = selectedData.value.map(
    (point: { date: Date; value: number; index: number }) =>
      selectedSeries.value.data.dataFrame.get_index_at(point.index)
  )

  setTimeout(() => {
    let groups: number[][] = [[]]
    const sorted = index.sort((a, b) => a - b)

    sorted.reduce((acc: number[][], curr: number) => {
      const target: number[] = acc[acc.length - 1]

      if (!target.length || curr == target[target.length - 1] + 1) {
        target.push(curr)
      } else {
        acc.push([curr])
      }

      return acc
    }, groups)

    groups = groups.filter((g) => g.length > 1)

    groups.forEach((g) => {
      const start = g[0]
      const end = g[g.length - 1]
      selectedSeries.value.data.dispatch(
        EnumEditOperations.DRIFT_CORRECTION,
        start,
        end,
        +driftGapWidth.value
      )
    })

    brushSelections.value = []
    selectedData.value = []
    updateVisualization()
  }, 0)
  emit('close')
}
</script>
