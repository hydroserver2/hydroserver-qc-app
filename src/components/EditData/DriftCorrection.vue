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

const { selectedData } = storeToRefs(useDataVisStore())
const { driftGapWidth, selectedDriftCorrectionMethod } = storeToRefs(
  usePyStore()
)
const { driftCorrection } = usePyStore()
const emit = defineEmits(['close'])

const onDriftCorrection = () => {
  driftCorrection()
  emit('close')
}
</script>
