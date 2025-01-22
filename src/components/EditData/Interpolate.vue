<template>
  <v-card>
    <v-card-title>Interpolate</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        {{ selectedData?.points.length }} Data Point{{
          selectedData?.points.length === 1 ? '' : 's'
        }}
        selected
      </div>
    </v-card-subtitle>

    <v-divider></v-divider>

    <v-card-text>
      <div class="d-flex gap-1">
        <v-radio-group
          hide-details
          color="primary"
          v-model="selectedInterpolationMethod"
        >
          <v-radio
            label="Linear Interpolation"
            :value="InterpolationMethods.LINEAR"
          ></v-radio>
        </v-radio-group>
      </div>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onInterpolate"
        >Interpolate</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { InterpolationMethods, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

import { EnumEditOperations } from '@/utils/plotting/observationRecord'
const { selectedData } = storeToRefs(useDataVisStore())
// const { selectedIndex } = useDataSelection()
const { selectedSeries } = storeToRefs(usePlotlyStore())
import { usePlotlyStore } from '@/store/plotly'
const { updateVisualizationData } = usePlotlyStore()

const { selectedInterpolationMethod } = storeToRefs(usePyStore())

const emit = defineEmits(['close'])

const onInterpolate = async () => {
  if (!selectedData.value?.points.length) {
    return
  }
  // TODO: value error when interpolating values lesser than 1
  await selectedSeries.value.data.dispatch(
    EnumEditOperations.INTERPOLATE,
    selectedData.value.points.map((p) => p.pointIndex)
  )

  // brushSelections.value = []
  // selectedData.value = {}
  updateVisualizationData()
  emit('close')
}
</script>
