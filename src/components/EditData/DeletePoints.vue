<template>
  <v-card>
    <v-card-title>Delete Points</v-card-title>
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
      <p class="text-body-1">
        Are you sure you want to delete {{ selectedData.length }} selected Data
        Point{{ selectedData.length !== 1 ? 's' : '' }}?
      </p>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onDeleteDataPoints"
        >Delete Data Points</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { EnumEditOperations } from '@/types'
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { updateVisualization } = useEChartsStore()

const { selectedData } = storeToRefs(useDataVisStore())

const emit = defineEmits(['close'])

const onDeleteDataPoints = async () => {
  if (!selectedData.value.length) {
    return
  }

  const index = selectedData.value.map(
    (point: { date: Date; value: number; index: number }) =>
      selectedSeries.value.data.dataFrame.get_index_at(point.index)
  )

  await selectedSeries.value.data.dispatch(
    EnumEditOperations.DELETE_POINTS,
    index
  )
  brushSelections.value = []
  selectedData.value = []
  updateVisualization()

  emit('close')
}
</script>
