<template>
  <v-card>
    <v-card-title>Delete Points</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        <b class="text-red">{{ selectedIndex.length }}</b> Data Point{{
          selectedIndex.length === 1 ? '' : 's'
        }}
        selected
      </div>
    </v-card-subtitle>

    <v-divider></v-divider>

    <v-card-text>
      <p class="text-body-1">
        Are you sure you want to delete
        <b class="text-red">{{ selectedIndex.length }}</b> selected Data Point{{
          selectedIndex.length !== 1 ? 's' : ''
        }}?
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
import { EnumEditOperations } from '@/utils/plotting/observationRecord'
import { useDataSelection } from '@/composables/useDataSelection'
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { updateVisualizationData } = useEChartsStore()

const { selectedData } = storeToRefs(useDataVisStore())
const { selectedIndex } = useDataSelection()

const emit = defineEmits(['close'])

const onDeleteDataPoints = async () => {
  if (!selectedIndex.value.length) {
    return
  }

  await selectedSeries.value.data.dispatch(
    EnumEditOperations.DELETE_POINTS,
    selectedIndex.value
  )
  brushSelections.value = []
  selectedData.value = {}
  updateVisualizationData()

  emit('close')
}
</script>
