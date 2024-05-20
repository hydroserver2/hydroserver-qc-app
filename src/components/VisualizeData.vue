<template>
  <FullScreenLoader v-if="loading" />
  <div v-else>
    <div class="my-4 mx-4">
      <v-expansion-panels v-model="panels">
        <v-expansion-panel
          title="Data Visualization"
          v-if="cardHeight"
          color="grey-lighten-2"
        >
          <v-expansion-panel-text>
            <DataVisualizationCard :cardHeight="cardHeight" />
          </v-expansion-panel-text>
        </v-expansion-panel>
      </v-expansion-panels>

      <v-sheet
        v-if="panels === 0"
        class="resize-handle mt-4"
        @mousedown="handleMouseDown"
        color="blue-grey-lighten-2"
        :height="3"
        :elevation="1"
        rounded="xl"
        outlined
      />
      <v-divider v-else />

      <div class="mt-1">
        <DataVisDatasetsTable />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import DataVisDatasetsTable from '@/components/VisualizeData/DataVisDatasetsTable.vue'
import DataVisualizationCard from '@/components/VisualizeData/DataVisualizationCard.vue'
import FullScreenLoader from '@/components/base/FullScreenLoader.vue'
import { api } from '@/services/api'
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'

const { resetState } = useDataVisStore()
const {
  things,
  processingLevels,
  observedProperties,
  datastreams,
  cardHeight,
  tableHeight,
} = storeToRefs(useDataVisStore())

const panels = ref(0)

watch(panels, () => {
  if (panels.value === 0)
    tableHeight.value = Math.max(70 - cardHeight.value, 16)
  else if (panels.value === undefined) tableHeight.value = Math.max(70, 16)
})

let startY = 0
let startHeight = 0

function handleMouseDown(e: MouseEvent) {
  startY = e.clientY
  startHeight = cardHeight.value
  document.addEventListener('mousemove', handleMouseMove)
  document.addEventListener('mouseup', handleMouseUp)
}

function handleMouseMove(e: MouseEvent) {
  const diffY = e.clientY - startY
  const diffVh = diffY * (100 / window.innerHeight)
  cardHeight.value = Math.max(startHeight + diffVh, 16) // Minimum height of 16vh
  tableHeight.value = Math.max(70 - cardHeight.value, 16)
}

function handleMouseUp() {
  document.removeEventListener('mousemove', handleMouseMove)
  document.removeEventListener('mouseup', handleMouseUp)
}

const loading = ref(true)

onMounted(async () => {
  const [
    thingsResponse,
    datastreamsResponse,
    processingLevelsResponse,
    observedPropertiesResponse,
  ] = await Promise.all([
    api.fetchThings(),
    api.fetchDatastreams(),
    api.fetchProcessingLevels(),
    api.fetchObservedProperties(),
  ])

  things.value = thingsResponse
  datastreams.value = datastreamsResponse
  processingLevels.value = processingLevelsResponse
  observedProperties.value = observedPropertiesResponse

  loading.value = false
})

onUnmounted(() => {
  resetState()
})
</script>

<style scoped>
.resize-handle {
  cursor: ns-resize;
}
</style>
