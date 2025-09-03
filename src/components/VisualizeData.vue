<template>
  <div
    v-if="currentView === DrawerType.Select"
    class="fill-height pa-4 d-flex flex-column"
  >
    <div class="d-flex gap-1" style="min-height: 40vh">
      <v-expansion-panels v-model="panels" @update:model-value="onExpand">
        <v-expansion-panel
          title="Data visualization"
          class="d-flex flex-column"
        >
          <v-divider></v-divider>
          <div class="flex-grow-1">
            <DataVisualization />
          </div>
        </v-expansion-panel>
      </v-expansion-panels>

      <v-expansion-panels
        v-if="plottedDatastreams.length"
        id="plotted-panels"
        v-model="panels"
      >
        <v-expansion-panel
          :title="`Plotted Datastreams (${plottedDatastreams.length}/5)`"
        >
          <v-divider></v-divider>
          <v-expansion-panel-text>
            <PlottedDatastreams></PlottedDatastreams>
          </v-expansion-panel-text>
        </v-expansion-panel>
      </v-expansion-panels>
    </div>

    <DataVisDatasetsTable class="flex-grow-1 mt-4" />
  </div>

  <v-row
    v-else-if="currentView === DrawerType.Edit"
    class="fill-height ma-0 gap-1 pa-4"
  >
    <v-col cols="12" md="2" class="pa-0">
      <v-card class="fill-height">
        <EditDrawer />
      </v-card>
    </v-col>
    <v-col class="pa-0">
      <v-card class="fill-height" min-height="40rem">
        <div class="fill-height">
          <DataVisualization />
        </div>
      </v-card>
    </v-col>
    <v-col cols="12" md="3" class="pa-0 d-flex flex-column gap-1">
      <v-card>
        <v-card-title class="text-body-1">Plotted Datastreams</v-card-title>

        <v-divider></v-divider>
        <PlottedDatastreams></PlottedDatastreams>
      </v-card>
      <EditHistory />
    </v-col>
  </v-row>
</template>

<script setup lang="ts">
import DataVisDatasetsTable from '@/components/VisualizeData/DataVisDatasetsTable.vue'
import DataVisualization from '@/components/VisualizeData/DataVisualization.vue'
import EditHistory from '@/components/EditData/EditHistory.vue'
import EditDrawer from '@/components/Navigation/EditDrawer.vue'

import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'
import { useUIStore, DrawerType } from '@/store/userInterface'
import { onUnmounted, ref } from 'vue'
import PlottedDatastreams from './VisualizeData/PlottedDatastreams.vue'

const panels = ref(0)

const { resetState } = useDataVisStore()
const { plottedDatastreams } = storeToRefs(useDataVisStore())
const { currentView } = storeToRefs(useUIStore())

onUnmounted(() => {
  resetState()
})

const onExpand = () => {
  // window.dispatchEvent(new Event('resize'))
}
</script>

<style scoped>
#plotted-panels {
  flex-basis: 40rem;

  :deep(.v-expansion-panel-text__wrapper) {
    padding: 0;
  }
}
</style>
