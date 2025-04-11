<template>
  <div
    v-if="currentView === DrawerType.Select"
    class="fill-height pa-4 d-flex flex-column"
  >
    <v-expansion-panels v-model="panels" @update:model-value="onExpand">
      <v-expansion-panel title="Data visualization">
        <v-divider></v-divider>
        <v-expansion-panel-text>
          <DataVisualization />
        </v-expansion-panel-text>
      </v-expansion-panel>
    </v-expansion-panels>

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
        <v-card-text class="fill-height">
          <DataVisualization />
        </v-card-text>
      </v-card>
    </v-col>
    <v-col cols="12" md="2" class="pa-0">
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

const panels = ref(0)

const { resetState } = useDataVisStore()

const { currentView } = storeToRefs(useUIStore())

onUnmounted(() => {
  resetState()
})

const onExpand = () => {
  // window.dispatchEvent(new Event('resize'))
}
</script>

<style scoped></style>
