<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="1"
    class="bg-navbar"
    theme="dark"
  >
    <!-- <DataVisualizationControls /> -->
    <v-list class="pb-2" density="compact">
      <v-list-subheader> View </v-list-subheader>
      <v-list-item
        @click="showLegend = !showLegend"
        :prepend-icon="showLegend ? 'mdi-map' : 'mdi-map-outline'"
        title="Show legend"
      />

      <v-list-item
        @click="showTooltip = !showTooltip"
        :prepend-icon="showTooltip ? 'mdi-comment' : 'mdi-comment-outline'"
        title="Show tooltip"
      />

      <v-list-item class="pb-0">
        <v-select
          :disabled="!qualifierOptions.length"
          :items="qualifierOptions"
          density="compact"
          label="Qualifying comments"
          v-model="selectedQualifier"
        />
      </v-list-item>
    </v-list>

    <v-divider />

    <v-list class="pb-6">
      <v-list-subheader>Time filters</v-list-subheader>
      <v-list-item>
        <DataVisTimeFilters />
      </v-list-item>
    </v-list>

    <v-divider />

    <v-list class="pb-0">
      <v-list-subheader>Datastream filters</v-list-subheader>
    </v-list>
    <DatastreamFilters />
  </v-navigation-drawer>
</template>

<script setup lang="ts">
import DataVisTimeFilters from '@/components/VisualizeData/DataVisTimeFilters.vue'
import DatastreamFilters from '@/components/VisualizeData/DatastreamFilters.vue'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'
import { computed, watch } from 'vue'
import DataVisualizationControls from '../VisualizeData/DataVisualizationControls.vue'
import { usePlotlyStore } from '@/store/plotly'

const { showLegend, showTooltip } = storeToRefs(usePlotlyStore())
const { qualifierSet, selectedQualifier } = storeToRefs(useDataVisStore())

const qualifierOptions = computed(() => {
  return qualifierSet.value.size
    ? ['All', ...Array.from(qualifierSet.value)]
    : []
})

watch(qualifierOptions, (newVal) => {
  selectedQualifier.value = newVal.length ? 'All' : ''
})
</script>
