<template>
  <v-card rounded="xl" min-height="300">
    <v-card-title class="text-body-1">
      Filter by value thresholds
      <v-badge
        v-if="Object.keys(appliedFilters).length"
        :content="Object.keys(appliedFilters).length"
        inline
        color="blue"
      ></v-badge>
    </v-card-title>
    <v-divider></v-divider>
    <template v-if="Object.keys(appliedFilters).length">
      <v-card-text class="d-flex gap-1">
        <div class="d-flex gap-1">
          <v-chip
            border="double blue"
            variant="outlined"
            closable
            color="blue"
            v-for="key of Object.keys(appliedFilters)"
            :key="key"
            @click:close="removeFilter(key)"
            >{{ key }}: {{ appliedFilters[key] }}</v-chip
          >
        </div>
        <v-spacer></v-spacer>
        <v-btn @click="clearFilters" variant="flat">Clear</v-btn>
      </v-card-text>

      <v-divider></v-divider>
    </template>

    <v-card-text>
      <div class="d-flex gap-1">
        <v-select
          label="Operation"
          :items="filterOperators"
          v-model="selectedFilter"
        ></v-select>
        <v-text-field
          label="Value"
          v-model="filterValue"
          step="0.1"
          type="number"
          width="30"
        >
        </v-text-field>
        <v-btn
          color="blue-grey-lighten-1"
          @click="onAddFilter(selectedFilter, filterValue)"
          prepend-icon="mdi-plus"
          >Add Filter</v-btn
        >
      </div>
    </v-card-text>
  </v-card>
</template>

<script setup lang="ts">
import { Ref, ref } from 'vue'
import { FilterOperation } from '@/store/py'
import { useEChartsStore } from '@/store/echarts'
import { storeToRefs } from 'pinia'
import { EnumFilterOperations } from '@/utils/plotting/observationRecord'
import { useDataSelection } from '@/composables/useDataSelection'
const { updateVisualizationData } = useEChartsStore()
const { selectedSeries } = storeToRefs(useEChartsStore())
const { applySelection } = useDataSelection()

const emit = defineEmits(['filter', 'close'])

// TODO: move these to store

// FILTERS
const filterOperators = [...Object.keys(FilterOperation)]
const selectedFilter = ref(filterOperators[2])
const filterValue = ref(0)
const appliedFilters: Ref<{ [key: string]: number }> = ref({})

const clearFilters = async () => {
  appliedFilters.value = {}
  const selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.VALUE_THRESHOLD,
    appliedFilters.value
  )
  applySelection(selection)
}

const onAddFilter = (key: string, value: number) => {
  addFilter(key, value)
  updateVisualizationData()
}

const addFilter = async (key: string, value: number) => {
  appliedFilters.value[key] = +value
  const selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.VALUE_THRESHOLD,
    appliedFilters.value
  )
  applySelection(selection)
}

const removeFilter = async (key: string) => {
  delete appliedFilters.value[key]
  const selection = await selectedSeries.value.data.dispatchFilter(
    EnumFilterOperations.VALUE_THRESHOLD,
    appliedFilters.value
  )
  applySelection(selection)
}
</script>
