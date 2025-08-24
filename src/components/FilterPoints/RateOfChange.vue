<template>
  <v-card rounded>
    <v-card-title class="text-body-1">Filter by rate of change</v-card-title>
    <v-divider></v-divider>

    <v-card-text>
      <v-label class="mb-4">Select points where the rate of change is</v-label>
      <v-select
        label="Comparison operator"
        :items="filterOperators"
        v-model="selectedFilter"
        return-object
      ></v-select>
      <v-text-field
        label="Rate of change"
        type="number"
        clearable
        step="0.1"
        v-model="filterValue"
      />
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn
        @click="onFilter(selectedFilter.title, filterValue)"
        :disabled="isUpdating || filterValue == null"
        >Filter</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { useDataSelection } from '@/composables/useDataSelection'
import { usePlotlyStore } from '@/store/plotly'
import { RateOfChangeOperation } from '@/store/userInterface'
import { EnumFilterOperations } from '@uwrl/qc-utils'
import { storeToRefs } from 'pinia'
import { ref } from 'vue'
const { selectedSeries, isUpdating } = storeToRefs(usePlotlyStore())
const { dispatchSelection, clearSelected } = useDataSelection()
const emit = defineEmits(['filter', 'close'])

// FILTERS
const filterOperators = [
  ...Object.keys(RateOfChangeOperation).map((key) => ({
    value: key,
    // @ts-ignore
    title: RateOfChangeOperation[key],
  })),
]
const selectedFilter = ref(filterOperators[2])
const filterValue = ref(0)

const onFilter = async (key: string, value: number) => {
  isUpdating.value = true
  setTimeout(async () => {
    await clearSelected()
    const selection = await selectedSeries.value.data.dispatchFilter(
      EnumFilterOperations.RATE_OF_CHANGE,
      key,
      +value
    )

    await dispatchSelection(selection)
    isUpdating.value = false
    emit('close')
  })
}
</script>
