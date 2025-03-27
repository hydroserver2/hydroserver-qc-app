<template>
  <v-card>
    <v-card-title class="text-body-1">Find persistent values</v-card-title>

    <v-divider></v-divider>

    <v-card-text>
      <v-timeline
        v-if="selectedData?.length"
        direction="horizontal"
        align="center"
        side="start"
        hide-opposite
      >
        <v-timeline-item dot-color="green" size="small">
          <div class="text-center">
            <div class="text-caption">From:</div>
            <strong>{{ startDateString }}</strong>
          </div>
        </v-timeline-item>

        <v-timeline-item dot-color="green" size="small">
          <div class="text-center">
            <div class="text-caption">To:</div>
            <strong>{{ endDateString }}</strong>
          </div>
        </v-timeline-item>
      </v-timeline>

      Select points where the values are the same at least:

      <v-text-field
        class="mt-2"
        v-model.number="times"
        type="number"
        max-width="15rem"
        suffix="times in a row"
        min="2"
      />
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn @click="onPersistence">Filter</v-btn>
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumFilterOperations } from '@/utils/plotting/observationRecordV2'
import { useDataSelection } from '@/composables/useDataSelection'
import { usePlotlyStore } from '@/store/plotly'

const { selectedSeries, isUpdating } = storeToRefs(usePlotlyStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { dispatchSelection, startDateString, endDateString } = useDataSelection()

const emit = defineEmits(['close'])
const times = ref(2)
const onPersistence = async () => {
  isUpdating.value = true

  setTimeout(async () => {
    const selection = await selectedSeries.value.data.dispatchFilter(
      EnumFilterOperations.PERSISTENCE,
      times.value,
      selectedData.value
        ? [
            selectedData.value[0],
            selectedData.value[selectedData.value.length - 1],
          ]
        : undefined
    )

    await dispatchSelection(selection)

    isUpdating.value = false
    emit('close')
  })
}
</script>
