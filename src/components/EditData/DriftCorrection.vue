<template>
  <v-card>
    <v-card-title>Drift Correction</v-card-title>

    <v-divider></v-divider>

    <v-card-text>
      <v-alert
        v-if="selectedGroups.length == 0"
        type="warning"
        density="compact"
        variant="outlined"
        class="text-body-2 mb-2"
      >
        You have not selected any groups of consecutive points.
      </v-alert>
      <v-alert
        v-if="selectedGroups.length > 1"
        type="info"
        density="compact"
        variant="outlined"
        class="text-body-2 mb-2"
      >
        You have selected <b>{{ selectedGroups.length }}</b> groups of
        consecutive points. Drift correction will be applied to each group.
      </v-alert>

      <v-card class="timeline-container my-6" variant="outlined" border="thin">
        <v-card-text>
          <v-timeline direction="horizontal" align="center" side="end">
            <v-timeline-item
              v-for="group of selectedGroups"
              dot-color="green"
              size="x-small"
              fill-dot
            >
              <template v-slot:icon>
                <v-icon v-tooltip="getDotTooltip(group)"
                  >mdi-dots-horizontal</v-icon
                >
              </template>
              <v-label>{{ group.length }} Points</v-label>
            </v-timeline-item>
          </v-timeline>
        </v-card-text>
      </v-card>

      <v-text-field
        label="Drift"
        type="number"
        class="mt-2"
        step="0.1"
        v-model="driftGapWidth"
      >
      </v-text-field>

      <v-radio-group
        hide-details
        color="primary"
        v-model="selectedDriftCorrectionMethod"
      >
        <v-radio
          label="Linear Drift Correction"
          :value="DriftCorrectionMethods.LINEAR"
        ></v-radio>
      </v-radio-group>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn :disabled="selectedGroups.length == 0" @click="onDriftCorrection"
        >Apply Drift Correction</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { EnumEditOperations } from '@/utils/plotting/observationRecordV2'
import { computed } from 'vue'
import { formatDate } from '@/utils/formatDate'
import { usePlotlyStore } from '@/store/plotly'
import { useDataSelection } from '@/composables/useDataSelection'
import { useUIStore, DriftCorrectionMethods } from '@/store/userInterface'
const { clearSelected } = useDataSelection()
const { selectedSeries, plotlyRef, isUpdating } = storeToRefs(usePlotlyStore())
const { driftGapWidth, selectedDriftCorrectionMethod } =
  storeToRefs(useUIStore())
const { redraw } = usePlotlyStore()

const { selectedData } = storeToRefs(useDataVisStore())
const emit = defineEmits(['close'])

const selectedGroups = computed((): number[][] => {
  if (!selectedData.value?.length) {
    return []
  }

  let groups: number[][] = [[]]

  selectedData.value.reduce((acc: number[][], curr) => {
    const target: number[] = acc[acc.length - 1]

    if (!target.length || curr == target[target.length - 1] + 1) {
      target.push(curr)
    } else {
      acc.push([curr])
    }

    return acc
  }, groups)

  return groups.filter((g) => g.length > 1)
})

const onDriftCorrection = async () => {
  const actions: [EnumEditOperations, ...any][] = []

  isUpdating.value = true

  setTimeout(async () => {
    selectedGroups.value?.forEach(async (g) => {
      const start = g[0]
      const end = g[g.length - 1]
      actions.push([
        EnumEditOperations.DRIFT_CORRECTION,
        start,
        end,
        +driftGapWidth.value,
      ])
    })

    await selectedSeries.value.data.dispatch(actions)
    await clearSelected()
    isUpdating.value = false
    await redraw()
    emit('close')
  })
}

const getDotTooltip = (group: number[]) => {
  const xData = plotlyRef.value?.data[0].x
  const start = formatDate(new Date(xData[group[0]]))
  return `${group.length} Points starting at ${start}`
}
</script>

<style scoped lang="scss">
.timeline-container {
  max-width: 100%;
  overflow-x: auto;
}
</style>
