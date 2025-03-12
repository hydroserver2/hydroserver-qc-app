<template>
  <v-card>
    <v-card-title>Edit History</v-card-title>

    <v-divider></v-divider>

    <v-card-actions>
      <v-btn variant="plain">Save Changes</v-btn>
    </v-card-actions>

    <v-divider></v-divider>

    <v-card-text>
      <v-empty-state
        v-if="editHistory.length === 0"
        icon="mdi-clock"
        text="Edit your data and manage your checkpoints here."
        title="Edit History"
      />
      <v-timeline v-else side="end" hide-opposite density="compact">
        <v-timeline-item dot-color="green" fill-dot size="small">
          <div class="d-flex align-center">
            <span class="text-body-1 mr-2">Start</span>
            <v-btn
              icon="mdi-reload"
              color="blue"
              variant="plain"
              title="Reload at this stage"
              @click="onReload"
            ></v-btn>
          </div>
        </v-timeline-item>
        <v-timeline-item
          v-for="(entry, index) of editHistory"
          :size="index < editHistory.length - 1 ? 'small' : 'large'"
          :icon="entry.icon"
          :fill-dot="index < editHistory.length - 1"
          dot-color="blue"
          elevation="4"
        >
          <div class="d-flex align-center">
            <v-expansion-panels>
              <v-expansion-panel>
                <v-expansion-panel-title color="grey-lighten-4">{{
                  entry.method
                }}</v-expansion-panel-title>
                <v-expansion-panel-text>
                  <div class="text-caption mb-2">Arguments:</div>
                  <ul class="text-caption px-2">
                    <template v-for="arg of entry.args">
                      <li>{{ arg }}</li>
                    </template>
                  </ul>
                </v-expansion-panel-text>
              </v-expansion-panel>
            </v-expansion-panels>

            <div class="d-flex ml-2 justify-end" style="min-width: 5rem">
              <v-btn
                v-if="index < editHistory.length - 1"
                icon="mdi-reload"
                color="blue"
                variant="plain"
                title="Reload at this stage"
                @click="onReloadHistory(index)"
              ></v-btn>
              <v-btn
                icon="mdi-close"
                color="red"
                variant="plain"
                title="Undo"
                @click="onRemoveHistoryItem(index)"
              ></v-btn>
            </div>
          </div>
        </v-timeline-item>
      </v-timeline>
    </v-card-text>
  </v-card>
</template>

<script setup lang="ts">
import { storeToRefs } from 'pinia'

const { editHistory, selectedSeries } = storeToRefs(usePlotlyStore())
import { usePlotlyStore } from '@/store/plotly'
const { createVisualization, redraw, updateOptions } = usePlotlyStore()

const onReload = async () => {
  await selectedSeries.value.data.reload()
  // selectedSeries.value.data.generateDataset()
  editHistory.value = []
  redraw()
}

const onReloadHistory = async (index: number) => {
  if (index < editHistory.value.length - 1) {
    await selectedSeries.value.data.reloadHistory(index)
    redraw()
  }
}

const onRemoveHistoryItem = async (index: number) => {
  console.log('onRemoveHistoryItem')
  await selectedSeries.value.data.removeHistoryItem(index)
  // Undoing actions will reload the data from scratch, so we must recreate the chart
  updateOptions() // Updates options

  await createVisualization()
}

// TODO: ADD CONFIRMATION DIALOGS TO HISTORY OPERATIONS
</script>

<style lang="scss" scoped>
:deep(.v-timeline-item__body) {
  width: 100%;
}
</style>
