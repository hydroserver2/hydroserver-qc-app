<template>
  <v-card>
    <v-card-title>Edit History</v-card-title>

    <v-divider></v-divider>

    <v-card-actions>
      <v-btn variant="plain" @click="onReload">Reload Series</v-btn>
      <v-btn variant="plain">Save Changes</v-btn>
    </v-card-actions>

    <v-divider></v-divider>

    <v-card-text>
      <v-timeline side="end" hide-opposite>
        <v-timeline-item
          dot-color="blue"
          size="small"
          v-for="(entry, index) of editHistory"
          :icon="entry.icon"
          fill-dot
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

            <div class="d-flex ml-1">
              <v-btn
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
              ></v-btn>
            </div>
          </div>
        </v-timeline-item>
      </v-timeline>

      <v-empty-state
        v-if="editHistory.length === 0"
        icon="mdi-clock"
        text="Edit your data and manage your checkpoints here."
        title="Edit History"
      ></v-empty-state>
    </v-card-text>
  </v-card>
</template>

<script setup lang="ts">
import { useEChartsStore } from '@/store/echarts'
import { storeToRefs } from 'pinia'

const { editHistory, selectedSeries } = storeToRefs(useEChartsStore())
const { updateVisualization } = useEChartsStore()

const onReload = async () => {
  await selectedSeries.value.data.reload()
  selectedSeries.value.data.generateDataset()
  selectedSeries.value.data.resetHistory()
  updateVisualization()
}

const onReloadHistory = async (index: number) => {
  await selectedSeries.value.data.reloadHistory(index)
  setTimeout(() => {
    selectedSeries.value.data.generateDataset()
    updateVisualization()
  })
}
</script>

<style lang="scss" scoped>
:deep(.v-timeline-item__body) {
  width: 100%;
}
</style>
