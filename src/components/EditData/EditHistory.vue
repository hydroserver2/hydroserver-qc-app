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
        text="Edit your data and manage your changes here."
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
              :disabled="isUpdating"
              @click="onReload"
            ></v-btn>
          </div>
        </v-timeline-item>
        <v-timeline-item
          v-for="(entry, index) of editHistory"
          :key="index"
          :size="index < editHistory.length - 1 ? 'small' : 'large'"
          :icon="entry.icon"
          :fill-dot="index < editHistory.length - 1"
          dot-color="blue"
          elevation="4"
        >
          <div class="d-flex align-center">
            <v-expansion-panels>
              <v-expansion-panel>
                <v-expansion-panel-title color="grey-lighten-4">
                  {{ entry.method }}</v-expansion-panel-title
                >
                <v-expansion-panel-text>
                  <div class="text-caption mb-2">Arguments:</div>
                  <ul class="text-caption px-2">
                    <code>
                      <li v-for="(arg, index) of entry.args" :key="index">
                        {{ arg }}
                      </li>
                    </code>
                  </ul>
                </v-expansion-panel-text>
              </v-expansion-panel>
            </v-expansion-panels>

            <div
              class="d-flex ml-2 justify-end align-center"
              style="min-width: 5rem"
            >
              <v-progress-circular
                v-if="entry.isLoading"
                size="20"
                color="primary"
                indeterminate
              />
              <v-btn
                v-else
                icon="mdi-reload"
                color="blue"
                variant="plain"
                :disabled="isUpdating || entry.isLoading"
                title="Reload at this stage"
                @click="onReloadHistory(index)"
              ></v-btn>
              <v-btn
                icon="mdi-close"
                color="red"
                variant="plain"
                title="Undo"
                :disabled="isUpdating"
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
import { usePlotlyStore } from '@/store/plotly'
import { useDataSelection } from '@/composables/useDataSelection'

const { editHistory, selectedSeries, isUpdating } =
  storeToRefs(usePlotlyStore())
const { redraw } = usePlotlyStore()
const { clearSelected } = useDataSelection()

const onReload = async () => {
  isUpdating.value = true
  setTimeout(async () => {
    await selectedSeries.value.data.reload()
    editHistory.value = []
    await clearSelected()
    isUpdating.value = false
    await redraw()
  })
}

const onReloadHistory = async (index: number) => {
  if (index < editHistory.value.length) {
    isUpdating.value = true
    setTimeout(async () => {
      await selectedSeries.value.data.reloadHistory(index)
      isUpdating.value = false
      await redraw()
    })
  }
}

const onRemoveHistoryItem = async (index: number) => {
  isUpdating.value = true

  setTimeout(async () => {
    await selectedSeries.value.data.removeHistoryItem(index)
    isUpdating.value = false
    await redraw()
  })
}

// TODO: ADD CONFIRMATION DIALOGS TO HISTORY OPERATIONS
</script>

<style lang="scss" scoped>
:deep(.v-timeline-item__body) {
  width: 100%;
}

:deep(.v-timeline-item .v-expansion-panel-text) {
  max-height: 15rem;
  overflow-y: auto;
}
</style>
