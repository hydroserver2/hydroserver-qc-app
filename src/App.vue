<template>
  <v-app>
    <v-main>
      <FullScreenLoader v-if="isLoading" />
      <router-view v-else />
    </v-main>

    <Notifications />
  </v-app>
</template>

<script setup lang="ts">
import Notifications from '@/components/base/Notifications.vue'
import FullScreenLoader from '@/components/base/FullScreenLoader.vue'

import { setupRouteGuards } from '@/router/router'
import { api } from '@/services/api'
import { ref } from 'vue'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'

// Use stores
const isLoading = ref(true)

const { things, processingLevels, observedProperties, datastreams } =
  storeToRefs(useDataVisStore())

const initializeHydroServer = async () => {
  const [
    thingsResponse,
    datastreamsResponse,
    processingLevelsResponse,
    observedPropertiesResponse,
  ] = await Promise.all([
    api.fetchThings(),
    api.fetchDatastreams(),
    api.fetchProcessingLevels(),
    api.fetchObservedProperties(),
  ])

  things.value = thingsResponse
  datastreams.value = datastreamsResponse
  processingLevels.value = processingLevelsResponse
  observedProperties.value = observedPropertiesResponse

  isLoading.value = false
}

initializeHydroServer()
// TODO: use route guard setup in Router v3
setupRouteGuards()
</script>
