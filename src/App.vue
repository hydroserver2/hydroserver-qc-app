<template>
  <v-app>
    <v-main>
      <FullScreenLoader v-if="isLoading || !hasPyScriptInitialized" />
      <router-view v-else />
    </v-main>

    <Notifications />
  </v-app>
</template>

<script setup lang="ts">
import Notifications from '@/components/base/Notifications.vue'
import FullScreenLoader from '@/components/base/FullScreenLoader.vue'

import { setupRouteGuards } from '@/router/router'
import { usePyStore } from '@/store/py'
import { api } from '@/services/api'
import { ref } from 'vue'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'
import * as echarts from 'echarts'
import dataSample from './utils/custom-down-sample'

// Use stores
const { $initialized } = usePyStore()
const isLoading = ref(true)
const hasPyScriptInitialized = ref(false)

const { things, processingLevels, observedProperties, datastreams } =
  storeToRefs(useDataVisStore())

const initializedSub = $initialized.subscribe(() => {
  hasPyScriptInitialized.value = true
  initializedSub.unsubscribe()
})

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

  // TODO: the previous dataSample processor needs to be unregistered
  echarts.registerProcessor(
    echarts.PRIORITY.PROCESSOR.STATISTIC,
    // @ts-ignore
    dataSample('line')
  )
}

initializeHydroServer()
// TODO: use route guard setup in Router v3
setupRouteGuards()
</script>
