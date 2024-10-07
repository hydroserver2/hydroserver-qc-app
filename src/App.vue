<template>
  <v-app>
    <v-main>
      <router-view />
    </v-main>

    <Notifications />
    <link
      href="https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900"
      rel="stylesheet"
    />
  </v-app>
</template>

<script setup lang="ts">
import Notifications from '@/components/base/Notifications.vue'
import { setupRouteGuards } from '@/router/router'
import { usePyStore } from '@/store/py'
import { ref } from 'vue'
const initialized = ref(false)

// Use stores
const py = usePyStore()

const initializedSub = py.$initialized.subscribe(() => {
  initialized.value = true
  initializedSub.unsubscribe()
})

setupRouteGuards()
</script>
