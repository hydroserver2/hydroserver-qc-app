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

setupRouteGuards()
</script>
