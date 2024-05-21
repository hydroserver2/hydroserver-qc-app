<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="1"
    class="bg-navbar"
    theme="dark"
  >
    <v-list class="pb-2" density="compact">
      <template v-for="item in viewItems">
        <v-list-subheader v-if="item.type === 'subheader'">
          {{ item.title }}
        </v-list-subheader>

        <v-list-item
          v-else
          @click="item.active = !item.active"
          :prepend-icon="item.active ? item.activeIcon : item.inactiveIcon"
          :title="item.title"
        />
      </template>
    </v-list>

    <v-list class="py-0">
      <v-list-item>
        <v-select density="compact" label="Qualifying comments"></v-select>
      </v-list-item>
    </v-list>

    <v-divider />
    <v-list class="pb-6">
      <v-list-subheader>Time filters</v-list-subheader>
      <v-list-item>
        <DataVisTimeFilters />
      </v-list-item>
    </v-list>

    <v-divider />
    <v-list class="pb-0">
      <v-list-subheader>Datastream filters</v-list-subheader>
    </v-list>
    <DatastreamFilters />

    <v-divider />
  </v-navigation-drawer>
</template>

<script setup lang="ts">
import DataVisTimeFilters from '@/components/VisualizeData/DataVisTimeFilters.vue'
import DatastreamFilters from '@/components/VisualizeData/DatastreamFilters.vue'
import { ref } from 'vue'

const viewItems = ref([
  { type: 'subheader', title: 'View' },
  {
    title: 'Show legend',
    activeIcon: 'mdi-map',
    inactiveIcon: 'mdi-map-outline',
    active: false,
  },
  {
    title: 'Show tooltip',
    activeIcon: 'mdi-comment',
    inactiveIcon: 'mdi-comment-outline',
    active: false,
  },
])
</script>
