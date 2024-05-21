<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="1"
    class="bg-navbar"
    theme="dark"
  >
    <v-list class="pb-2" :items="filterPoints" density="compact"> </v-list>
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

const filterPoints = [
  { type: 'subheader', title: 'View' },
  {
    title: 'Show legend',
    props: {
      prependIcon: 'mdi-map',
    },
    value: 1,
  },
  {
    title: 'Show tooltip',
    props: {
      prependIcon: 'mdi-comment',
    },
    value: 2,
  },
]
</script>
