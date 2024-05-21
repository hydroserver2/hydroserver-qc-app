<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="3"
    class="bg-navbar"
    theme="dark"
  >
    <v-container>
      <v-expansion-panels color="primary-darken-2" v-model="panelOpen">
        <v-expansion-panel title="History" elevation="3">
          <v-expansion-panel-text>
            <v-virtual-scroll :items="['Action 3', 'Action 2', 'Action 1']">
              <template v-slot:default="{ item }">
                <v-list-item
                  :key="item"
                  @click="selected = item"
                  :class="{ 'v-list-item--active': selected === item }"
                  density="compact"
                  class="my-1"
                  rounded="xl"
                >
                  <v-list-item-title>{{ item }}</v-list-item-title>
                  <v-divider />
                </v-list-item>
              </template>
            </v-virtual-scroll>
          </v-expansion-panel-text>
        </v-expansion-panel>
      </v-expansion-panels>
    </v-container>

    <v-divider />
    <v-list :items="filterPoints" density="compact"> </v-list>

    <v-divider />
    <v-list :items="editData" density="compact"> </v-list>
  </v-navigation-drawer>
</template>

<script setup lang="ts">
import { ref, watch } from 'vue'

const selected = ref('action 1')
const panelOpen = ref([0])

watch(selected, (newValue, oldValue) => {
  console.log(`Selected item changed from ${oldValue} to ${newValue}`)
})

const filterPoints = [
  { type: 'subheader', title: 'Filter points' },
  {
    title: 'Qualifying comments',
    props: {
      prependIcon: 'mdi-flag',
    },
    value: 1,
  },
  {
    title: 'Linear drift correction',
    props: {
      prependIcon: 'mdi-chart-sankey',
    },
    value: 2,
  },
  {
    title: 'Interpolate',
    props: {
      prependIcon: 'mdi-chart-bell-curve-cumulative',
    },
    value: 3,
  },
  {
    title: 'Change values',
    props: {
      prependIcon: 'mdi-pencil',
    },
    value: 4,
  },
]

const editData = [
  { type: 'subheader', title: 'Edit data' },
  {
    title: 'Qualifying Comments',
    props: {
      prependIcon: 'mdi-flag',
    },
    value: 1,
  },
  {
    title: 'Linear Drift Correction',
    props: {
      prependIcon: 'mdi-chart-sankey',
    },
    value: 2,
  },
  {
    title: 'Interpolate',
    props: {
      prependIcon: 'mdi-chart-bell-curve-cumulative',
    },
    value: 3,
  },
  {
    title: 'Change Values',
    icon: 'mdi-pencil',
    props: {
      prependIcon: 'mdi-pencil',
    },
    value: 4,
  },
]
</script>
