<template>
  <v-navigation-drawer permanent>
    <v-expansion-panels color="primary-lighten-2" v-model="panelOpen">
      <v-expansion-panel title="History" elevation="0">
        <v-expansion-panel-text>
          <v-virtual-scroll :items="['action 3', 'action 2', 'action 1']">
            <template v-slot:default="{ item }">
              <v-list-item
                :key="item"
                @click="selected = item"
                :class="{ 'v-list-item--active': selected === item }"
                density="compact"
              >
                <v-list-item-title>{{ item }}</v-list-item-title>
              </v-list-item>
            </template>
          </v-virtual-scroll>
        </v-expansion-panel-text>
      </v-expansion-panel>
    </v-expansion-panels>

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
