<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="3"
    class="bg-navbar"
    theme="dark"
  >
    <!-- <v-container>
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
    </v-container> -->

    <v-divider />

    <v-list density="compact">
      <v-list-subheader>Filter points</v-list-subheader>

      <v-list-item
        v-for="(item, i) in filterPoints"
        :key="i"
        @click="item.clickAction"
      >
        <template v-slot:prepend>
          <v-icon :icon="item.props.prependIcon"></v-icon>
        </template>
        <v-list-item-title>{{ item.title }}</v-list-item-title>
      </v-list-item>
    </v-list>

    <v-divider />

    <v-list :items="editData" density="compact">
      <v-list-subheader>Edit Data</v-list-subheader>

      <v-list-item
        v-for="(item, i) in editData"
        :key="i"
        @click="item.clickAction"
      >
        <template v-slot:prepend>
          <v-icon :icon="item.props.prependIcon"></v-icon>
        </template>
        <v-list-item-title>{{ item.title }}</v-list-item-title>
      </v-list-item>
    </v-list>
  </v-navigation-drawer>

  <!-- <v-dialog v-model="openVT" max-width="500">
    <ValueThresholdsCard @close="openVT = false" />
  </v-dialog> -->

  <v-dialog v-model="openRateOfChange" max-width="500">
    <RateOfChangeCard @close="openRateOfChange = false" />
  </v-dialog>

  <v-dialog v-model="openPersistence" max-width="500">
    <PersistenceCard @close="openPersistence = false" />
  </v-dialog>
</template>

<script setup lang="ts">
// import ValueThresholdsCard from '@/components/FilterPoints/ValueThresholdsCard.vue'
import RateOfChangeCard from '@/components/FilterPoints/RateOfChangeCard.vue'
import PersistenceCard from '@/components/FilterPoints/PersistenceCard.vue'
import { usePyStore } from '@/store/py'
import { ref, watch } from 'vue'

const selected = ref('action 1')
const panelOpen = ref([0])
const py = usePyStore()

watch(selected, (newValue, oldValue) => {
  console.log(`Selected item changed from ${oldValue} to ${newValue}`)
})

// const openVT = ref(false)
const openRateOfChange = ref(false)
const openGaps = ref(false)
const openPersistence = ref(false)

const filterPoints = [
  // {
  //   title: 'Value thresholds',
  //   props: {
  //     prependIcon: 'mdi-align-vertical-center',
  //   },
  //   value: 1,
  //   clickAction: () => {
  //     openVT.value = true
  //   },
  // },
  {
    title: 'Rate of change',
    props: {
      prependIcon: 'mdi-delta',
    },
    value: 2,
    clickAction: () => {
      openRateOfChange.value = true
    },
  },
  {
    title: 'Select gaps',
    props: {
      prependIcon: 'mdi-keyboard-space',
    },
    value: 3,
    clickAction: () => {
      openGaps.value = true
    },
  },
  {
    title: 'Persistence',
    props: {
      prependIcon: 'mdi-dots-horizontal',
    },
    value: 4,
    clickAction: () => {
      openPersistence.value = true
    },
  },
]

const editData = [
  {
    title: 'Qualifying comments',
    props: {
      prependIcon: 'mdi-flag',
    },
    value: 1,
    clickAction: () => {},
  },
  {
    title: 'Linear drift correction',
    props: {
      prependIcon: 'mdi-chart-sankey',
    },
    value: 2,
    clickAction: () => {},
  },
  {
    title: 'Interpolate',
    props: {
      prependIcon: 'mdi-transit-connection-horizontal',
    },
    value: 3,
    clickAction: () => {},
  },
  {
    title: 'Change values',
    props: {
      prependIcon: 'mdi-pencil',
    },
    value: 4,
    clickAction: () => {
      py.changeValues([0])
    },
  },
  {
    title: 'Delete points',
    props: {
      prependIcon: 'mdi-trash-can',
    },
    value: 5,
    clickAction: () => {},
  },
  {
    title: 'Add points',
    props: {
      prependIcon: 'mdi-plus',
    },
    value: 6,
    clickAction: () => {},
  },
]
</script>
