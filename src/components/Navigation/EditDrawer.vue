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

  <v-dialog v-model="openChangeValues" max-width="500">
    <ChangeValues @close="openChangeValues = false" />
  </v-dialog>

  <v-dialog v-model="openInterpolate" max-width="500">
    <Interpolate @close="openInterpolate = false" />
  </v-dialog>

  <v-dialog v-model="openDeletePoints" max-width="500">
    <DeletePoints @close="openDeletePoints = false" />
  </v-dialog>

  <v-dialog v-model="openDriftCorrection" max-width="500">
    <DriftCorrection @close="openDriftCorrection = false" />
  </v-dialog>
</template>

<script setup lang="ts">
// import ValueThresholdsCard from '@/components/FilterPoints/ValueThresholdsCard.vue'
import RateOfChangeCard from '@/components/FilterPoints/RateOfChangeCard.vue'
import PersistenceCard from '@/components/FilterPoints/PersistenceCard.vue'
import { ref } from 'vue'
import ChangeValues from '@/components/EditData/ChangeValues.vue'
import Interpolate from '@/components/EditData/Interpolate.vue'
import DeletePoints from '@/components/EditData/DeletePoints.vue'
import DriftCorrection from '@/components/EditData/DriftCorrection.vue'

// const openVT = ref(false)
const openRateOfChange = ref(false)
const openGaps = ref(false)
const openPersistence = ref(false)
const openChangeValues = ref(false)
const openInterpolate = ref(false)
const openDeletePoints = ref(false)
const openDriftCorrection = ref(false)

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
    clickAction: () => {
      openRateOfChange.value = true
    },
  },
  {
    title: 'Select gaps',
    props: {
      prependIcon: 'mdi-keyboard-space',
    },
    clickAction: () => {
      openGaps.value = true
    },
  },
  {
    title: 'Persistence',
    props: {
      prependIcon: 'mdi-dots-horizontal',
    },
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
    clickAction: () => {},
  },
  {
    title: 'Drift correction',
    props: {
      prependIcon: 'mdi-chart-sankey',
    },
    clickAction: () => {
      openDriftCorrection.value = true
    },
  },
  {
    title: 'Interpolate',
    props: {
      prependIcon: 'mdi-transit-connection-horizontal',
    },
    clickAction: () => (openInterpolate.value = true),
  },
  {
    title: 'Change values',
    props: {
      prependIcon: 'mdi-pencil',
      disabled: true,
    },
    clickAction: () => (openChangeValues.value = true),
  },
  {
    title: 'Delete points',
    props: {
      prependIcon: 'mdi-trash-can',
    },
    clickAction: () => {
      openDeletePoints.value = true
    },
  },
  {
    title: 'Add points',
    props: {
      prependIcon: 'mdi-plus',
    },
    clickAction: () => {},
  },
]
</script>
