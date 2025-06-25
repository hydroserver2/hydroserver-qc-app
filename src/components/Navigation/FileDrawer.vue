<template>
  <v-navigation-drawer
    permanent
    :width="350"
    rounded="e-xl"
    elevation="3"
    class="bg-navbar"
    theme="dark"
  >
    <v-list :items="editData" density="compact"> </v-list>
  </v-navigation-drawer>
</template>

<script setup lang="ts">
const editData = [
  { type: 'subheader', title: 'Save to database' },
  {
    title: 'Save for review',
    props: {
      prependIcon: 'mdi-account-multiple',
    },
    value: 1,
  },
  {
    title: 'Save as new',
    props: {
      prependIcon: 'mdi-content-save-all-outline',
    },
    value: 2,
  },
  {
    title: 'Overwrite existing',
    props: {
      prependIcon: 'mdi-content-save-edit',
    },
    value: 3,
  },
  {
    title: 'Save',
    props: {
      prependIcon: 'mdi-content-save',
    },
    value: 4,
  },
]
</script>
