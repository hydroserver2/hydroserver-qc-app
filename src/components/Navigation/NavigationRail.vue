<template>
  <v-navigation-drawer permanent rail>
    <v-list-item>
      <v-img :src="HydroServerIcon"></v-img>
    </v-list-item>

    <v-divider />

    <v-list density="compact" nav>
      <v-list-item
        v-for="item in items"
        :prepend-icon="item.icon"
        :value="item.title"
        @click="onRailItemClicked(item.title)"
      />
    </v-list>
  </v-navigation-drawer>

  <div v-if="showDrawer">
    <FileDrawer v-if="selectedDrawer === 'File'" />
    <EditDrawer v-if="selectedDrawer === 'Edit'" />
    <SelectDrawer v-if="selectedDrawer === 'Select'" />
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import HydroServerIcon from '@/assets/favicon-32x32.png'
import FileDrawer from '@/components/Navigation/FileDrawer.vue'
import EditDrawer from '@/components/Navigation/EditDrawer.vue'
import SelectDrawer from '@/components/Navigation/SelectDrawer.vue'

const selectedDrawer = ref('')
const showDrawer = ref(false)

const items = ref([
  { title: 'File', icon: 'mdi-file' },
  { title: 'Select', icon: 'mdi-cursor-default-click-outline' },
  { title: 'Edit', icon: 'mdi-pencil' },
])

const onRailItemClicked = (title: string) => {
  if (selectedDrawer.value === title) {
    showDrawer.value = !showDrawer.value
  } else {
    selectedDrawer.value = title
    showDrawer.value = true
  }
}
</script>
