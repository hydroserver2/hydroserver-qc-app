<template>
  <v-navigation-drawer permanent rail class="bg-navbar">
    <v-list-item>
      <v-img :src="HydroServerIcon"></v-img>
    </v-list-item>

    <v-divider />

    <v-list density="compact" nav>
      <v-tooltip bottom :openDelay="500" v-for="item in items">
        <template v-slot:activator="{ props }">
          <v-list-item
            :prepend-icon="item.icon"
            :value="item.title"
            @click="onRailItemClicked(item.title as DrawerType)"
            v-bind:="props"
            :class="{
              'v-list-item--active':
                selectedDrawer === item.title && isDrawerOpen,
            }"
          />
        </template>
        {{ item.title }}
      </v-tooltip>
    </v-list>

    <v-spacer />

    <template v-slot:append>
      <v-list>
        <v-list-item prepend-icon="mdi-logout" @click.prevent="onLogout" />
      </v-list>
    </template>
  </v-navigation-drawer>

  <div v-if="isDrawerOpen">
    <FileDrawer v-if="selectedDrawer === DrawerType.File" />
    <EditDrawer v-if="selectedDrawer === DrawerType.Edit" />
    <SelectDrawer v-if="selectedDrawer === DrawerType.Select" />
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import HydroServerIcon from '@/assets/favicon-32x32.png'
import FileDrawer from '@/components/Navigation/FileDrawer.vue'
import EditDrawer from '@/components/Navigation/EditDrawer.vue'
import SelectDrawer from '@/components/Navigation/SelectDrawer.vue'
import { useAuthStore } from '@/store/authentication'
import { useUIStore, DrawerType } from '@/store/userInterface'
import { Snackbar } from '@/utils/notifications'
import { storeToRefs } from 'pinia'

const { logout } = useAuthStore()
const { onRailItemClicked } = useUIStore()
const { selectedDrawer, isDrawerOpen } = storeToRefs(useUIStore())

const items = ref([
  { title: 'File', icon: 'mdi-file' },
  { title: 'Select', icon: 'mdi-cursor-default-click-outline' },
  { title: 'Edit', icon: 'mdi-pencil' },
])

function onLogout() {
  logout()
  Snackbar.info('You have logged out')
}
</script>
