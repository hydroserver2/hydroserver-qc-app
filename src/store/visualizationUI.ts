import { defineStore } from 'pinia'
import { computed, ref, watch } from 'vue'

export enum DrawerType {
  File = 'File',
  Edit = 'Edit',
  Select = 'Select',
  None = '',
}

export const useUIStore = defineStore('visualizationUI', () => {
  // Navigation Drawer
  const selectedDrawer = ref<DrawerType>(DrawerType.None)
  const isDrawerOpen = ref(false)

  const currentView = ref<DrawerType.Edit | DrawerType.Select>(
    DrawerType.Select
  )
  const cardHeight = ref(40)
  const tableHeight = ref(35)

  const loadingStates = ref(new Map<string, boolean>()) // State to track loading status of individual datasets

  const onRailItemClicked = (title: DrawerType) => {
    if (selectedDrawer.value === title) {
      isDrawerOpen.value = !isDrawerOpen.value
    } else {
      selectedDrawer.value = title
      if (title === DrawerType.Edit) currentView.value = DrawerType.Edit
      if (title === DrawerType.Select) currentView.value = DrawerType.Select
      isDrawerOpen.value = true
    }
  }

  return {
    loadingStates,
    cardHeight,
    tableHeight,
    selectedDrawer,
    isDrawerOpen,
    currentView,
    onRailItemClicked,
  }
})
