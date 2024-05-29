import { defineStore } from 'pinia'
import { ref } from 'vue'

export enum DrawerType {
  File = 'File',
  Edit = 'Edit',
  Select = 'Select',
  None = '',
}

export const useUIStore = defineStore('userInterface', () => {
  // Navigation Drawer
  const selectedDrawer = ref<DrawerType>(DrawerType.None)
  const isDrawerOpen = ref(false)

  const currentView = ref<DrawerType.Edit | DrawerType.Select>(
    DrawerType.Select
  )

  // Selection page
  const cardHeight = ref(40)
  const tableHeight = ref(35)

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
    selectedDrawer,
    isDrawerOpen,
    currentView,
    cardHeight,
    tableHeight,
    onRailItemClicked,
  }
})
