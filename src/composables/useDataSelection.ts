import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { storeToRefs } from 'pinia'
import { computed } from 'vue'

const { selectedData } = storeToRefs(useDataVisStore())
const { selectedSeries } = storeToRefs(useEChartsStore())

export function useDataSelection() {
  const selectedIndex = computed(() => {
    return Object.keys(selectedData.value)
      .map((i) => selectedSeries.value.data.dataFrame.get_index_at(+i))
      .sort((a, b) => a - b)
  })

  const selectedRange = computed(() => {
    return selectedIndex.value.length > 1
      ? [
          selectedIndex.value[0],
          selectedIndex.value[selectedIndex.value.length - 1],
        ]
      : undefined
  })

  return {
    selectedIndex,
    selectedRange,
  }
}
