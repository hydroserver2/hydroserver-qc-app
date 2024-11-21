import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { storeToRefs } from 'pinia'
import { computed } from 'vue'

export function useDataSelection() {
  const { selectedData } = storeToRefs(useDataVisStore())
  const { selectedSeries, brushSelections, echartsRef } = storeToRefs(
    useEChartsStore()
  )

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

  /** Applies and dispatches the selection from an iterable object */
  const applySelection = (iterable: any) => {
    const selection = Array.from(iterable)
    selectedData.value = {}
    brushSelections.value = []
    selection.forEach((index) => {
      selectedData.value[index as number] = {
        index: index as number,
        date: selectedSeries.value.data.dataFrame.get_datetime_at(index),
        value: selectedSeries.value.data.dataFrame.get_value_at(index),
      }
    })

    dispatchSelection()
  }

  /** Dispatch selection of the currently selected points in `selectedData` */
  const dispatchSelection = () => {
    setTimeout(() => {
      console.log('dispatchSelection')
      echartsRef.value?.dispatchAction({
        type: 'select',
        dataIndex: selectedIndex.value,
      })
    }, 100) // Need to wait for brush selection handler
  }

  const clearSelected = () => {
    echartsRef.value?.dispatchAction({
      type: 'unselect',
      dataIndex: selectedIndex.value,
    })
    selectedData.value = {}
  }

  return {
    selectedIndex,
    selectedRange,
    applySelection,
    dispatchSelection,
    clearSelected,
  }
}
