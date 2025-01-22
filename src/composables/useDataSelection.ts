import { useDataVisStore } from '@/store/dataVisualization'
import { usePlotlyStore } from '@/store/plotly'
import { storeToRefs } from 'pinia'
import { computed } from 'vue'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'

export function useDataSelection() {
  const { selectedData } = storeToRefs(useDataVisStore())
  const { selectedSeries, plotlyRef } = storeToRefs(usePlotlyStore())

  // const selectedIndex = computed(() => {
  //   return Object.keys(selectedData.value)
  //     .map((i) => selectedSeries.value.data.dataFrame.get_index_at(+i))
  //     .sort((a, b) => a - b)
  // })

  // const selectedRange = computed(() => {
  //   return selectedIndex.value.length > 1
  //     ? [
  //         selectedIndex.value[0],
  //         selectedIndex.value[selectedIndex.value.length - 1],
  //       ]
  //     : undefined
  // })

  /** Applies and dispatches the selection from an iterable object */
  const applySelection = (iterable: any) => {
    const selection = Array.from(iterable)
    // selectedData.value = {}
    const selectedIndex: number[] = []
    // brushSelections.value = []
    selection.forEach((index) => {
      selectedIndex.push(+(index as number))
      // selectedData.value[index as number] = {
      //   index: index as number,
      //   x: selectedSeries.value.data.dataFrame.get_datetime_at(index),
      //   y: selectedSeries.value.data.dataFrame.get_value_at(index),
      // }
    })

    dispatchSelection(selectedIndex)
  }

  /** Dispatch selection  */
  const dispatchSelection = (selectedIndex: number[]) => {
    return Plotly.update(
      plotlyRef.value,
      {
        selections: [], // Removes the selected areas
        selectedpoints: selectedIndex,
      },
      {},
      0
    )
  }

  const clearSelected = () => {
    return Plotly.update(
      plotlyRef.value,
      {
        selections: [], // Removes the selected areas
        selectedpoints: [],
      },
      {},
      0
    )
  }

  const clearBrush = () => {
    return Plotly.update(
      plotlyRef.value,
      {
        selections: [], // Removes the selected areas
        // selectedpoints: [],
      },
      {},
      0
    )
  }

  return {
    // selectedIndex,
    // selectedRange,
    applySelection,
    dispatchSelection,
    clearSelected,
    clearBrush,
  }
}
