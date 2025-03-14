import { useDataVisStore } from '@/store/dataVisualization'
import { usePlotlyStore } from '@/store/plotly'
import { storeToRefs } from 'pinia'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'

export function useDataSelection() {
  const { plotlyRef } = storeToRefs(usePlotlyStore())

  /** Applies and dispatches the selection from an iterable object */
  const applySelection = (selection: number[]) => {
    dispatchSelection(selection)
  }

  /** Dispatch selection  */
  const dispatchSelection = async (selection: number[]) => {
    const { selectedData } = storeToRefs(useDataVisStore())

    await Plotly.update(
      plotlyRef.value,
      {
        selections: [], // Removes the selected areas
        selectedpoints: [selection], // Plotly expects one array per trace (even if updating a single trace).
      },
      {},
      0
    )

    // If the selection is empty, Plotly wont trigger relayout. We must cleanup the selection array.
    if (!selection.length) {
      selectedData.value = plotlyRef.value?.data[0].selectedpoints || null
    }
  }

  /** Call this method after operations that change the order of elements or remove elements in the data */
  const clearSelected = async () => {
    const { selectedData } = storeToRefs(useDataVisStore())
    const { plotlyRef } = storeToRefs(usePlotlyStore())

    // Removes selected areas
    await Plotly.update(
      plotlyRef.value,
      {},
      { selections: [], selectedpoints: [[]] },
      [0]
    )

    // Updates the color
    await Plotly.restyle(plotlyRef.value, {
      selectedpoints: [[]],
    })

    selectedData.value = []
  }

  return {
    applySelection,
    dispatchSelection,
    clearSelected,
  }
}
