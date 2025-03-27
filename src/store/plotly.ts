import { Datastream, GraphSeries } from '@/types'
import { defineStore, storeToRefs } from 'pinia'
import { computed, ComputedRef, Ref, ref, watch } from 'vue'

import { api } from '@/services/api'
// import { preProcessData } from '@/utils/observationsUtils'
import { useDataVisStore } from './dataVisualization'
import { EnumEditOperations } from '@/utils/plotting/observationRecord'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'

// Register custom data sampler
// import dataSample from '@/utils/custom-down-sample'
import { createPlotlyOption, cropXaxisRange } from '@/utils/plotting/plotly'
import { LineColors } from '@/utils/materialColors'
import { useObservationStore } from './observations'

export const usePlotlyStore = defineStore('Plotly', () => {
  const showLegend = ref(true)
  const showTooltip = ref(false)
  const isUpdating = ref(false)

  const graphSeriesArray = ref<GraphSeries[]>([])
  /** The index of the series that represents the datastream selected for quality control */
  const selectedSeriesIndex = computed(() => {
    const { qcDatastream } = storeToRefs(useDataVisStore())
    if (qcDatastream?.value?.id) {
      return graphSeriesArray.value.findIndex(
        (s) => s.id === qcDatastream.value?.id
      )
    }
    return -1
  })
  /** The edit history for the currently selected series */
  const editHistory: Ref<
    { method: EnumEditOperations; args?: any[]; icon: string }[]
  > = ref([])

  const selectedSeries: ComputedRef<GraphSeries> = computed(() => {
    return graphSeriesArray.value[selectedSeriesIndex.value]
  })

  const plotlyOptions: Ref<any> = ref({})
  const plotlyRef: Ref<(HTMLDivElement & { [key: string]: any }) | null> =
    ref(null) // Populated during DataVisualization onMounted hook

  /**
   * This function searches through the Pinia store's GraphSeries[] to determine which colors,
   * are currently in use. It then selects and returns
   * the first color that is not already being used in any of the graph series.
   *
   * @returns {string} - Hex code of the first available color that is not in use. Returns black as a default if all are in use.
   */
  function assignColor(): string {
    const usedColors = new Set(
      graphSeriesArray.value.map((s) => s.seriesOption.itemStyle?.color)
    )

    for (const color of LineColors) {
      if (!usedColors.has(color)) {
        return color
      }
    }

    return '#000000'
  }

  const clearChartState = () => {
    graphSeriesArray.value = []
  }

  /**
   * Set the initial chart options.
   */
  function updateOptions() {
    console.log('updateOptions')
    // @ts-ignore
    plotlyOptions.value = createPlotlyOption(graphSeriesArray.value)
  }

  /**
   * Use this function to update the chart after the data has mutated.
   * @param recomputeXaxisRange Useful for when an operation can add or delete elements in the array and the axis range needs to be updated.
   */
  async function redraw(recomputeXaxisRange?: boolean) {
    console.log('redraw')

    updateOptions()

    // TODO: After a DataFrame operation the array would have changed and this redraw has no effect
    // await Plotly.redraw(plotlyRef.value, [0])

    // TODO: this updates range, but breaks selection controls
    await Plotly.react(
      plotlyRef.value,
      plotlyOptions.value.traces,
      plotlyOptions.value.layout
    )

    if (recomputeXaxisRange) {
      await cropXaxisRange()
    }
  }

  const fetchGraphSeries = async (
    datastream: Datastream,
    start: Date,
    end: Date
  ) => {
    console.log('fetchGraphSeries')
    const { fetchObservationsInRange } = useObservationStore()
    const observationsPromise = fetchObservationsInRange(datastream, start, end)
    const fetchUnitPromise = api.getUnit(datastream.unitId).catch((error) => {
      console.error('Failed to fetch Unit:', error)
      return null
    })
    const fetchObservedPropertyPromise = api
      .fetchObservedProperty(datastream.observedPropertyId)
      .catch((error) => {
        console.error('Failed to fetch ObservedProperty:', error)
        return null
      })

    const [data, unit, observedProperty] = await Promise.all([
      observationsPromise,
      fetchUnitPromise,
      fetchObservedPropertyPromise,
    ])

    if (!data.dataset.source.x) {
      await data.reload()
    }

    const yAxisLabel =
      observedProperty && unit
        ? `${observedProperty.name} (${unit.symbol})`
        : 'Unknown'

    return {
      id: datastream.id,
      name: datastream.name,
      data,
      yAxisLabel,
    } as GraphSeries
  }

  watch(showLegend, () => {
    if (plotlyOptions.value) {
      // TODO: integrate createLegendConfig()
    }
  })

  watch(showTooltip, () => {
    if (plotlyOptions.value) {
      // TODO: integrate createTooltipConfig()
    }
  })

  return {
    graphSeriesArray,
    showLegend,
    showTooltip,
    selectedSeriesIndex,
    selectedSeries,
    editHistory,
    updateOptions,
    redraw,
    clearChartState,
    fetchGraphSeries,
    plotlyOptions,
    plotlyRef,
    isUpdating,
  }
})
