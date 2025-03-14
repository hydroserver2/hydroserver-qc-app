import { Datastream, GraphSeries } from '@/types'
import { defineStore, storeToRefs } from 'pinia'
import { computed, ComputedRef, Ref, ref, watch } from 'vue'

import { Snackbar } from '@/utils/notifications'
import { api } from '@/services/api'
// import { preProcessData } from '@/utils/observationsUtils'
import { useObservationStore } from '@/store/observations'
import { useDataVisStore } from './dataVisualization'
import {
  EnumEditOperations,
  ObservationRecord,
} from '@/utils/plotting/observationRecord'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'

// Register custom data sampler
// import dataSample from '@/utils/custom-down-sample'
import { createPlotlyOption } from '@/utils/plotting/plotly'
import { LineColors } from '@/utils/materialColors'

export const usePlotlyStore = defineStore('Plotly', () => {
  const { fetchObservationsInRange } = useObservationStore()

  const showLegend = ref(true)
  const showTooltip = ref(false)

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
   */
  async function redraw() {
    console.log('redraw')

    updateOptions()

    await Plotly.redraw(plotlyRef.value, [0])

    // TODO: this updates range, but breaks selection controls
    // await Plotly.react(
    //   plotlyRef.value,
    //   plotlyOptions.value.traces,
    //   plotlyOptions.value.layout
    // )
  }

  const fetchGraphSeriesData = async (
    datastream: Datastream,
    start: Date,
    end: Date
  ): Promise<ObservationRecord | null> => {
    console.log('fetchGraphSeriesData')
    const obsRecord = await fetchObservationsInRange(
      datastream,
      start,
      end
    ).catch((error) => {
      Snackbar.error('Failed to fetch observations')
      console.error('Failed to fetch observations:', error)
      return null
    })

    // qualifiers.resultQualifiers.map((q) => q.code).join(', ')

    return obsRecord

    // TODO: try to avoid this pre processing
    // return preProcessData(observations, datastream)
  }

  const fetchGraphSeries = async (
    datastream: Datastream,
    start: Date,
    end: Date
  ) => {
    console.log('fetchGraphSeries')
    const observationsPromise = fetchGraphSeriesData(datastream, start, end)
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
    fetchGraphSeriesData,
    plotlyOptions,
    plotlyRef,
  }
})
