import {
  Datastream,
  EnumEditOperations,
  GraphSeries,
  ObservationRecord,
} from '@/types'
import { defineStore, storeToRefs } from 'pinia'
import { computed, ComputedRef, Ref, ref, watch } from 'vue'
import { EChartsOption, LineSeriesOption } from 'echarts'
import { EChartsColors } from '@/utils/materialColors'
import {
  createEChartsOption,
  createLegendConfig,
  createTooltipConfig,
  createYAxisConfigurations,
  generateDataZoomOptions,
  generateSeriesOptions,
} from '@/utils/plotting/echarts'
import { Snackbar } from '@/utils/notifications'
import { api } from '@/services/api'
// import { preProcessData } from '@/utils/observationsUtils'
import { useObservationStore } from '@/store/observations'
import { useDataVisStore } from './dataVisualization'

export const useEChartsStore = defineStore('ECharts', () => {
  const { fetchObservationsInRange } = useObservationStore()

  const showLegend = ref(true)
  const showTooltip = ref(false)
  const dataZoomEnd = ref(100)

  const graphSeriesArray = ref<GraphSeries[]>([])
  /** The index of the ECharts series that represents the datastream selected for quality control */
  const selectedSeriesIndex = computed(() => {
    const { qcDatastream } = storeToRefs(useDataVisStore())
    if (qcDatastream.value?.id) {
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

  const echartsOption = ref<EChartsOption | undefined>()
  const dataZoomStart = ref(0)
  const brushSelections = ref([])

  /**
   * This function searches through the Pinia store's GraphSeries[] to determine which colors,
   * defined in the EChartsColors array, are currently in use. It then selects and returns
   * the first color from EChartsColors that is not already being used in any of the graph series.
   *
   * @returns {string} - Hex code of the first available color that is not in use. Returns black as a default if all are in use.
   */
  function assignColor(): string {
    const usedColors = new Set(
      graphSeriesArray.value.map((s) => s.seriesOption.itemStyle?.color)
    )

    for (const color of EChartsColors) {
      if (!usedColors.has(color)) {
        return color
      }
    }

    return '#000000'
  }

  const getDefaultSeriesOption = (): LineSeriesOption => ({
    itemStyle: {
      color: assignColor(),
    },
    lineStyle: {
      type: 'solid',
    },
    symbol: undefined,
  })

  function resetChartZoom() {
    dataZoomStart.value = 0
    dataZoomEnd.value = 100
  }

  const clearChartState = () => {
    graphSeriesArray.value = []
    echartsOption.value = undefined
  }

  /**
   * This won't trigger EChart's setOption because the reference changes.
   * Builds the chart from scratch.
   * @see https://echarts.apache.org/en/api.html#echartsInstance.setOption
   */
  function createVisualization() {
    console.log('createVisualization')
    echartsOption.value = createEChartsOption(graphSeriesArray.value)
  }

  /**
   * Use this function to update the Echart's dataset option only without
   * recreating the entire chart
   */
  function updateVisualizationData() {
    console.log('updateVisualizationData')
    if (echartsOption.value) {
      echartsOption.value.dataset = graphSeriesArray.value.map(
        (s) => s.data.dataset
      )
    }
  }

  /** Use this function when the data is already plotted, but we still want to
   * draw the scatter plot for the selected dataset */
  function drawScatterPlot() {
    if (echartsOption.value) {
      const yAxisConfigurations = createYAxisConfigurations(
        graphSeriesArray.value
      )

      echartsOption.value.series = generateSeriesOptions(
        graphSeriesArray.value,
        yAxisConfigurations,
        selectedSeriesIndex.value
      )

      echartsOption.value.brush = {
        toolbox: ['rect', 'keep', 'lineY'],
        xAxisIndex: [0],
        seriesIndex: selectedSeriesIndex.value,
        throttleType: 'debounce',
        throttleDelay: 100,
        outOfBrush: {
          colorAlpha: 0.1, // dims the points outside the brushed area
        },
      }
    }
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
      seriesOption: getDefaultSeriesOption(),
    } as GraphSeries
  }

  // This manually updates the legend since reactivity isn't preserved just setting
  // the echarts option to a pinia store variable. There's probably a better way to do this
  watch(showLegend, () => {
    if (echartsOption.value) {
      echartsOption.value.legend = createLegendConfig()
      echartsOption.value.dataZoom = generateDataZoomOptions(false)
      let seriesCount = 0

      if (Array.isArray(echartsOption.value.series)) {
        seriesCount = echartsOption.value.series.length
      } else if (echartsOption.value.series) {
        seriesCount = 1
      }
    }
  })

  watch(showTooltip, () => {
    if (echartsOption.value) {
      echartsOption.value.tooltip = createTooltipConfig()
      echartsOption.value.dataZoom = generateDataZoomOptions(false)
    }
  })

  return {
    dataZoomStart,
    dataZoomEnd,
    graphSeriesArray,
    echartsOption,
    showLegend,
    showTooltip,
    selectedSeriesIndex,
    selectedSeries,
    editHistory,
    brushSelections,
    createVisualization,
    updateVisualizationData,
    clearChartState,
    resetChartZoom,
    fetchGraphSeries,
    fetchGraphSeriesData,
    drawScatterPlot,
  }
})
