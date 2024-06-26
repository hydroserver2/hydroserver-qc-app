import { Datastream, GraphSeries } from '@/types'
import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
import { EChartsOption, LineSeriesOption } from 'echarts'
import { EChartsColors } from '@/utils/materialColors'
import {
  createEChartsOption,
  createLegendConfig,
  createTooltipConfig,
  generateDataZoomOptions,
} from '@/utils/plotting/echarts'
import { Snackbar } from '@/utils/notifications'
import { api } from '@/services/api'
import { preProcessData } from '@/utils/observationsUtils'
import { useObservationStore } from '@/store/observations'

export const useEChartsStore = defineStore('ECharts', () => {
  const { fetchObservationsInRange } = useObservationStore()

  const showLegend = ref(true)
  const showTooltip = ref(false)
  const dataZoomEnd = ref(100)

  const graphSeriesArray = ref<GraphSeries[]>([])
  const prevIds = ref<string[]>([]) // DatastreamIds that were previously plotted

  const echartsOption = ref<EChartsOption | undefined>()
  const dataZoomStart = ref(0)

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
    prevIds.value = []
    echartsOption.value = undefined
  }

  // TODO: This should only trigger an ECharts refresh. Move the line coloring somewhere else
  // Also, I'm thinking the line colors shouldn't change once set to a series. I think it would
  // be better instead to have them loop through the colors and somehow keep track of which ones are used
  function updateVisualization() {
    echartsOption.value = createEChartsOption(graphSeriesArray.value)
    prevIds.value = graphSeriesArray.value.map((series) => series.id)
  }

  const fetchGraphSeriesData = async (
    datastream: Datastream,
    start: string,
    end: string
  ) => {
    const observations = await fetchObservationsInRange(
      datastream,
      start,
      end
    ).catch((error) => {
      Snackbar.error('Failed to fetch observations')
      console.error('Failed to fetch observations:', error)
      return []
    })

    return preProcessData(observations, datastream)
  }

  const fetchGraphSeries = async (
    datastream: Datastream,
    start: string,
    end: string
  ) => {
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
    prevIds,
    updateVisualization,
    clearChartState,
    resetChartZoom,
    fetchGraphSeries,
    fetchGraphSeriesData,
  }
})
