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

  /**Stores styles for the graphSeriesArray to persist preferences across reloads */
  const seriesOptionMap = ref<{ [id: string]: LineSeriesOption }>({})

  function getSeriesOption(id: string): LineSeriesOption {
    const option = seriesOptionMap.value[id]
    if (option) return option

    return {
      itemStyle: {
        color: '#5571c7', // blue,
      },
      lineStyle: {
        type: 'solid',
      },
      symbol: undefined,
    }
  }

  // TODO: Update this and use it in the code
  /**Update all variables that hold ECharts series options */
  function setSeriesStyles(
    id: string,
    options: Partial<LineSeriesOption>
  ): void {
    const existingOptions = seriesOptionMap.value[id] || {}
    seriesOptionMap.value[id] = { ...existingOptions, ...options }

    graphSeriesArray.value = graphSeriesArray.value.map((series) => {
      if (series.id === id) {
        return {
          ...series,
          seriesOption: {
            ...series.seriesOption,
            ...options,
          },
        }
      }
      return series
    })

    // TODO: Optionally, force an update to the ECharts instance. Should this be a param?
    // Or should the caller of this function just call updateVisualization?
    echartsOption.value = createEChartsOption(graphSeriesArray.value)
  }

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
  function updateVisualization(selectedDatastreamId?: string) {
    graphSeriesArray.value.forEach((series, index) => {
      series.isSelected = series.id === selectedDatastreamId

      if (!series.seriesOption.itemStyle)
        series.seriesOption.itemStyle = { color: '' }

      if (series.isSelected) series.seriesOption.itemStyle.color = '#5571c7'
      else
        series.seriesOption.itemStyle.color =
          EChartsColors[index % EChartsColors.length]
    })

    echartsOption.value = createEChartsOption(graphSeriesArray.value)
    prevIds.value = graphSeriesArray.value.map((series) => series.id)
  }

  const fetchGraphSeries = async (
    datastream: Datastream,
    start: string,
    end: string
  ) => {
    const observationsPromise = fetchObservationsInRange(
      datastream,
      start,
      end
    ).catch((error) => {
      Snackbar.error('Failed to fetch observations')
      console.error('Failed to fetch observations:', error)
      return null
    })
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

    const [observations, unit, observedProperty] = await Promise.all([
      observationsPromise,
      fetchUnitPromise,
      fetchObservedPropertyPromise,
    ])

    const processedData = preProcessData(observations, datastream)

    const yAxisLabel =
      observedProperty && unit
        ? `${observedProperty.name} (${unit.symbol})`
        : 'Unknown'

    console.log('fetching graph series', datastream.name)

    return {
      id: datastream.id,
      name: datastream.name,
      data: processedData,
      yAxisLabel,
      seriesOption: getSeriesOption(datastream.id),
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
    seriesOptionMap,
    updateVisualization,
    clearChartState,
    resetChartZoom,
    fetchGraphSeries,
    setSeriesStyles,
  }
})
