import {
  Datastream,
  GraphSeries,
  EChartsLineStyleType,
  EChartsSeriesSymbol,
} from '@/types'
import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
import { EChartsOption } from 'echarts'
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

  interface SeriesStyle {
    lineStyleType: EChartsLineStyleType
    symbol: EChartsSeriesSymbol
  }

  const seriesStyleMap = ref<{ [id: string]: SeriesStyle }>({})

  function getLineStyleType(id: string): EChartsLineStyleType {
    const style = seriesStyleMap.value[id]
    return style ? style.lineStyleType : undefined
  }

  function getSymbol(id: string): EChartsSeriesSymbol {
    const style = seriesStyleMap.value[id]
    return style ? style.symbol : undefined
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

  function updateVisualization(selectedDatastreamId?: string) {
    graphSeriesArray.value.forEach((series, index) => {
      series.isSelected = series.id === selectedDatastreamId

      if (series.isSelected) series.lineColor = '#5571c7'
      else series.lineColor = EChartsColors[index % EChartsColors.length]
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

    return {
      id: datastream.id,
      name: datastream.name,
      data: processedData,
      yAxisLabel,
      lineColor: '#5571c7', // default to blue,
      lineStyleType: getLineStyleType(datastream.id),
      symbol: getSymbol(datastream.id),
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
    seriesStyleMap,
    updateVisualization,
    clearChartState,
    resetChartZoom,
    fetchGraphSeries,
  }
})
