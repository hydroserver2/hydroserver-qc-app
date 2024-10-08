import {
  EChartsOption,
  YAXisComponentOption,
  SeriesOption,
  LegendComponentOption,
  TooltipComponentOption,
  // registerTransform,
} from 'echarts'
import { DataPoint, Datastream, GraphSeries } from '@/types'
import { storeToRefs } from 'pinia'
import { useEChartsStore } from '@/store/echarts'
import { useDataVisStore } from '@/store/dataVisualization'
import {
  GridOption,
  TooltipOption,
  XAXisOption,
  YAXisOption,
} from 'echarts/types/dist/shared'

type yAxisConfigurationMap = Map<
  string,
  { index: number; yAxisLabel: string; color: string }
>

// registerTransform({
//   type: 'hydroserver:pandas-df-map',
//   transform: function (params) {
//     // const rawData = params.upstream.cloneRawData() // TODO: too expensive
//     // TODO: why does this run 3 times?
//     // @ts-ignore
//     // const data = rawData.map((o, index) => ({
//     //   ...o,
//     // }))
//     console.log('transform')
//     const count = params.upstream.count()
//     return [
//       {
//         data: [],
//       },
//     ]
//   },
// })

/**
Function that processes an array of GraphSeries and returns a map for Y-Axis configurations in order 
to allow multiple series to share the same Y-Axis if yAxisLabels are the same.
Colors axis black if multiple series use it.
*/
export function createYAxisConfigurations(
  data: GraphSeries[]
): yAxisConfigurationMap {
  const yAxisConfigurations: yAxisConfigurationMap = new Map()

  data.forEach((series) => {
    if (!yAxisConfigurations.has(series.yAxisLabel)) {
      const color =
        typeof series.seriesOption.itemStyle?.color === 'string'
          ? series.seriesOption.itemStyle?.color
          : '#5571c7' // default blue

      yAxisConfigurations.set(series.yAxisLabel, {
        index: yAxisConfigurations.size,
        yAxisLabel: series.yAxisLabel,
        color: color,
      })
    } else {
      const existingEntry = yAxisConfigurations.get(series.yAxisLabel)
      if (existingEntry && existingEntry.color !== 'black') {
        yAxisConfigurations.set(series.yAxisLabel, {
          ...existingEntry,
          color: 'black',
        })
      }
    }
  })

  return yAxisConfigurations
}

// export function generateDataset(seriesArray: GraphSeries[]) {
//   console.log(seriesArray)
//   return [
//     {
//       source: [],
//     },
//     ...seriesArray.map((gs) => {
//       return {
//         transform: {
//           // Reference the registered external transform.
//           type: 'hydroserver:pandas-df-map',
//           config: {
//             // Parameters needed by the external transform.
//             // someParam: 'someValue',
//           },
//         },
//       }
//     }),
//   ]
// }

export function generateYAxisOptions(
  yAxisConfigurations: yAxisConfigurationMap
): YAXisComponentOption[] {
  const leftYAxesCount = Math.ceil(yAxisConfigurations.size / 2)

  return Array.from(yAxisConfigurations.values()).map((yAxisConfig, index) => {
    const position = index < leftYAxesCount ? 'left' : 'right'
    let offset = index === 0 ? 0 : (index - leftYAxesCount) * 85
    if (position === 'left') offset = -offset

    // TODO: extend range of y-axis to facilitate padding for selection controls
    return {
      name: yAxisConfig.yAxisLabel,
      nameLocation: 'middle',
      nameGap: 60,
      type: 'value',
      position,
      offset,
      min: 'dataMin',
      max: 'dataMax',
      axisLabel: {
        showMaxLabel: false,
        showMinLabel: false,
      },
      axisTick: {
        show: true,
        length: 5,
      },
      splitLine: {
        show: false,
      },
      axisLine: { show: true, lineStyle: { color: yAxisConfig.color } },
    }
  })
}

// TODO: Instead of merging manually, use a spread operator on seriesOption
// The reason we're overlaying a line plot on top of a scatter plot for the selected
// datastream is because ECharts currently only supports data selection for scatter plots not line plots
export function generateSeriesOptions(
  seriesArray: GraphSeries[],
  yAxisConfigurations: yAxisConfigurationMap,
  selectedIndex: number
): SeriesOption[] {
  return seriesArray.flatMap((series, index): SeriesOption[] => {
    const baseSeries: SeriesOption = {
      name: series.name,
      type: 'line',
      xAxisIndex: 0,
      yAxisIndex: yAxisConfigurations.get(series.yAxisLabel)?.index,
      itemStyle: {
        color: series.seriesOption.itemStyle?.color,
      },
      lineStyle: {
        width: 1,
        type: series.seriesOption.lineStyle?.type,
      },
      emphasis: {
        focus: 'series',
      },
      sampling: 'lttb',
      symbol: series.seriesOption.symbol,
      showSymbol: !!series.seriesOption.symbol,
      // dimensions: ['date', 'value'],
      encode: {
        x: 'date',
        y: 'value',
      },
      datasetIndex: index,
    }

    if (index === selectedIndex) {
      const scatterSeries: SeriesOption = {
        ...baseSeries,
        type: 'scatter',
        large: true, // Makes the series render more efficiently
      }

      const lineSeries: SeriesOption = {
        ...baseSeries,
        name: '', // No name for this series so it won't appear in the legend
        lineStyle: { ...baseSeries.lineStyle, opacity: 0.5 },
        emphasis: { focus: 'none' },
        showSymbol: false,
        tooltip: { show: false },
      }

      return [scatterSeries, lineSeries]
    }

    return [baseSeries]
  })
}

export function generateToolboxOptions() {
  const { brushSelections } = storeToRefs(useEChartsStore())
  const { updateVisualization } = useEChartsStore()
  const { selectedData } = storeToRefs(useDataVisStore())

  return {
    feature: {
      dataZoom: {
        yAxisIndex: false,
      },
      restore: {},
      saveAsImage: { name: 'plot_export' },
      myClearSelected: {
        show: true,
        title: 'Clear selections!',
        icon: 'path://M2 2h20v20h-20z M7 7l10 10 M7 17l10-10',
        onclick: function () {
          brushSelections.value = []
          selectedData.value = []
          // TODO: just call setOption or applyBrushSelection instead
          updateVisualization() // call updateVisualization to rerender the plot with the empty brushSelections
        },
      },
    },
  }
}

export function generateDataZoomOptions(initializeZoomed: boolean) {
  const { dataZoomStart, dataZoomEnd } = storeToRefs(useEChartsStore())

  // For mouse scrolling in the chart
  const insideSettings = {
    type: 'inside',
    xAxisIndex: [0, 1],
  }

  const sliderSettings = {
    type: 'slider',
    xAxisIndex: [0, 1],
    bottom: '60',
    ...(initializeZoomed && {
      start: dataZoomStart.value,
      end: dataZoomEnd.value,
    }),
  }

  return [sliderSettings, insideSettings]
}

export function createLegendConfig(): LegendComponentOption {
  const { showLegend } = storeToRefs(useEChartsStore())
  return {
    bottom: 0,
    show: showLegend.value,
    // orient: 'vertical',
    // left: 'auto',
    left: 'center',
  }
}

export function createTooltipConfig(): TooltipComponentOption {
  const { showTooltip } = storeToRefs(useEChartsStore())
  return {
    confine: true,
    trigger: 'axis',
    showContent: showTooltip.value ? true : false, // Displays the actual tooltip card
    axisPointer: {
      type: 'cross',
      animation: false,
      label: {
        backgroundColor: '#505765',
      },
    },
  }
}

export function addQualifierOptions(
  dsOption: any,
  echartsOption: EChartsOption,
  yAxisIndex: number,
  gridRightPadding: number,
  gridLeftPadding: number
): EChartsOption {
  // Add second grid
  ;(echartsOption.grid as GridOption[]).push({
    bottom: '90',
    right: gridRightPadding,
    left: gridLeftPadding,
    height: '5%',
  })

  // Add YAxis
  ;(echartsOption.yAxis! as YAXisOption[]).push({
    show: false,
    gridIndex: 1,
    // splitNumber:.001,
    max: '1000000000000000',
    min: '-1000000000000000',
  })

  // Add XAxis
  const { selectedQualifier } = storeToRefs(useDataVisStore())
  ;(echartsOption.xAxis! as XAXisOption[]).push({
    type: 'time',
    show: false,
    gridIndex: 1,
    axisPointer: {
      label: {
        formatter: function (params) {
          if (params.seriesData && params.seriesData.length > 0) {
            const firstSeriesItem = params.seriesData[0]
            if (
              !Array.isArray(firstSeriesItem.data) ||
              firstSeriesItem.data.length <= 2
            )
              return ''

            // TODO
            const qualifierValue = firstSeriesItem.data[2]
            if (typeof qualifierValue === 'string') {
              if (selectedQualifier.value === 'All') return qualifierValue
              if (qualifierValue.includes(selectedQualifier.value))
                return qualifierValue
            }
          }
          return ''
        },
      },
    },
  })

  // Add series
  ;(echartsOption.series! as SeriesOption[]).push({
    type: 'scatter',
    name: 'Qualifiers',

    // XAxis data must be the same or the on mouse vertical lines won't sync up
    // TODO: too costly operation
    data: dsOption.source['value'].map((_val: string, index: number) => {
      return [
        // datetime
        new Date(dsOption?.source?.date[index]).getTime(),
        // qualifier value
        selectedQualifier.value === 'All' ||
        dsOption.source?.qualifier[index]?.has(selectedQualifier.value)
          ? 1
          : NaN,
        // qualifier codes
        // dsOption.source?.qualifier[index],
      ]
    }),

    xAxisIndex: 1,
    yAxisIndex: yAxisIndex,
    symbolSize: 7,
    itemStyle: {
      color: '#F44336',
    },
  })

  // tooltip will be fixed on the right if mouse hovering on the left,
  // and on the left if hovering on the right.
  ;(echartsOption.tooltip as TooltipOption).position = (
    pos,
    _params,
    _dom,
    _rect,
    size
  ) => {
    const position: { top: number; left?: number; right?: number } = { top: 60 }
    const side = pos[0] < size.viewSize[0] / 2 ? 'right' : 'left'
    position[side] = 30
    return position
  }

  echartsOption.axisPointer = {
    link: [
      {
        xAxisIndex: [0, 1],
      },
    ],
    label: {
      backgroundColor: '#777',
    },
  }

  return echartsOption
}

// Function to find the index of the selected datastream in the series array
const findSelectedDatastreamIndex = (
  seriesArray: GraphSeries[],
  datastream: Datastream
) => seriesArray.findIndex((s) => s.id === datastream.id)

// TODO
// const hasStringQualifier = (data: DataPoint[]) =>
//   data.some((dp) => typeof dp.qualifierValue === 'string')

interface CustomOptions {
  initializeZoomed: boolean
}

export const createEChartsOption = (
  seriesArray: GraphSeries[],
  opts: Partial<CustomOptions> = {}
): EChartsOption => {
  console.log('createEChartsOption')
  const { initializeZoomed = true } = opts
  const { qcDatastream } = storeToRefs(useDataVisStore())
  const { selectedSeriesIndex } = storeToRefs(useEChartsStore())

  selectedSeriesIndex.value = -1
  if (qcDatastream.value?.id) {
    selectedSeriesIndex.value = findSelectedDatastreamIndex(
      seriesArray,
      qcDatastream.value
    )
  }

  const yAxisConfigurations = createYAxisConfigurations(seriesArray)
  const leftYAxesCount = Math.ceil(yAxisConfigurations.size / 2)
  const rightYAxesCount = yAxisConfigurations.size - leftYAxesCount
  const gridRightPadding = 20 + rightYAxesCount * 85
  const gridLeftPadding = leftYAxesCount * 85

  // TODO: this is an expensive operation and should be only executed when necessary
  seriesArray.forEach((s) => s.data?.generateDataset())

  let echartsOption: EChartsOption = {
    // https://echarts.apache.org/en/option.html#dataset.source
    dataset: seriesArray.map((s) => s.data.dataset),
    grid: [
      {
        bottom: '160',
        right: gridRightPadding,
        left: gridLeftPadding,
      },
    ],
    tooltip: createTooltipConfig(),
    xAxis: [
      {
        type: 'time',
        axisLabel: {
          hideOverlap: true,
          formatter: {
            year: '{yyyy}',
            month: '{MMM} {yyyy}',
            day: '{MMM} {d}, {yy}',
            hour: '{HH}:{mm}\n{MMM} {d}, {yy}',
            minute: '{HH}:{mm}\n{MMM} {d}, {yy}',
            second: '{H}:{mm}:{s}\n{MMM} {d}, {yy}',
            millisecond: '{HH}:{mm}:{s}:{S}\n{MMM} {d}, {yy}',
          },
        },
      },
    ],
    yAxis: generateYAxisOptions(yAxisConfigurations),
    series: generateSeriesOptions(
      seriesArray,
      yAxisConfigurations,
      selectedSeriesIndex.value
    ),
    dataZoom: generateDataZoomOptions(initializeZoomed),
    legend: createLegendConfig(),
    toolbox: generateToolboxOptions() as {},
    brush: {
      toolbox: ['rect', 'keep', 'lineY'],
      xAxisIndex: [0],
      seriesIndex: selectedSeriesIndex.value,
      throttleType: 'debounce',
      throttleDelay: 100,
      outOfBrush: {
        colorAlpha: 0.1, // dims the points outside the brushed area
      },
    },
  }

  // Add result qualifier options if there's a datastream selected for quality control with qualifiers
  if (
    selectedSeriesIndex.value !== -1
    // && hasStringQualifier(seriesArray[selectedSeriesIndex.value].data.dataFrame)
  ) {
    echartsOption = addQualifierOptions(
      seriesArray[selectedSeriesIndex.value].data.dataset,
      echartsOption,
      yAxisConfigurations.size,
      gridRightPadding,
      gridLeftPadding
    )
  }

  return echartsOption
}
