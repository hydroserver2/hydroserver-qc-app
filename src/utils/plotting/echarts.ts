import {
  EChartsOption,
  YAXisComponentOption,
  SeriesOption,
  LegendComponentOption,
  TooltipComponentOption,
} from 'echarts'
import { Datastream, GraphSeries } from '@/types'
import { storeToRefs } from 'pinia'
import { useEChartsStore } from '@/store/echarts'
import { useDataVisStore } from '@/store/dataVisualization'
import { useDataSelection } from '@/composables/useDataSelection'

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
export function generateSeriesOptions(
  seriesArray: GraphSeries[],
  yAxisConfigurations: yAxisConfigurationMap,
  selectedIndex: number
): SeriesOption[] {
  console.log('generateSeriesOptions')
  return seriesArray.flatMap((series, index): SeriesOption[] => {
    const baseSeries: SeriesOption = {
      name: series.name,
      type: 'line',
      xAxisIndex: 0,
      yAxisIndex: yAxisConfigurations.get(series.yAxisLabel)?.index,
      lineStyle: {
        width: 1,
        type: series.seriesOption.lineStyle?.type,
      },
      emphasis: {
        focus: 'series',
      },
      symbolSize: 10,
      sampling: 'custom-lttb',
      // @ts-ignore: Actually supported, but not type annotated or documented in EChart's API
      // sampling: (frame) => {
      //   console.log(frame)
      //   let sum = 0
      //   let count = 0
      //   for (let i = 0; i < frame.length; i++) {
      //     if (!isNaN(frame[i])) {
      //       sum += frame[i]
      //       count++
      //     }
      //   }
      //   // Return NaN if count is 0
      //   return count === 0 ? NaN : sum / count
      // },
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
      const lineSeries: SeriesOption = {
        ...baseSeries,
        name: '', // No name for this series so it won't appear in the legend
        lineStyle: { ...baseSeries.lineStyle, opacity: 0.5 },
        showSymbol: true,
        tooltip: { show: false },
        selectedMode: 'multiple',
        select: { itemStyle: { color: 'red' } },
        emphasis: {
          itemStyle: {
            color: 'red',
          },
          focus: 'series',
        },
      }

      return [lineSeries]
    }

    return [baseSeries]
  })
}

export function generateToolboxOptions() {
  const { clearSelected, clearBrush } = useDataSelection()

  return {
    feature: {
      dataZoom: {
        yAxisIndex: false,
      },
      restore: {},
      saveAsImage: { name: 'plot_export' },
      myClearSelected: {
        show: true,
        title: 'Clear selections',
        icon: 'path://M2 2h20v20h-20z M7 7l10 10 M7 17l10-10',
        onclick: () => {
          clearSelected()
          clearBrush()
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
  // ;(echartsOption.series! as SeriesOption[]).push({
  //   type: 'scatter',
  //   name: 'Qualifiers',

  //   // XAxis data must be the same or the on mouse vertical lines won't sync up
  //   // TODO: too costly operation
  //   data: dsOption.source['value'].map((_val: string, index: number) => {
  //     return [
  //       // datetime
  //       new Date(dsOption?.source?.date[index]).getTime(),
  //       // qualifier value
  //       // selectedQualifier.value === 'All' ||
  //       // dsOption.source?.qualifier[index]?.has(selectedQualifier.value)
  //       //   ? 1
  //       //   : NaN,
  //       // qualifier codes
  //       // dsOption.source?.qualifier[index],
  //     ]
  //   }),

  //   xAxisIndex: 1,
  //   yAxisIndex: yAxisIndex,
  //   symbolSize: 7,
  //   itemStyle: {
  //     color: '#F44336',
  //   },
  // })

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
  const { selectedSeriesIndex } = storeToRefs(useEChartsStore())
  const yAxisConfigurations = createYAxisConfigurations(seriesArray)
  const leftYAxesCount = Math.ceil(yAxisConfigurations.size / 2)
  const rightYAxesCount = yAxisConfigurations.size - leftYAxesCount
  const gridRightPadding = 20 + rightYAxesCount * 85
  const gridLeftPadding = leftYAxesCount * 85

  let echartsOption: EChartsOption = {
    // https://echarts.apache.org/en/option.html#dataset.source
    dataset: seriesArray.map((s) => s.data?.dataset),
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
      toolbox: ['rect', 'keep', 'lineY', 'lineX', 'clear'],
      xAxisIndex: [0],
      seriesIndex: selectedSeriesIndex.value,
      throttleType: 'debounce',
      throttleDelay: 100,
    },
  }

  // Add result qualifier options if there's a datastream selected for quality control with qualifiers
  // if (
  //   selectedSeriesIndex.value !== -1
  //   // && hasStringQualifier(seriesArray[selectedSeriesIndex.value].data.dataFrame)
  // ) {
  //   echartsOption = addQualifierOptions(
  //     seriesArray[selectedSeriesIndex.value].data.dataset,
  //     echartsOption,
  //     yAxisConfigurations.size,
  //     gridRightPadding,
  //     gridLeftPadding
  //   )
  // }

  return echartsOption
}
