import {
  EChartsOption,
  YAXisComponentOption,
  SeriesOption,
  LegendComponentOption,
  TooltipComponentOption,
} from 'echarts'
import { GraphSeries } from '@/types'
import { storeToRefs } from 'pinia'
import { useEChartsStore } from '@/store/echarts'

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
      yAxisConfigurations.set(series.yAxisLabel, {
        index: yAxisConfigurations.size,
        yAxisLabel: series.yAxisLabel,
        color: series.lineColor,
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

export function generateSeriesOptions(
  seriesArray: GraphSeries[],
  yAxisConfigurations: yAxisConfigurationMap
): SeriesOption[] {
  return seriesArray.map((series) => ({
    name: series.name,
    type: 'line',
    data: series.data.map((dp) => [dp.date.getTime(), dp.value]),
    xAxisIndex: 0,
    yAxisIndex: yAxisConfigurations.get(series.yAxisLabel)?.index,
    itemStyle: {
      color: series.lineColor,
    },
    lineStyle: {
      width: 1,
    },
    sampling: 'lttb',
    showSymbol: false,
  }))
}

export function generateToolboxOptions() {
  return {
    feature: {
      dataZoom: {
        yAxisIndex: false,
      },
      restore: {},
      saveAsImage: { name: 'plot_export' },
      brush: {
        type: ['rect', 'polygon', 'lineX', 'lineY', 'keep', 'clear'],
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

interface CustomOptions {
  addToolbox: boolean
  initializeZoomed: boolean
}

export const createEChartsOption = (
  seriesArray: GraphSeries[],
  opts: Partial<CustomOptions> = {}
): EChartsOption => {
  const { addToolbox = true, initializeZoomed = true } = opts

  const yAxisConfigurations = createYAxisConfigurations(seriesArray)
  const yAxisOptions = generateYAxisOptions(yAxisConfigurations)

  // Push qualifying comments axis
  yAxisOptions.push({
    show: false,
    gridIndex: 1,
    // splitNumber:.001,
    max: '1000000000000000',
    min: '-1000000000000000',
  })

  const seriesOptions = generateSeriesOptions(seriesArray, yAxisConfigurations)

  // TODO: Only add this for the selected dataset if there is one
  // TODO: Only add points where there's a qualifying comment
  seriesOptions.push({
    type: 'scatter',
    data: seriesArray[0].data.map((dp) => [dp.date.getTime(), dp.value]), // XAxis data must be the same or the on mouse vertical lines won't sync up
    xAxisIndex: 1,
    yAxisIndex: yAxisConfigurations.size,
    symbolSize: 5,
    itemStyle: {
      color: '#F44336',
    },
  })

  const leftYAxesCount = Math.ceil(yAxisConfigurations.size / 2)
  const rightYAxesCount = yAxisConfigurations.size - leftYAxesCount
  let gridRightPadding = 20 + rightYAxesCount * 85
  let gridLeftPadding = leftYAxesCount * 85

  let echartsOption: EChartsOption = {
    grid: [
      {
        bottom: '160',
        right: gridRightPadding,
        left: gridLeftPadding,
      },
      {
        bottom: '90',
        right: gridRightPadding,
        left: gridLeftPadding,
        height: '5%',
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
      {
        type: 'time',
        show: false,
        gridIndex: 1,
      },
    ],
    yAxis: yAxisOptions,
    series: seriesOptions,
    dataZoom: generateDataZoomOptions(initializeZoomed),
    axisPointer: {
      link: [
        {
          xAxisIndex: 'all',
        },
      ],
      label: {
        backgroundColor: '#777',
      },
    },
  }

  if (addToolbox) {
    echartsOption.toolbox = generateToolboxOptions() as {}
  }

  echartsOption.legend = createLegendConfig()

  return echartsOption
}
