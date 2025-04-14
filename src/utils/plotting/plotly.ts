import { usePlotlyStore } from '@/store/plotly'
import { GraphSeries } from '@/types'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { debounce, isEqual } from 'lodash-es'

// TODO: import these directly from Plotly
// https://github.com/plotly/plotly.js/blob/v2.14.0/src/components/color/attributes.js#L5-L16
export const COLORS = [
  '#1f77b4', // muted blue
  '#ff7f0e', // safety orange
  '#2ca02c', // cooked asparagus green
  '#d62728', // brick red
  '#9467bd', // muted purple
  '#8c564b', // chestnut brown
  '#e377c2', // raspberry yogurt pink
  '#7f7f7f', // middle gray
  '#bcbd22', // curry yellow-green
  '#17becf', // blue-teal
]

const selectorOptions = {
  buttons: [
    {
      step: 'month',
      stepmode: 'backward',
      count: 1,
      label: '1m',
    },
    {
      step: 'month',
      stepmode: 'backward',
      count: 6,
      label: '6m',
    },
    {
      step: 'year',
      stepmode: 'todate',
      count: 1,
      label: 'YTD',
    },
    {
      step: 'year',
      stepmode: 'backward',
      count: 1,
      label: '1y',
    },
    {
      step: 'all',
    },
  ],
}

export const createPlotlyOption = (seriesArray: GraphSeries[]) => {
  console.log('createPlotlyOption')
  const { qcDatastream } = storeToRefs(useDataVisStore())

  const traces: any = []
  const yaxis: any = {}

  let maxDatetime = -Infinity
  let minDatetime = Infinity
  const axisPlotFraction = 0.075 // Between 0 and 1

  let qcTrace: any
  let qcYaxis: any
  let counter = 0
  let axisSuffix = counter > 0 ? counter + 1 : ''

  seriesArray.forEach((s, index) => {
    const color = COLORS[index + 1] // The first color is reserved for the QC datastream
    const xData = s.data?.dataX

    maxDatetime = Math.max(xData[xData.length - 1], maxDatetime)
    minDatetime = Math.min(xData[0], minDatetime)

    const trace: any = {
      id: s.id,
      x: s.data?.dataX,
      y: s.data?.dataY,
      yaxis: `y${axisSuffix}`,
      type: 'scattergl',
      mode: 'lines+markers',
      // https://github.com/plotly/plotly.js/issues/5927
      hoverinfo: 'skip', // Fixes performance issues, but disables tooltips
      // hoverinfo: 'x+y',
      name: s.name,
      showLegend: false,
      selected: {
        marker: {
          color: 'red',
        },
      },
      marker: {
        color,
      },
      line: {
        color,
      },
    }

    if (s.id === qcDatastream.value?.id) {
      // The trace for the QC datastream needs to be added last so it's drawn on top.
      qcTrace = trace
      qcTrace.marker.color = COLORS[0]
      qcTrace.line.color = COLORS[0]
      qcYaxis = {
        title: { text: s.yAxisLabel, font: { color: COLORS[0] } },
        tickfont: { color: COLORS[0] },
        side: 'left',
        anchor: 'free',
        position: 0,
      }
    } else {
      traces.push(trace)

      const yAxis: any = {
        title: { text: s.yAxisLabel, font: { color } },
        tickfont: { color },
        // overlaying: 'y',
        side: 'right',
        anchor: 'free',
        position: 1 - axisPlotFraction * counter,
        // fixedrange: true,
        // autorange: true,
      }
      if (axisSuffix) {
        yAxis.overlaying = 'y'
      }

      yaxis[`yaxis${axisSuffix}`] = yAxis
      counter++
      axisSuffix = counter > 0 ? counter + 1 : ''
    }
  })

  qcTrace.yaxis = `y${axisSuffix}`
  traces.push(qcTrace)
  if (axisSuffix) {
    qcYaxis.overlaying = 'y'
  }
  yaxis[`yaxis${axisSuffix}`] = qcYaxis

  const xaxis: any = {
    type: 'date',
    title: { text: 'Datetime' },
    rangeselector: selectorOptions,
    range: [minDatetime, maxDatetime],
    minallowed: minDatetime,
    maxallowed: maxDatetime,
    autorange: false,
    // range slider compatibility for Scattergl: https://github.com/plotly/plotly.js/issues/2627
  }

  if (seriesArray.length > 2 || true) {
    xaxis.domain = [0, 1 - axisPlotFraction * (seriesArray.length - 2)]
  }

  const iconRescaleY = {
    width: 500,
    height: 600,
    path: 'M182.6 9.4c-12.5-12.5-32.8-12.5-45.3 0l-96 96c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L128 109.3l0 293.5L86.6 361.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l96 96c12.5 12.5 32.8 12.5 45.3 0l96-96c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L192 402.7l0-293.5 41.4 41.4c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-96-96z',
  }

  const newPlotlyOptions = {
    traces,
    layout: {
      spikedistance: 0, // https://github.com/plotly/plotly.js/issues/5927#issuecomment-1697679087
      // hoverdistance: 20,
      xaxis,
      ...yaxis,
      dragmode: 'pan',
      hovermode: 'closest', // Disable if hovering is too costly
      uirevision: true,
      title: { text: qcTrace?.name, font: { color: COLORS[0] } },
      showlegend: false,
    },
    config: {
      displayModeBar: true,
      showlegend: false,
      modeBarButtonsToRemove: ['toImage', 'autoScale'],
      scrollZoom: true,
      responsive: true,
      doubleClick: false,
      modeBarButtonsToAdd: [
        {
          name: 'Autoscale Y axis',
          icon: iconRescaleY,
          direction: 'up',
          click: cropYaxisRange,
        },
      ],
      // plotGlPixelRatio: 1,
    },
  }

  return newPlotlyOptions
}

export const handleClick = async (eventData: any) => {
  console.log('handleClick')
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  const { selectedData } = storeToRefs(useDataVisStore())

  const point = eventData.points[0]
  if (point) {
    let alreadySelected = []

    if (point.data.selectedpoints) {
      alreadySelected = Array.isArray(point.data.selectedpoints)
        ? point.data.selectedpoints
        : [point.data.selectedpoints]
    }

    const index = alreadySelected.indexOf(point.pointIndex)
    // Toggle the point
    index >= 0
      ? alreadySelected.splice(index, 1)
      : alreadySelected.push(point.pointIndex)

    alreadySelected.sort()

    // Removes selected areas
    await Plotly.update(plotlyRef.value, {}, { selections: [] }, [0])

    // Colors selected points
    await Plotly.restyle(plotlyRef.value, {
      selectedpoints: [[...alreadySelected]],
    })

    handleSelected()
  }
}

export const handleSelected = async (eventData?: any) => {
  console.log('handleSelected')
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  const { selectedData } = storeToRefs(useDataVisStore())
  const { qcDatastream } = storeToRefs(useDataVisStore())

  const traceIndex = plotlyRef.value?.data.findIndex(
    (trace: any) => trace.id == qcDatastream.value?.id
  )

  selectedData.value = plotlyRef.value?.data[traceIndex].selectedpoints || null

  // TODO: prevent selection on other traces
}

export const handleNewPlot = async (element?: any) => {
  const { plotlyOptions, plotlyRef } = storeToRefs(usePlotlyStore())

  plotlyRef.value = await Plotly.newPlot(
    element || plotlyRef.value,
    plotlyOptions.value.traces,
    plotlyOptions.value.layout,
    plotlyOptions.value.config
  )

  handleRelayout(null)
  plotlyRef.value?.on('plotly_redraw', debounce(handleRelayout, 150))
  plotlyRef.value?.on('plotly_relayout', debounce(handleRelayout, 150))
  plotlyRef.value?.on('plotly_selected', debounce(handleSelected, 150))
  plotlyRef.value?.on('plotly_deselec', debounce(handleDeselect, 150))
  plotlyRef.value?.on('plotly_click', handleClick)
  plotlyRef.value?.on('plotly_doubleclick', handleDoubleClick)
}

export const handleRelayout = async (eventData: any) => {
  const {
    plotlyOptions,
    plotlyRef,
    isUpdating,
    areTooltipsEnabled,
    visiblePoints,
    tooltipsMaxDataPoints,
  } = storeToRefs(usePlotlyStore())

  handleSelected()

  // Plotly fires the relayout event for practically everything.
  // We only need to handle it when panning or zooming.
  if (
    isUpdating.value ||
    eventData?.dragmode || // Changing selected tool
    eventData?.selections || // Selecting points
    eventData?.['selections[0].x0'] || // Moving a selected area
    isEqual(eventData, {}) // Double click using pan tool
  ) {
    return
  }

  isUpdating.value = true

  setTimeout(async () => {
    console.log('handleRelayout')
    try {
      const layoutUpdates = { ...plotlyOptions.value.layout }

      // Plotly will rewrite timestamps as datestrings. We need to convert them back to timestamps.
      if (typeof layoutUpdates.xaxis.range[0] == 'string') {
        layoutUpdates.xaxis.range[0] = Date.parse(layoutUpdates.xaxis.range[0])
        layoutUpdates.xaxis.range[1] = Date.parse(layoutUpdates.xaxis.range[1])
      }

      // Find visible points count
      // Plotly does not return the indexes. We must find them using binary seach
      const startIdx = findFirstGreaterOrEqual(
        plotlyRef.value?.data[0].x,
        layoutUpdates.xaxis.range[0]
      )
      const endIdx = findFirstGreaterOrEqual(
        plotlyRef.value?.data[0].x,
        layoutUpdates.xaxis.range[1]
      )

      visiblePoints.value = endIdx - startIdx

      // Threshold check
      const newHoverState =
        visiblePoints.value > tooltipsMaxDataPoints.value ? 'skip' : 'x+y'

      // Only update if state changed
      if (plotlyRef.value?.data[0].hoverinfo !== newHoverState) {
        if (newHoverState === 'x+y' && !areTooltipsEnabled.value) {
          return
        }

        await Plotly.restyle(plotlyRef.value, { hoverinfo: [newHoverState] }, 0)
      }

      // await Plotly.update(plotlyRef.value, {}, layoutUpdates)
    } finally {
      isUpdating.value = false
    }
  })
}

export const handleDeselect = async (_eventData: any) => {
  console.log('handleDeselect')
  handleSelected()
}

export const handleDoubleClick = async () => {
  console.log('handleDoubleClick')
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

  const { selectedData } = storeToRefs(useDataVisStore())
  selectedData.value = []
}

export const findFirstGreaterOrEqual = (
  array: number[] | Float64Array<SharedArrayBuffer>,
  target: number
) => {
  let low = 0,
    high = array.length
  while (low < high) {
    const mid = (low + high) >> 1
    if (array[mid] < target) low = mid + 1
    else high = mid
  }
  return low
}

export const findLastLessOrEqual = (
  array: number[] | Float64Array<SharedArrayBuffer>,
  target: number
) => {
  let low = 0,
    high = array.length
  while (low < high) {
    const mid = (low + high) >> 1
    if (array[mid] > target) high = mid
    else low = mid + 1
  }
  return low - 1
}

export const cropXaxisRange = async () => {
  const { plotlyOptions, plotlyRef, isUpdating } = storeToRefs(usePlotlyStore())

  console.log('cropXaxisRange')

  isUpdating.value = true

  setTimeout(async () => {
    try {
      const layoutUpdates = { ...plotlyOptions.value.layout }
      // Plotly will rewrite timestamps as datestrings. We need to convert them back to timestamps.
      if (typeof layoutUpdates.xaxis.range[0] == 'string') {
        layoutUpdates.xaxis.range[0] = Date.parse(layoutUpdates.xaxis.range[0])
        layoutUpdates.xaxis.range[1] = Date.parse(layoutUpdates.xaxis.range[1])
      }

      const currentRange = plotlyRef.value?.layout.xaxis.range.map(
        (d: string | number) => (typeof d == 'string' ? Date.parse(d) : d)
      )

      layoutUpdates.xaxis.range = [
        Math.max(currentRange[0], layoutUpdates.xaxis.range[0]),
        Math.min(currentRange[1], layoutUpdates.xaxis.range[1]),
      ]

      await Plotly.update(plotlyRef.value, {}, layoutUpdates)
    } finally {
      isUpdating.value = false
    }
  })
}

/**
 * Crops the y axis to only contain the extent of currently visible points
 * @param _eventData
 */
export const cropYaxisRange = async (_eventData: any) => {
  const { plotlyOptions, plotlyRef, isUpdating, graphSeriesArray } =
    storeToRefs(usePlotlyStore())

  isUpdating.value = true
  console.log('cropYaxisRange')

  try {
    const layoutUpdates: any = {}

    const xRange = plotlyRef.value?.layout.xaxis.range.map((d: string) => {
      if (typeof d == 'string') {
        return Date.parse(d)
      }
      return d
    })

    // Find visible points count
    // Plotly does not return the indexes of current axis range. We must find them using binary seach
    const startIdx = findFirstGreaterOrEqual(
      plotlyRef.value?.data[0].x,
      xRange[0]
    )
    const endIdx = findFirstGreaterOrEqual(
      plotlyRef.value?.data[0].x,
      xRange[1]
    )

    for (let i = 0; i < graphSeriesArray.value.length; i++) {
      const axisKey = i == 0 ? 'yaxis' : `yaxis${i + 1}`
      const yAxis = plotlyOptions.value.layout[axisKey]

      const traceData = plotlyRef.value?.data[i]
      const yData = traceData.y as number[]

      // Find all y-values within the current x-axis range
      let yMin = Infinity
      let yMax = -Infinity

      // Could use Math.max and Math.min and spread operator, but this is more memory efficient
      for (let i = startIdx; i < endIdx; i++) {
        const val = yData[i]
        if (yMin > val && val > yAxis.range[0]) {
          yMin = val
        }

        if (yMax < val && val < yAxis.range[1]) {
          yMax = val
        }
      }

      // Calculate new y-axis range with padding
      if (endIdx - startIdx != 0 && yMax !== yMin) {
        const padding = (yMax - yMin) * 0.1 // 10% padding

        layoutUpdates[axisKey] = {
          ...yAxis,
          range: [yMin - padding, yMax + padding],
          autorange: false,
        }
      }
    }

    // Update axis range
    await Plotly.update(plotlyRef.value, {}, layoutUpdates)
  } finally {
    isUpdating.value = false
  }
}
