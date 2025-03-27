import { usePlotlyStore } from '@/store/plotly'
import { GraphSeries } from '@/types'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

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
  const traces: any[] = seriesArray.map((s, index) => {
    return {
      x: s.data?.dataX,
      y: s.data?.dataY,
      xaxis: `x${index + 1}`,
      yaxis: `y${index + 1}`,
      type: 'scattergl',
      mode: 'lines+markers',
      // https://github.com/plotly/plotly.js/issues/5927
      hoverinfo: 'skip', // Fixes performance issues, but disables tooltips
      // hoverinfo: 'x+y',
      name: s.name,
      selected: {
        marker: {
          color: 'red',
        },
      },
    }
  })

  const xaxis: any = {}
  const yaxis: any = {}

  seriesArray.forEach((s, index) => {
    const xData = s.data?.dataX
    const maxDatetime = xData[xData.length - 1]
    const minDatetime = xData[0]

    xaxis[`xaxis${index > 0 ? index + 1 : ''}`] = {
      type: 'date',
      title: { text: 'Datetime' },
      rangeselector: selectorOptions,
      range: [minDatetime, maxDatetime],
      minallowed: minDatetime,
      maxallowed: maxDatetime,
      autorange: false,
      // range slider compatibility for Scattergl: https://github.com/plotly/plotly.js/issues/2627
    }

    yaxis[`yaxis${index > 0 ? index + 1 : ''}`] = {
      title: { text: s.yAxisLabel },
      // fixedrange: true,
      // autorange: true,
    }
  })

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
      ...xaxis,
      ...yaxis,
      dragmode: 'pan',
      hovermode: 'closest', // Disable if hovering is too costly
      uirevision: true,
      title: { text: seriesArray[0].name },
    },
    config: {
      displayModeBar: true,
      showlegend: true,
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

    selectedData.value = plotlyRef.value?.data[0].selectedpoints || null
  }
}

export const handleSelected = async (_eventData: any) => {
  console.log('handleSelected')
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  const { selectedData } = storeToRefs(useDataVisStore())
  selectedData.value = plotlyRef.value?.data[0].selectedpoints || null
}

export const handleDeselect = async () => {
  console.log('handleDeselect')
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  const { selectedData } = storeToRefs(useDataVisStore())
  selectedData.value = plotlyRef.value?.data[0].selectedpoints || null
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

// Binary search
export const findLowerBound = (target: number) => {
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  const xData = plotlyRef.value?.data[0].x
  let low = 0
  let high = xData.length
  while (low < high) {
    const mid = (low + high) >>> 1
    if (xData[mid] < target) {
      low = mid + 1
    } else high = mid
  }
  return low
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
  const { plotlyOptions, plotlyRef, isUpdating } = storeToRefs(usePlotlyStore())

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

    const yRange = plotlyRef.value?.layout.yaxis.range

    // Find visible points count
    // Plotly does not return the indexes of current axis range. We must find them using binary seach
    const startIdx = findLowerBound(xRange[0])
    const endIdx = findLowerBound(xRange[1])

    // auto scale y axis using data from the first trace
    const traceData = plotlyRef.value?.data[0]
    const yData = traceData.y as number[]

    // Find all y-values within the current x-axis range
    let yMin = Infinity
    let yMax = -Infinity

    // Could use Math.max and Math.min and spread operator, but this is more memory efficient
    for (let i = startIdx; i < endIdx; i++) {
      const val = yData[i]
      if (yMin > val && val > yRange[0]) {
        yMin = val
      }

      if (yMax < val && val < yRange[1]) {
        yMax = val
      }
    }

    // Calculate new y-axis range with padding
    if (endIdx - startIdx != 0 && yMax !== yMin) {
      const padding = (yMax - yMin) * 0.1 // 10% padding

      layoutUpdates.yaxis = {
        ...plotlyOptions.value.layout.yaxis,
        range: [yMin - padding, yMax + padding],
        autorange: false,
      }
    }

    // Update axis range
    await Plotly.update(plotlyRef.value, {}, layoutUpdates)
  } finally {
    isUpdating.value = false
  }
}
