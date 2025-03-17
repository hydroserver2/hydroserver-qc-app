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
  const traces: any[] = seriesArray.map((s, index) => ({
    x: s.data?.dataset.source.x,
    y: s.data?.dataset.source.y,
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
  }))

  const xaxis: any = {}
  const yaxis: any = {}

  seriesArray.forEach((s, index) => {
    const xData = s.data?.dataset.source.x
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
          click: handleYaxisScale,
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

export const handleSelected = async (eventData: any) => {
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
const findLowerBound = (target: number) => {
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

const handleYaxisScale = async (eventData: any) => {
  const { plotlyOptions, plotlyRef, isUpdating } = storeToRefs(usePlotlyStore())

  // Plotly fires the relayout event for basically everything.
  // We only need to handle it when panning or zooming
  // if (
  //   isUpdating.value ||
  //   eventData?.dragmode || // Changing selected tool
  //   eventData?.selections || // Selecting points
  //   eventData?.['selections[0].x0'] || // Moving a selected area
  //   isEqual(eventData, {}) // Double click using pan tool
  // ) {
  //   return
  // }

  isUpdating.value = true

  setTimeout(async () => {
    console.log('handleYaxisScale')
    try {
      let yMin = 0
      let yMax = 0
      let xMin = 0
      let xMax = 0

      const layoutUpdates: any = {}

      const currentRange = plotlyRef.value?.layout.xaxis.range.map(
        (d: string) => {
          if (typeof d == 'string') {
            return Date.parse(d)
          }
          return d
        }
      )

      xMin = currentRange[0]
      xMax = currentRange[1]

      // Find visible points count using binary search
      // Plotly does not return the indexes. We must find them using binary seach
      const startIdx = findLowerBound(xMin)
      const endIdx = findLowerBound(xMax)

      // auto scale y axis using data from the first trace
      const traceData = plotlyRef.value?.data[0]
      const yData = traceData.y as number[]

      // Find all y-values within the current x-axis range
      yMin = yData[startIdx]
      yMax = yData[endIdx - 1]

      // Could use Math.max and Math.min and spread operator, but this is more memory efficient
      for (let i = startIdx; i < endIdx; i++) {
        if (yMin > yData[i]) {
          yMin = yData[i]
        }

        if (yMax < yData[i]) {
          yMax = yData[i]
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
  })
}
