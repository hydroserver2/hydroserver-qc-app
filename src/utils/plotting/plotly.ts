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
      title: 'Datetime',
      rangeselector: selectorOptions,
      range: [minDatetime, maxDatetime],
      minallowed: minDatetime,
      maxallowed: maxDatetime,
      autorange: false,
      // range slider compatibility for Scattergl: https://github.com/plotly/plotly.js/issues/2627
    }

    yaxis[`yaxis${index > 0 ? index + 1 : ''}`] = {
      title: s.yAxisLabel,
      fixedrange: true,
      // autorange: true,
    }
  })

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
      title: seriesArray[0].name,
    },
    config: {
      displayModeBar: true,
      showlegend: true,
      modeBarButtonsToRemove: ['toImage'],
      scrollZoom: true,
      responsive: true,
      doubleClick: false,
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
