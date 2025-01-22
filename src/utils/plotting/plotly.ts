import { usePlotlyStore } from '@/store/plotly'
import { GraphSeries } from '@/types'
// @ts-ignore no type definitions
import Plotly from 'plotly.js-dist'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'

export const createPlotlyOption = (seriesArray: GraphSeries[]) => {
  const plotlyOptions = {
    data: seriesArray.map((s) => ({
      ...s.data?.dataset.source,
      type: 'scattergl',
      mode: 'lines+markers',
      selected: {
        marker: {
          color: 'red',
        },
      },
    })),

    layout: {
      margin: { t: 40 },
      xaxis: {
        type: 'date',
        title: 'Date',
        autorange: true,
      },
      yaxis: {
        title: 'Value',
        fixedrange: true,
        autorange: true,
      },
      title: {
        text: 'Some title',
      },
      dragmode: 'select',
      // hovermode: false, // Disable if hovering is too costly
    },
    config: {
      displayModeBar: true,
      showlegend: true,
      modeBarButtonsToRemove: ['toImage'],
      scrollZoom: true,
      responsive: true,
    },
  }

  return plotlyOptions
}

export const handleClick = (eventData: any) => {
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  if ('selectedpoints' in eventData.points[0].fullData) {
    const point = eventData.points[0]
    point.data.selectedpoints = [point.pointIndex]

    // removes the area layer
    Plotly.restyle(plotlyRef.value, { selectedpoints: [null] })

    Plotly.update(
      plotlyRef.value,
      {
        selections: [], // Removes the selected areas
        selectedpoints: [point.pointIndex],
      },
      {},
      0
    )
  }
}

export const handleRelayout = (eventData: any) => {
  const { plotlyRef } = storeToRefs(usePlotlyStore())
  if ('selections' in eventData) {
    Plotly.update(
      plotlyRef.value,
      {},
      {
        selections: [],
      }
    )
  }
}

export const handleSelected = (eventData: any) => {
  const { selectedData } = storeToRefs(useDataVisStore())
  selectedData.value = eventData
}
