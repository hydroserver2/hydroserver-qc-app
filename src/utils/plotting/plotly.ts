import { GraphSeries } from '@/types'

export const createPlotlyOption = (seriesArray: GraphSeries[]) => {
  console.log('createPlotlyOption')

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
      },
      yaxis: {
        title: 'Value',
        fixedrange: true,
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
