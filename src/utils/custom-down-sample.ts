import { DimensionIndex, SeriesOption } from 'echarts/types/src/util/types'
import {
  StageHandler,
  SeriesSamplingOptionMixin,
} from 'echarts/types/src/util/types'
import SeriesModel from 'echarts/types/src/model/Series'
import { range } from './interpolate'

/**
 * Large data down sampling
 * @param {string} valueDimension
 * @param params
 */
function customDownSample(valueDimension: DimensionIndex, params: any) {
  // TODO:  IN PROGRESS
  const xAxis = params[0]
  const yAxis = params[1]
  const api = params[2]

  const target = this.clone([valueDimension], true)
  const targetStorage = target._chunks
  const yStore = targetStorage[valueDimension]
  const xStore = targetStorage[0]

  const len = this.count()

  const xExtent = xAxis.getExtent()
  const rawXExtent = xAxis.scale.rawExtentInfo
  const xDomain = [
    rawXExtent._determinedMin || rawXExtent._dataMin,
    rawXExtent._determinedMax || rawXExtent._dataMax,
  ]

  const yExtent = yAxis.getExtent()
  console.log(this._rawExtent[1]) // TODO: this is the raw extent, unsure if updated after brush
  // TODO: find yAxis domain
  // const rawYExtent = yAxis.scale.rawExtentInfo
  // const yDomain = [
  //   rawYExtent._determinedMin || rawYExtent._dataMin,
  //   rawYExtent._determinedMax || rawYExtent._dataMax,
  // ]

  const dpr = api.getDevicePixelRatio()
  // In case coordinste system has been resized
  const size = Math.abs(xExtent[1] - xExtent[0]) * (dpr || 1)

  for (let i = 0; i < len; i++) {
    // 'i' is the index in the current visible data
    const rawIndex = this.getRawIndex(i) // The index in the full dataset

    if (rawIndex == 1000) {
      const dx = range(
        xDomain[0],
        xDomain[1],
        xExtent[0],
        xExtent[1],
        xStore[rawIndex]
      )
      console.log(dx)
      // const dy = range(
      //   yDomain[0],
      //   yDomain[1],
      //   yExtent[0],
      //   yExtent[1],
      //   yStore[rawIndex]
      // )
      // TODO: use extent and domain to get the current extent coordinates at x, y
      break
    }
  }

  // Example output
  target._count = 5 // Where to slice the _indices array
  target._indices = [1000, 2000, 3000, 4000, 5000] // The new indices

  target.getRawIndex = this._getRawIdx
  return target
}

export default function dataSample(seriesType: string): StageHandler {
  return {
    seriesType: seriesType,

    reset: function (
      seriesModel: SeriesModel<SeriesOption & SeriesSamplingOptionMixin>,
      _ecModel,
      api
    ) {
      const data = seriesModel.getData()
      const sampling = seriesModel.get('sampling')
      const coordSys = seriesModel.coordinateSystem
      const count = data.count()
      // Only cartesian2d support down sampling. Disable it when there is few data.
      if (count > 10 && coordSys.type === 'cartesian2d' && sampling) {
        // @ts-ignore
        const baseAxis = coordSys.getBaseAxis()
        // @ts-ignore
        const valueAxis = coordSys.getOtherAxis(baseAxis)

        // @ts-ignore
        if (sampling === 'custom-lttb') {
          // @ts-ignore: Hijack the internal lttbDownSample method and provide our own
          data._store.lttbDownSample = customDownSample
          seriesModel.setData(
            // @ts-ignore
            data.lttbDownSample(data.mapDimension(valueAxis.dim), [
              baseAxis,
              valueAxis,
              api,
            ])
          )
        }
      }
    },
  }
}
